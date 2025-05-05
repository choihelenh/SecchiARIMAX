# forecast_code/R/generate_secchi_stl.R
generate_secchi_arimax <- function(forecast_date,
                                   model_id  = "secchi_arimax",
                                   site      = "fcre",
                                   horizon   = 30,
                                   project_id = "vera4cast") {

  # --- 1. Pull & prepare historical ----------------------------------
  source(here::here("forecast_code", "R", "get_secchi_airtemp_precip_data.R"))
  source(here::here("forecast_code", "R", "interpolate_secchi.R"))

  fcre_combined <- get_secchi_airtemp_precip_data(site) |>
    dplyr::filter(datetime < forecast_date)

  fcre_smoothed <- interpolate_secchi(fcre_combined)

  # 4. ----- Build regressors -------------------------------------------
  ## (your lag‑23 rain + noon means code here, but using `historical_weather`
  ##  and `future_weather` variables)
  # print(fcre_smoothed)
  smoothed_data <- fcre_smoothed %>%
    select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean, Secchi_m_smoothed) %>%
    mutate(
      Rain_lag23 = lag(Rain_mm_sum, 23),
      AirTemp_lag0 = AirTemp_C_mean
    ) %>%
    filter(!is.na(Secchi_m_smoothed), !is.na(Rain_lag23), !is.na(AirTemp_lag0))
  # print(smoothed_data)

 # get future data
  # start_forecast_date <- format(Sys.Date(), "%Y-%m-%d")
  forecast_date <- format(forecast_date)
  weather <- vera4castHelpers::noaa_stage2(start_date = forecast_date)
  df_future <- weather %>% filter(site_id == "fcre") %>% collect()

  # ---- Step 4: Aggregate noon forecasts ----
  agg_noon <- function(.data, value_col, fun = mean) {
    .data %>%
      filter(hour(datetime) == 12, minute(datetime) == 0) %>%
      mutate(date = as.Date(datetime)) %>%
      group_by(date) %>%
      summarise(value = fun({{ value_col }}, na.rm = TRUE), .groups = "drop")
  }

  airtemp_noon <- df_future %>%
    filter(variable == "air_temperature") %>%
    mutate(pred_C = prediction - 273.15) %>%
    agg_noon(pred_C) %>%
    rename(AirTemp_C_mean = value)


  precip_noon <- df_future %>%
    filter(variable == "precipitation_flux",
           hour(datetime) == 12, minute(datetime) == 0) %>%
    mutate(date = as.Date(datetime),
           flux = prediction,
           precip_mm = flux * 86400) %>%
    group_by(date) %>%
    slice(1) %>%
    ungroup() %>%
    select(date, precip_mm) %>%
    rename(Rain_mm_sum = precip_mm)

  noon_future_regs <- airtemp_noon %>%
    left_join(precip_noon, by = "date") %>%
    rename(datetime = date)

  all_meteo <- fcre_smoothed %>%
    select(datetime, Rain_mm_sum, AirTemp_C_mean) %>%
    bind_rows(noon_future_regs) %>%
    arrange(datetime) %>%
    mutate(Rain_lag23 = lag(Rain_mm_sum, 23))

  future_predictors <- all_meteo %>%
    mutate(date = as.Date(datetime)) %>%
    filter(date %in% as.Date(noon_future_regs$datetime)) %>%
    filter(!is.na(Rain_lag23), !is.na(AirTemp_C_mean)) %>%
    select(-date)  # Optional: drop helper column


  secchi_data <- fcre_smoothed %>%
    select(datetime, Secchi_m, Rain_mm_sum, AirTemp_C_mean, Secchi_m_smoothed) %>%
    mutate(Rain_lag23 = lag(Rain_mm_sum, 23)) %>%
    filter(!is.na(Secchi_m_smoothed), !is.na(Rain_lag23), !is.na(AirTemp_C_mean))
  # print(secchi_data)
  # print(tail(secchi_data))
  # na_counts_base <- colSums(is.na(secchi_data))
  # print(na_counts_base)

  # Training/testing split (last 30 days for testing)
  n <- nrow(secchi_data)
  trainN <- n - 30
  # train <- secchi_data[1:trainN, ]
  train <- secchi_data

  ts.train <- ts(train$Secchi_m_smoothed, frequency = 365)
  xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_C_mean))

  # ---- Step 7: Forecast future Secchi ----
  future_xreg <- as.matrix(future_predictors %>% select(Rain_lag23, AirTemp_C_mean))

  fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
  fc <- forecast(fit, h = nrow(future_predictors), newxreg = future_xreg)
  # print(fc)
  z_80 <- qnorm(0.9)
  forecast_df <- tibble(
    date = future_predictors$datetime,
    forecast = as.numeric(fc$mean),
    lower = as.numeric(fc$lower[, 1]),
    upper = as.numeric(fc$upper[, 1])
  )

  # ---- Step 8: Plot the forecast ----
  p <- ggplot(forecast_df, aes(x = date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6bddb", alpha = 0.4) +
    geom_line(aes(y = forecast), color = "#0570b0", linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = forecast), color = "#0570b0", linewidth = 2) +
    labs(
      title = "30-Day Forecast of Secchi Depth",
      subtitle = "Model: Rain (lag 23) + Air Temp",
      x = "Date", y = "Secchi Depth (m)"
    ) +
    theme_minimal(base_size = 14)

  sd_est <- (forecast_df$upper - forecast_df$lower) / (2 * z_80)
  forecast_comparison <- forecast_df %>%
    transmute(datetime = date,          # keep the time stamp
              forecast = forecast,      # point forecast  (μ)
              sd = sd_est)              # std‑dev estimate (σ)

  # 6. ----- Build long‑format output ------------------------------------
  #Set metadata for VERA4cast
  project_id <- "vera4cast"
  model_id <- "secchi_arimax"
  reference_datetime <- forecast_date
  duration <- "P1D"
  site_id <- "fcre"
  depth_m <- NA
  family <- "normal"
  variable <- "secchi"

  #Build our long-format forecast for export
  vera4cast_df <- forecast_comparison %>%          # <‑‑ now defined
    mutate(datetime = format(as.POSIXct(datetime),
                             "%Y-%m-%d 00:00:00", tz = "UTC")) %>%
    pivot_longer(cols = c(forecast, sd),
                 names_to = "parameter",
                 values_to = "prediction") %>%
    mutate(parameter = recode(parameter,
                              "forecast" = "mu",
                              "sd"       = "sigma"),
           project_id = project_id,
           model_id   = model_id,
           reference_datetime = format(Sys.time(), "%Y-%m-%d 00:00:00", tz = "UTC"),
           duration   = duration,
           site_id    = site_id,
           depth_m    = depth_m,
           family     = family,
           variable   = variable) %>%
    select(project_id, model_id, datetime, reference_datetime,
           duration, site_id, depth_m, family, parameter, variable, prediction)

  # return(vera4cast_df)
  return(list(forecast = vera4cast_df, plot = p))

}
