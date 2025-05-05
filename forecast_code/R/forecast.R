# Load libraries
library(tidyverse)
library(forecast)
library(Metrics)
library(scoringRules)
library(zoo)



# === 3. Forecast setup ===
n <- nrow(smoothed_data)
trainN <- n - 30
testN <- trainN + 1

train <- smoothed_data[1:trainN, ]
test  <- smoothed_data[testN:n, ]

ts.train <- ts(train$Secchi_m, frequency = 7)
xreg_train <- as.matrix(train %>% select(Rain_lag23, AirTemp_lag0))
xreg_test  <- as.matrix(test %>% select(Rain_lag23, AirTemp_lag0))

# === 4. Fit and forecast ===
fit <- stlm(ts.train, s.window = "periodic", method = "arima", xreg = xreg_train)
fc <- forecast(fit, h = 30, newxreg = xreg_test)

z_80 <- qnorm(0.9)
sd_est <- (fc$upper[, 1] - fc$lower[, 1]) / (2 * z_80)

# === 5. Forecast dataframe with dates ===
forecast_df <- tibble(
  datetime = test$datetime,
  forecast = as.numeric(fc$mean),
  lower = as.numeric(fc$lower[, 1]),
  upper = as.numeric(fc$upper[, 1])
)

# === 6. Join raw Secchi observations for comparison (no filtering) ===
raw_obs <- fcre_Combined %>%
  select(datetime, Secchi_m) %>%
  rename(observed_secchi = Secchi_m)

comparison_df <- forecast_df %>%
  left_join(raw_obs, by = "datetime")  # Keep full forecast period

# === 7. Plot forecast vs raw ===
ggplot(comparison_df, aes(x = datetime)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#a6d854", alpha = 0.3) +
  geom_line(aes(y = forecast), color = "#33a02c", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = forecast), color = "#33a02c", size = 2) +
  geom_line(aes(y = observed_secchi), color = "black", linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = observed_secchi), color = "black", size = 2, na.rm = TRUE) +
  labs(
    title = "30-Day Forecast vs Raw Secchi (fcre)",
    subtitle = "Model: Rain lag 23 + AirTemp unlagged",
    x = "Date", y = "Secchi Depth (m)"
  ) +
  theme_minimal(base_size = 14)

# === 8. Performance metrics (only on available obs) ===
scored_df <- comparison_df %>%
  filter(!is.na(observed_secchi))

metrics <- tibble(
  Model = "STL + ARIMA (Rain_lag23 + AirTemp)",
  RMSE = rmse(scored_df$observed_secchi, scored_df$forecast),
  MAE = mae(scored_df$observed_secchi, scored_df$forecast),
  CRPS = mean(crps_norm(scored_df$observed_secchi, scored_df$forecast, sd_est[1:nrow(scored_df)]), na.rm = TRUE)
)

print(metrics)

# === Filter to 30-day forecast window only ===
start_date <- min(forecast_df$datetime)
end_date <- max(forecast_df$datetime)

plot_df <- plot_df %>%
  mutate(datetime = as.Date(datetime)) %>%  # <- this line is the fix
  filter(datetime >= start_date & datetime <= end_date)


