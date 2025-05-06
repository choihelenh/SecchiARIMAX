
setwd(here::here())
# load required packages
source('forecast_code/load_packages.R')
# source(here::here("forecast_code", "R", "load_packages.R"))


# load the forecast generation function - include at least a forecast_date argument
source('forecast_code/R/generate_secchi_arimax.R')

# ---- Generate the forecasts -----
# default is to run a real-time forecast for today for fcre only
forecast_date <- Sys.Date() - 1
site_list <- read_csv("https://raw.githubusercontent.com/LTREB-reservoirs/vera4cast/main/vera4cast_field_site_metadata.csv",
                      show_col_types = FALSE)

fcre_lat <- site_list |>
  filter(site_id == 'fcre') |>
  pull(latitude)

fcre_long <- site_list |>
  filter(site_id == 'fcre') |>
  pull(longitude)

model_id <- 'secchi_arimax'

res <- generate_secchi_arimax(forecast_date = forecast_date,
                              model_id = model_id,
                              site = 'fcre',
                              horizon = 30,
                              project_id = "vera4cast")
print(res$plot)        # shows the figure
forecast <- res$forecast  # long‑format df, same as before


# # this should generate a df
# forecast <- generate_secchi_arimax(forecast_date = forecast_date,
#                                       model_id = model_id,
#                                       site = 'fcre',
#                                       horizon = 30,
#                                       project_id = "vera4cast")
#----------------------------------------#

# write forecast locally
message('Writing forecast')
save_here <- 'Forecasts/'
forecast_file <- paste0(save_here, forecast_date, '-', model_id, '.csv')

if (dir.exists(save_here)) {
  write_csv(forecast, forecast_file)
} else {
  dir.create(save_here)
  write_csv(forecast, forecast_file)
}

vera4castHelpers::forecast_output_validator(forecast_file)




# Submit forecast!
vera4castHelpers::submit(forecast_file = forecast_file)
