get_secchi_airtemp_precip_data <- function(site = "fcre") {

  # URLs for CSV files containing daily data for Secchi depth, inflow, and meteorology
  url_insitu <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
  url_inflow <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-inflow-targets.csv.gz"
  url_met <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-met-targets.csv.gz"

  # Read insitu, inflow, and meteorological data into dataframes
  insitu <- read_csv(url_insitu, show_col_types = FALSE)
  inflow <- read_csv(url_inflow, show_col_types = FALSE)
  met <- read_csv(url_met, show_col_types = FALSE)

  # Combine insitu and inflow, filtering for selected site
  # Only retain rows for the specified site ID (such as "fcre" or 'bvre")
  targets <- bind_rows(insitu, inflow) %>%
    filter(site_id == !!site)

  # Extract and clean Secchi depth observations
  # Keep only Secchi depth variable, rename, and keep the relevant columns
  secchi <- targets %>%
    filter(variable == "Secchi_m_sample") %>%
    rename(Secchi_m = observation) %>%
    select(datetime, Secchi_m)

  # Meteorology — use full datetime set (not site-specific)

  # Extract air temperature and rename the column
  airtemp <- met %>%
    filter(variable == "AirTemp_C_mean") %>%
    rename(AirTemp_C_mean = observation) %>%
    select(datetime, AirTemp_C_mean)

  # Extract daily rainfall and rename the column
  precip <- met %>%
    filter(variable == "Rain_mm_sum") %>%
    rename(Rain_mm_sum = observation) %>%
    select(datetime, Rain_mm_sum)

  # Combine meteorology with Secchi depth
  # Join meteorology first (covers all days), then left join Secchi
  # Full join ensures we retain all dates for met data
  # Left join Secchi data since it may be missing on some days
  combined <- airtemp %>%
    full_join(precip, by = "datetime") %>%
    left_join(secchi, by = "datetime") %>%
    arrange(datetime) %>%
    mutate(site_id = site) %>%
    relocate(site_id)

  # Return combined dataframe
  return(combined)
}

# Usage for FCRE and BCRE sites
fcre_Combined <- get_secchi_airtemp_precip_data("fcre")
bvre_Combined <- get_secchi_airtemp_precip_data("bvre")
