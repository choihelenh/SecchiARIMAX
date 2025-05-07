library(dplyr)
library(zoo)

# Define a function to interpolate (fill in) missing Secchi depth values
interpolate_secchi <- function(df) {
  df %>%
    arrange(datetime) %>% # Order the data by time
    mutate(
      # Create a new smoothed column 'Secchi_m_smoothed' where missing values are linearly interpolated (na.approx)
      # rule = 2 allows extrapolation at both ends
      Secchi_m_smoothed = na.approx(Secchi_m, na.rm = FALSE, rule = 2)
    ) %>%
    # Move this new 'Secchi_m_smoothed' column to appear right after the 'datetime' column
    relocate(Secchi_m_smoothed, .after = datetime)
}

# fcre_smoothed <- interpolate_secchi(fcre_Combined)
# bvre_smoothed <- interpolate_secchi(bvre_Combined)
