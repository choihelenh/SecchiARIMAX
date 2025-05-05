library(dplyr)
library(zoo)

interpolate_secchi <- function(df) {
  df %>%
    arrange(datetime) %>%
    mutate(
      Secchi_m_smoothed = na.approx(Secchi_m, na.rm = FALSE, rule = 2)
    ) %>%
    relocate(Secchi_m_smoothed, .after = datetime)
}

# fcre_smoothed <- interpolate_secchi(fcre_Combined)
# bvre_smoothed <- interpolate_secchi(bvre_Combined)
