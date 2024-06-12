# data manipulation and analysis functions
source("R/compute_grouped_exposure_data.R")
source("R/compute_national_analytics_summaries.R")
source("R/compute_regional_analytics_summaries.R")
source("R/compute_Rt_decomposition.R")
source("R/estimate_rho.R")

# plotting aesthetic functions
f1 <- list(
  family = "Arial, sans-serif",
  size = 32,
  color = "black"
)

f2 <- list(
  family = "Arial, sans-serif",
  size = 22,
  color = "black"
)

f3 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "black"
)

f4 <- list(
  family = "Arial, sans-serif",
  size = 28,
  color = "black"
)

week.days <- format(ISOdate(1, 1, 1:7), "%a")
weekday.cols = hue_pal()(7)
diff_days <- function(x,y){
  round(as.numeric(difftime(as.Date(x),as.Date(y),units='d')))
}

# plotting functions
source("R/get_coloured_by_risk_plots.R")
source("R/get_England_Wales_Euros_plot.R")
source("R/get_England_Wales_Euros_infections_plot.R")
source("R/get_infections_plot.R")
source("R/get_infections_by_setting_plot.R")
source("R/get_map_plots.R")
source("R/get_mobility_plots.R")
source("R/get_national_plot.R")
source("R/get_R_estimates_plots.R")
source("R/get_region_and_setting_summary_plots.R")
source("R/get_regional_plots_all_regions.R")
source("R/get_stacked_fraction_of_contacts_by_setting_plot.R")
source("R/get_stacked_fraction_of_transmissions_by_setting_plot.R")
source("R/get_transmissions_by_setting_plot.R")
source("R/get_transmissions_plot.R")
source("R/get_uptake_plot.R")
