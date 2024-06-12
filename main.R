# See README for an overview of this project.
# This file runs the analysis, saves the results and creates the plots.

# Choose whether to save plots as png or pdf (or neither). Defaults to both.
save.plots.as.png <- TRUE
save.plots.as.pdf <- TRUE

# using dummy or original data - needed within the functions "compute_national_analytics_summaries" and "compute_regional_analytics_summaries" because the dpois function breaks when using the dummy (mean) values throughout
dummy.data <- TRUE

#############################
# LOAD PACKAGES & FUNCTIONS
#############################

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        suppressWarnings(lapply(need,require,character.only=TRUE))
    }
}

using("dplyr", "stringr", "reticulate", "readr", "tibble", "tidyr", "plotly", "viridis", "glue", "here", "data.table", "Matrix", "scales", "leaflet", "leaflet.extras", "sf", "mapview")
reticulate::py_run_string("import sys") # to ensure we can use plotly::save_image() later

setwd(glue("{here()}")) # should set working directory to the folder where this "main.R" file lives. If not, do so manually.

# source the necessary functions
source("R/functions.R")


## Manual date setting

# dates to start and end main-text plots of exposure data
exposure.data.start.date <- as.Date("2021-04-01")
exposure.data.end.date <- as.Date("2022-02-21")

# dates to start and end main-text plots of analytics data
analytics.data.start.date <- as.Date("2021-02-01")
analytics.data.end.date <- as.Date("2023-03-15")


#####################
### Main data - load
#####################
  
# load exposure data summary
table_outcome_riskScores <- read_csv(file="data/private/table_riskScores.csv", show_col_types=FALSE) %>% 
  filter(maxRiskScore > 100)

# load analytics data summary
cleaned.app.data <- read_csv(glue("data/private/clean_timeseries_24Sep20_9May23.csv"), show_col_types = FALSE)


####################################
### Main data - process and analyse
####################################å

# compute / gather national C(t) and TPAEN from analytics data
national.data.full <- compute_national_analytics_summaries(cleaned.app.data = cleaned.app.data, dummy.data = dummy.data)

# compute / gather regional C(t) and TPAEN from analytics data
regional.data.full <- compute_regional_analytics_summaries(cleaned.app.data = cleaned.app.data, dummy.data = dummy.data)

# wrangle exposure data table to get classification by setting, group by date & ltla, 
# weight by the ratio of daily analytics notifications/grouped event analytics notifications for a given date of notification and LTLA
# and add "date_plotted_exposure" field
grouped_exposure_data <- compute_grouped_exposure_data(table_outcome_riskScores = table_outcome_riskScores,
                                                       ltla.data = cleaned.app.data,
                                                       dummy.data = dummy.data)

# estimate the rho coefficient
if (dummy.data) {
  rho <- 0.47 # approx value
  } else {
  rho <- estimate_rho(table_outcome_riskScores = table_outcome_riskScores)
}

##################
# Preparing plots. Saving statistical results into the 'results' folder also happens within some of these scripts.
##################

### FIGURE 1

# prepare national timeseries plots of Ct, TPAEN and Rt
national.plot <- get_national_plot(national.data.full = national.data.full, 
                                   first.date.to.plot = analytics.data.start.date, 
                                   last.date.to.plot = analytics.data.end.date)


# combine plots into one subplot
figure_national_timeseries <- subplot(national.plot$p1, 
                                      national.plot$p2, 
                                      national.plot$p3, 
                                      national.plot$p4,
                                      nrows=4, shareX = TRUE, titleY = TRUE, margin = 0.05) %>%
  layout(legend = list(y=0.1))



### FIGURE 2

# prepare plot of four different R estimators / indicators
R_estimates_plot <- get_R_estimates_plot(national.data.full = national.data.full)


### FIGURE 3

# regional summary maps of Ct, TPAEN and Rt
regional.maps <- get_regional_map_plots()

# regional timeseries of Ct, TPAEN and Rt, London highlighted
regional.plots <- ten_regions_plot(regional.data.full = regional.data.full, 
                                   first.date.to.plot = analytics.data.start.date, 
                                   last.date.to.plot = analytics.data.end.date)

figure_regional_timeseries <- subplot(regional.plots$p1, 
                                      regional.plots$p2, 
                                      regional.plots$p3, 
                                      nrows=3, shareX = TRUE, titleY = TRUE, margin = 0.05) %>%
  layout(legend = list(y=1))


### FIGURE 4

# Total daily contacts (and variations for supplementary materials), coloured by risk
coloured_by_risk_plots <- get_coloured_by_risk_plots(grouped_exposure_data,
                                                     exposure.data.start.date = exposure.data.start.date,
                                                     exposure.data.end.date = exposure.data.end.date,
                                                     national.data.full = national.data.full,
                                                     rho = rho,
                                                     dummy.data = dummy.data)

# Overall transmissions coloured by day of week
transmissions.plot <- get_transmissions_plot(grouped_exposure_data = grouped_exposure_data,
                                       exposure.data.start.date = exposure.data.start.date,
                                       exposure.data.end.date = exposure.data.end.date,
                                       line.height = 10000,
                                       marker.size = 12,
                                       rho = rho)

# combining into one subplot
contacts_and_transmissions_plot <- subplot(coloured_by_risk_plots$p1, 
                                           transmissions.plot,
                                           nrows=2,
                                           shareX=T,
                                           titleX=T,
                                           titleY=T)
                    



### FIGURE 5

# stacked fraction of contacts by setting (and the Christmas-focused ones ready for Figure 7) of 
stacked.fraction.of.contacts.by.setting.plots <- get_stacked_fraction_of_contacts_by_setting_plot(grouped_exposure_data = grouped_exposure_data,
                                                                                                  exposure.data.end.date = exposure.data.end.date)
# stacked fraction of transmissions by setting
stacked.fraction.of.transmissions.by.setting.plots <- get_stacked_fraction_of_transmissions_by_setting_plot(grouped_exposure_data = grouped_exposure_data,
                                                                                                      exposure.data.end.date = exposure.data.end.date,
                                                                                                      rho = rho)

# combining into one subplot
stacked.fractions.plot <- subplot(stacked.fraction.of.contacts.by.setting.plots$p1,
                                  stacked.fraction.of.transmissions.by.setting.plots,
                                  nrows=2,
                                  shareX=TRUE,
                                  titleY=TRUE, 
                                  margin = 0.0)

### FIGURE 6

# transmissions disaggregated by setting and day of week. Two plots are output.
transmissions.by.setting.plot.main <- get_transmissions_by_setting_plot(grouped_exposure_data = grouped_exposure_data,
                                                                  start.date = exposure.data.start.date,
                                                                  end.date = exposure.data.end.date,
                                                                  date.to.annotate.panel.titles = as.Date("2021-08-15"),
                                                                  line.height = 4900,
                                                                  marker.size = 12,
                                                                  rho = rho)

### FIGURE 7

# Figure 7a, C(t) in the winter periods with Christmases annotated, is produced in the "get_national_plot" function above (Figure 1) and can be accessed as national.plot$p5

# Figure 7b, Christmas periods fractions of contacts by setting, combines plots produced during the Figure 5 preparation above
Christmas_fraction_of_contacts_by_setting <- subplot(stacked.fraction.of.contacts.by.setting.plots$p2, 
                                                     stacked.fraction.of.contacts.by.setting.plots$p3,
                                                     nrows=2, 
                                                     shareY=FALSE, titleY=TRUE, titleX=TRUE, shareX=FALSE,
                                                     margin=0)  %>%
                                              layout(legend = list(tracegroupgap = 2000))

# Figure 7c, transmissions over Christmas disaggregated by setting and coloured by day of week
transmissions.by.setting.plot.Dec.2021 <- get_transmissions_by_setting_Dec_plot(grouped_exposure_data = grouped_exposure_data,
                                                                                start.date = as.Date("2021-12-01"),
                                                                                end.date = as.Date("2022-01-11"),
                                                                                date.to.annotate.panel.titles = as.Date("2021-12-20"),
                                                                                line.height = 2500,
                                                                                marker.size = 12,
                                                                                tick.frequency = 4,
                                                                                rho = rho)

### FIGURE 8

# transmissions during the Euros, disaggregated by country
England.Wales.Euros.plot <- get_England_Wales_Euros_plot(grouped_exposure_data = grouped_exposure_data,
                                                         rho = rho)




### SUPPLEMENTARY FIGURES

# Supplementary Figure S1
google_mobility_plot <- get_google_mobility_plot()

# Supplementary Figure S2 was created manually

# Supplementary Figure S3
compute_Rt_decomposition(national.data.full, save.plots.as.pdf, save.plots.as.png)

# Supplementary Figure S4
R_estimates_plot_supplementary <- get_R_estimates_plot_supplementary()

# Supplementary Figures S5 and S6 were created manually

# Supplementary Figures S7 - S9. Code to be cleaned and added.

# Supplementary Figure S10 was calculated earlier in the Figure 4 call, "get_coloured_by_risk_plots". Output is coloured_by_risk_plots$p2

# Supplementary Figure S11
# Overall infections coloured by day of week
infections.plot <- get_infections_plot(grouped_exposure_data = grouped_exposure_data,
                                       exposure.data.start.date = exposure.data.start.date,
                                       exposure.data.end.date = exposure.data.end.date,
                                       line.height = 10000,
                                       marker.size = 12)

# Supplementary Figure S12
# infections disaggregated by setting and day of week
infections.by.setting.plot <- get_infections_by_setting_plot(grouped_exposure_data = grouped_exposure_data,
                                                             start.date = exposure.data.start.date,
                                                             end.date = exposure.data.end.date,
                                                             date.to.annotate.panel.titles = as.Date("2021-08-15"),
                                                             line.height = 6500,
                                                             marker.size = 12)

# Supplementary Figure S13
uptake_plot <- get_uptake_plot(cleaned.app.data = cleaned.app.data,
                               analytics.data.start.date = analytics.data.start.date,
                               analytics.data.end.date = analytics.data.end.date)

# Supplementary Figure S14
region.and.setting.summaries <- get_region_and_setting_summary_plots(grouped_exposure_data = grouped_exposure_data,
                                                                     exposure.data.start.date = exposure.data.start.date,
                                                                     exposure.data.end.date = exposure.data.end.date)



# Supplementary Figure S15 was calculated in the Figure 4 call, "get_coloured_by_risk_plot": the four outputs "coloured_by_risk_plots$p3" - "coloured_by_risk_plots$p6"

# Supplementary Figure S16
transmissions.by.setting.plot.Dec.2022 <- get_transmissions_by_setting_Dec_plot(grouped_exposure_data = grouped_exposure_data,
                                                                            start.date = as.Date("2022-12-01"),
                                                                            end.date = as.Date("2023-01-11"),
                                                                            date.to.annotate.panel.titles = as.Date("2022-12-20"),
                                                                            line.height = 85,
                                                                            marker.size = 12,
                                                                            tick.frequency = 4,
                                                                            rho = rho)


# Supplementary Figure S17
# infections during the Euros, disaggregated by country
England.Wales.Euros.infections.plot <- get_England_Wales_Euros_infections_plot(grouped_exposure_data = grouped_exposure_data)

# Supplementary Figure S18
coloured_by_risk_plots_Euro <- get_coloured_by_risk_plots_Euros(grouped_exposure_data,
                                                                exposure.data.start.date = as.Date("2021-05-21"),
                                                                exposure.data.end.date = as.Date("2021-07-31"),
                                                                national.data.full = national.data.full,
                                                                rho = rho,
                                                                dummy.data = dummy.data)
                                                          

# Supplementary Figures S19 - end. Code to be cleaned and added.


#################
# Saving plots
#################

save_image <- function(...) {
  scope <- kaleido()
  scope$scope$mathjax = NULL
  scope$transform(...)
}

if (save.plots.as.png || save.plots.as.pdf) {
  
  file.types <- c("png","pdf")[c(save.plots.as.png, save.plots.as.pdf)]
  
  for (f in file.types) {
    
    # Figure 1
    save_image(figure_national_timeseries, file=glue("plots/figure_1_decomposition_of_R.{f}"), width=2000, height=1800)
    
    # Figure 2
    save_image(R_estimates_plot, file=glue("plots/figure_2_R_estimates_plot.{f}"), width=2000, height=2000)
    
    # Figure 3
    mapshot(regional.maps$p1, file=glue("plots/figure_3a_regional_map_ct.{f}"), vwidth=440, vheight=450, selfcontained=FALSE)
    
    mapshot(regional.maps$p2, file=glue("plots/figure_3b_regional_map_TPAEN.{f}"), vwidth=440, vheight=450, selfcontained=FALSE)
    
    mapshot(regional.maps$p3, file=glue("plots/figure_3c_regional_map_Rt.{f}"), vwidth=440, vheight=450, selfcontained=FALSE)
    
    save_image(figure_regional_timeseries, file=glue("plots/figure_3def_decomposition_of_R_regional.{f}"), width=2000, height=1500)
    
    # Figure 4
    save_image(contacts_and_transmissions_plot, file=glue("plots/figure_4_contacts_risk_and_transmissions.{f}"), width=1500, height=1200)

    # Figure 5
    save_image(stacked.fractions.plot, file=glue("plots/figure_5_stacked.fractions.plot.{f}"), width = 1500, height = 1200)

    # Figure 6
    save_image(transmissions.by.setting.plot.main$p1, file=glue("plots/figure_6a_transmissions_by_setting.{f}"), width=1500, height=1200)

    save_image(transmissions.by.setting.plot.main$p2, file=glue("plots/figure_6b_transmissions_by_setting_day_summary.{f}"), width=1500, height=400)

    # Figure 7
    save_image(national.plot$p5, file=glue("plots/figure_7a_contact_rate_annotating_Christmases.{f}"), width=1500, height=600)
    
    save_image(Christmas_fraction_of_contacts_by_setting, file=glue("plots/figure_7b_Christmas_fraction_of_contacts_by_setting.{f}"), width=750, height=900)
    
    save_image(transmissions.by.setting.plot.Dec.2021, file=glue("plots/figure_7c_transmissions_by_setting_Dec_2021.{f}"), width=750, height=900)
    
    # Figure 8
    save_image(England.Wales.Euros.plot$p1, file=glue("plots/figure_8a_euros.{f}"), width=1000, height=1200)

    save_image(England.Wales.Euros.plot$p2, file=glue("plots/figure_8b_euros.{f}"), width=1000, height=1200)

    
    # Supplementary figures
    save_image(google_mobility_plot, file=glue("plots/supp_figure_S1_google_mobility.{f}"), height=800, width=1600)
    
    save_image(R_estimates_plot_supplementary, file=glue("plots/supp_figure_S4_R_ensemble_estimates.{f}"), width=2400, height=2800)
    
    save_image(coloured_by_risk_plots$p2, file=glue("plots/supp_figure_S10_contacts_and_risk_with_notifications.{f}"), width=1500, height=700)
    
    save_image(infections.plot, file=glue("plots/supp_figure_S11_infections.{f}"), width=1500, height=600)
    
    save_image(infections.by.setting.plot, file=glue("plots/supp_figure_S12_infections_by_setting.{f}"), width=1500, height=1200)
    
    save_image(uptake_plot, file=glue("plots/supp_figure_S13_active_users.{f}"), width=1200, height=600)

    save_image(region.and.setting.summaries, file=glue("plots/supp_figure_S14_region_and_setting_summaries.{f}"), width=1500, height=1800)
    
    save_image(coloured_by_risk_plots$p3, file=glue("plots/supp_figure_S15a_contacts_and_risk_households.{f}"), width=1500, height=500)

    save_image(coloured_by_risk_plots$p4, file=glue("plots/supp_figure_S15b_contacts_and_risk_recurring.{f}"), width=1500, height=500)

    save_image(coloured_by_risk_plots$p5, file=glue("plots/supp_figure_S15c_contacts_and_risk_single_day.{f}"), width=1500, height=500)

    save_image(coloured_by_risk_plots$p6, file=glue("plots/supp_figure_S15d_contacts_and_risk_fleeting.{f}"), width=1500, height=500)

    save_image(transmissions.by.setting.plot.Dec.2022, file=glue("plots/supp_figure_S16_transmissions_by_setting_Dec_2022.{f}"), width=1500, height=900)
    
    save_image(England.Wales.Euros.infections.plot$p1, file=glue("plots/supp_figure_S17a_euros_absolute.{f}"), width=1000, height=1200)
    
    save_image(England.Wales.Euros.infections.plot$p2, file=glue("plots/supp_figure_S17b_euros_detrended.{f}"), width=1000, height=1200)
    
    save_image(coloured_by_risk_plots_Euro$p1, file=glue("plots/supp_figure_S18a_Euro_contacts_and_risk_households.{f}"), width=1500, height=500)
    
    save_image(coloured_by_risk_plots_Euro$p2, file=glue("plots/supp_figure_S18b_Euro_contacts_and_risk_recurring.{f}"), width=1500, height=500)
    
    save_image(coloured_by_risk_plots_Euro$p3, file=glue("plots/supp_figure_S18c_Euro_contacts_and_risk_single_day.{f}"), width=1500, height=500)
    
    save_image(coloured_by_risk_plots_Euro$p4, file=glue("plots/supp_figure_S18d_Euro_contacts_and_risk_fleeting.{f}"), width=1500, height=500)
  }
  
  # zip the plot files ready for easy exporting / downloading
  zip(zipfile = glue("plots/all_plots"), files = 'plots/')  
  
}


