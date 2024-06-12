get_stacked_fraction_of_transmissions_by_setting_plot <- function(grouped_exposure_data,
                                              start.date,
                                              exposure.data.end.date,
                                              date.to.annotate.panel.titles,
                                              line.height,
                                              marker.size,
                                              rho) {
  
  fleeting.color <- "#88CCEE"
  single.day.color <- "#44AA99"
  recurring.color <- "#CC6677"
  household.color <- "#882255"
  
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>% 
    group_by(date_plotted_exposure, setting) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
    mutate(weekday=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
    filter(date_plotted_exposure <= exposure.data.end.date & date_plotted_exposure >= as.Date("2021-04-01")) %>%
    ungroup()

  
  # pull out some key stats
  household_transmissions_total <- sum((grouped_exposure_data_for_plotting %>% filter(setting == "household"))$transmissions)
  household_transmissions_mean <- mean((grouped_exposure_data_for_plotting %>% filter(setting == "household"))$transmissions)
  household_transmissions_max <- max((grouped_exposure_data_for_plotting %>% filter(setting == "household"))$transmissions)
  
  recurring_transmissions_total <- sum((grouped_exposure_data_for_plotting %>% filter(setting == "recurring"))$transmissions)
  recurring_transmissions_mean <- mean((grouped_exposure_data_for_plotting %>% filter(setting == "recurring"))$transmissions)
  recurring_transmissions_max <- max((grouped_exposure_data_for_plotting %>% filter(setting == "recurring"))$transmissions)
  
  single_day_transmissions_total <- sum((grouped_exposure_data_for_plotting %>% filter(setting == "single day"))$transmissions)
  single_day_transmissions_mean <- mean((grouped_exposure_data_for_plotting %>% filter(setting == "single day"))$transmissions)
  single_day_transmissions_max <- max((grouped_exposure_data_for_plotting %>% filter(setting == "single day"))$transmissions)
  
  fleeting_transmissions_total <- sum((grouped_exposure_data_for_plotting %>% filter(setting == "fleeting"))$transmissions)
  fleeting_transmissions_mean <- mean((grouped_exposure_data_for_plotting %>% filter(setting == "fleeting"))$transmissions)
  fleeting_transmissions_max <- max((grouped_exposure_data_for_plotting %>% filter(setting == "fleeting"))$transmissions)
  
  wider_table_fraction_of_transmissions_to_plot <- grouped_exposure_data_for_plotting %>%
    select(-weekday) %>%
    pivot_wider(names_from = setting, values_from = transmissions) %>%
    mutate("total" = household + recurring + `single day` + fleeting,
           "fraction_household" = household / total,
           "fraction_recurring" = recurring / total,
           "fraction_single_day" = `single day` / total,
           "fraction_fleeting" = fleeting / total) 
  
  write_lines(c(
    # stats to quote / discuss:
    paste0("OVERALL:"),
    paste0("fraction of transmission events which were household:"),
    paste0("median ", median(wider_table_fraction_of_transmissions_to_plot$fraction_household)),
    paste0("min ", min(wider_table_fraction_of_transmissions_to_plot$fraction_household), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.min(wider_table_fraction_of_transmissions_to_plot$fraction_household)]),
    paste0("max ", max(wider_table_fraction_of_transmissions_to_plot$fraction_household), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.max(wider_table_fraction_of_transmissions_to_plot$fraction_household)]),
    paste0("fraction of transmission events which were recurring:"),
    paste0("median ", median(wider_table_fraction_of_transmissions_to_plot$fraction_recurring)), 
    paste0("min ", min(wider_table_fraction_of_transmissions_to_plot$fraction_recurring), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.min(wider_table_fraction_of_transmissions_to_plot$fraction_recurring)]),
    paste0("max ", max(wider_table_fraction_of_transmissions_to_plot$fraction_recurring), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.max(wider_table_fraction_of_transmissions_to_plot$fraction_recurring)]),
    paste0("fraction of transmission events which were single day:"),
    paste0("median ", median(wider_table_fraction_of_transmissions_to_plot$fraction_single_day)) ,
    paste0("min ", min(wider_table_fraction_of_transmissions_to_plot$fraction_single_day), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.min(wider_table_fraction_of_transmissions_to_plot$fraction_single_day)]),
    paste0("max ", max(wider_table_fraction_of_transmissions_to_plot$fraction_single_day), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.max(wider_table_fraction_of_transmissions_to_plot$fraction_single_day)]),
    paste0("fraction of transmission events which were fleeting:"),
    paste0("median ", median(wider_table_fraction_of_transmissions_to_plot$fraction_fleeting)) ,
    paste0("min ", min(wider_table_fraction_of_transmissions_to_plot$fraction_fleeting), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.min(wider_table_fraction_of_transmissions_to_plot$fraction_fleeting)]),
    paste0("max ", max(wider_table_fraction_of_transmissions_to_plot$fraction_fleeting), " on ", wider_table_fraction_of_transmissions_to_plot$date_plotted_exposure[which.max(wider_table_fraction_of_transmissions_to_plot$fraction_fleeting)])
    
  ),
  file="results/fraction.of.transmissions.by.setting.txt")
  
  date.labels.exposure.data <- seq.Date(as.Date("2021-04-01"), as.Date("2022-02-28"), by="month")
  
  stacked_fraction_of_transmissions_plot <- plot_ly(wider_table_fraction_of_transmissions_to_plot, 
                                                    type="bar", 
                                                    x=~date_plotted_exposure, y=~fraction_fleeting, name="Fleeting", color=I(fleeting.color), showlegend=FALSE) %>%
    add_trace(x=~date_plotted_exposure, y=~fraction_single_day, name="Single day", color=I(single.day.color), showlegend=FALSE) %>%
    add_trace(x=~date_plotted_exposure, y=~fraction_recurring, name="Recurring", color=I(recurring.color), showlegend=FALSE) %>%
    add_trace(x=~date_plotted_exposure, y=~fraction_household, name="Household", color=I(household.color), showlegend=FALSE) %>%
    layout(
      xaxis=list(
        title="Date of exposure",
        titlefont=f1,
        tickfont=f1,
        tickvals = date.labels.exposure.data,
        ticktext = paste0("1 ",format(date.labels.exposure.data, "%b %y")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        title="Fraction of transmission events",
        titlefont=f1,
        tickfont=f1
      ),
      legend=list(
        font=f1),
      barmode="stack",
      bargap = 0
    )
  
  stacked_fraction_of_transmissions_plot
}
  
