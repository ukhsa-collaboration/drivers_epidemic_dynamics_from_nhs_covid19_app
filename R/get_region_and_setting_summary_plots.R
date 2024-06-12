get_region_and_setting_summary_plots <- function(grouped_exposure_data,
                                                 exposure.data.start.date,
                                                 exposure.data.end.date) {
  
  # prepare tidy region and setting names
  regions <- tibble(bind_cols(
    "region" = sort(unique(grouped_exposure_data$region)),
    "region_name" = c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "Wales", "West Midlands", "Yorkshire and The Humber")
  )
  )
  
  regions_ordered_by_gdp <- c("South East", "London", "East of England", "South West", "West Midlands", "North West", "East Midlands", "Wales", "Yorkshire and The Humber", "North East")
  
  settings <- tibble(bind_cols(
    "setting" = sort(unique(grouped_exposure_data$setting)),
    "setting_name" = c("Household", "Recurring", "Single day", "Fleeting")
  )
  )
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>% 
      group_by(date_plotted_exposure, setting, region) %>% 
      summarise("contacts" = sum(weight, na.rm = T),
                "transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(0.4+0.07*(classification=='fleeting'))))*weight, na.rm = T))
      ) %>%
      mutate("TPAEN" = transmissions / contacts) %>%
      filter(date_plotted_exposure <= exposure.data.end.date & date_plotted_exposure >= exposure.data.start.date) %>%
      ungroup() %>%
    left_join(., regions) %>%
    left_join(., settings)
  
  median_region_and_setting_summary <- grouped_exposure_data_for_plotting %>%
    group_by(region_name, setting_name) %>%
    summarise("median_contacts" = median(contacts, na.rm=T),
              "median_transmissions" = median(transmissions, na.rm=T),
              "median_TPAEN" = median(TPAEN, na.rm=T)) %>%
    ungroup() %>%
    mutate("setting_name" = factor(setting_name, levels=c("Fleeting", "Single day", "Recurring", "Household"), ordered=TRUE) )
  
  fleeting.color <- "#88CCEE"
  single.day.color <- "#44AA99"
  recurring.color <- "#CC6677"
  household.color <- "#882255"
  
  
  contacts_plot <- plot_ly(median_region_and_setting_summary, type="bar",
          x=~region_name, y=~median_contacts,
          color=~setting_name,
          colors=c(fleeting.color, single.day.color, recurring.color, household.color),
          legendgroup="setting", showlegend=FALSE) %>%
    layout(
      xaxis=list(
        title = "",
        titlefont=f1,
        tickfont=f2,
        categoryorder = "array",
        categoryarray = regions_ordered_by_gdp
      ),
      yaxis=list(
        title="Median daily contacts",
        titlefont=f2,
        tickfont=f2
      ),
      legend = list(
        font=f2
      )
    )
  
  transmissions_plot <- plot_ly(median_region_and_setting_summary, type="bar",
          x=~region_name, y=~median_transmissions,
          color=~setting_name,
          colors=c(fleeting.color, single.day.color, recurring.color, household.color),
          legendgroup="setting", showlegend=FALSE) %>%
    layout(
      xaxis=list(
        title = "",
        titlefont=f1,
        tickfont=f2,
        categoryorder = "array",
        categoryarray = regions_ordered_by_gdp
        ),
      yaxis=list(
        title="Median daily transmission events",
        titlefont=f2,
        tickfont=f2
      )
    )
  
  # TPAEN_plot <- plot_ly(median_region_and_setting_summary, type="bar",
  #         x=~region_name, y=~median_TPAEN,
  #         color=~setting_name,
  #         colors=c(fleeting.color, single.day.color, recurring.color, household.color),
  #         legendgroup="setting") %>%
  #   layout(
  #     xaxis=list(
  #       title = "",
  #       titlefont=f1,
  #       tickfont=f2
  #       #categoryorder = "array",
  #       #categoryarray = c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "Wales", "West Midlands", "Yorkshire and The Humber")
  #     ),
  #     yaxis=list(
  #       title="Median transmissions per contact (TPAEN)",
  #       titlefont=f2,
  #       tickfont=f2
  #     ),
  #     legend = list(
  #       font=f2
  #     )
  #   )
  
  median_region_and_setting_summary_as_fractions <- left_join(median_region_and_setting_summary,
    median_region_and_setting_summary %>%
      group_by(region_name) %>%
      summarise("region_contacts_denominator" = sum(median_contacts),
                "region_transmissions_denominator" = sum(median_transmissions))
  ) %>%
    mutate("fraction_contacts_by_setting" = median_contacts / region_contacts_denominator,
           "fraction_transmissions_by_setting" = median_transmissions / region_transmissions_denominator)
  
  contacts_as_fractions_plot <- plot_ly(median_region_and_setting_summary_as_fractions, type="bar",
                                        x=~region_name, y=~fraction_contacts_by_setting,
                                        color=~setting_name,
                                        colors=c(fleeting.color, single.day.color, recurring.color, household.color),
                                        legendgroup="setting", showlegend=FALSE) %>%
    layout(
      xaxis=list(
        title = "",
        titlefont=f1,
        tickfont=f2
        #categoryorder = "array",
        #categoryarray = c("East Midlands", "East of England", "London", "North East", "North West", "South East", "South West", "Wales", "West Midlands", "Yorkshire and The Humber")
      ),
      yaxis=list(
        title="Median fraction of contacts",
        titlefont=f2,
        tickfont=f2,
        categoryorder = "array",
        categoryarray = regions_ordered_by_gdp
      ),
      legend = list(
        font=f2
      )
    )
  
  transmissions_as_fractions_plot <- plot_ly(median_region_and_setting_summary_as_fractions, type="bar",
                                        x=~region_name, y=~fraction_transmissions_by_setting,
                                        color=~setting_name,
                                        colors=c(fleeting.color, single.day.color, recurring.color, household.color),
                                        legendgroup="setting", showlegend=TRUE) %>%
    layout(
      xaxis=list(
        title = "",
        titlefont=f1,
        tickfont=f2,
        categoryorder = "array",
        categoryarray = regions_ordered_by_gdp
        ),
      yaxis=list(
        title="Median fraction of transmission events",
        titlefont=f2,
        tickfont=f2
      ),
      legend = list(
        font=f2
      )
    )
  
  subplot(contacts_as_fractions_plot,
          transmissions_as_fractions_plot,
          contacts_plot, 
          transmissions_plot,
          nrows=4, titleY=TRUE, shareX=TRUE,
          margin=0.01)
  
}