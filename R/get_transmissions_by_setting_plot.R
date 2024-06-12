get_transmissions_by_setting_plot <- function(grouped_exposure_data,
                                  start.date,
                                  end.date,
                                  date.to.annotate.panel.titles,
                                  line.height,
                                  marker.size,
                                  tick.frequency="month",
                                  rho) {
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>% 
   group_by(date_plotted_exposure, setting) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
    mutate("weekday"=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
    filter(date_plotted_exposure <= end.date & date_plotted_exposure >= start.date) %>%
    ungroup()

  write_csv(grouped_exposure_data %>%
              group_by(date_plotted_exposure, setting) %>% 
              arrange(date_plotted_exposure) %>%
              summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
              mutate("weekday"=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
              filter(date_plotted_exposure <= end.date & date_plotted_exposure >= start.date) %>%
              arrange(date_plotted_exposure) %>%
              ungroup()  %>% 
              mutate(date_exposure=date_plotted_exposure,type=setting,number_of_transmissions=transmissions) %>% 
              select(date_exposure,type,number_of_transmissions),
            file="results/SupplementaryTable2.csv")
    
  # for household transmissions, find weekly fractional contributions by day of the week; take overall medians
  household_transmissions_with_weekly_totals <- left_join(grouped_exposure_data_for_plotting %>% 
                                                         filter(setting == "household") %>%
                                                         select(date_plotted_exposure, transmissions, weekday) %>%
                                                         mutate("week" = lubridate::week(date_plotted_exposure)),
                                                       grouped_exposure_data_for_plotting %>% 
                                                         filter(setting == "household") %>%
                                                         group_by("week" = lubridate::week(date_plotted_exposure)) %>% 
                                                         summarise("weekly_total" = sum(transmissions))
    ) %>%
    mutate("weekly_fraction" = transmissions / weekly_total)
  
  # repeat for each setting
  recurring_transmissions_with_weekly_totals <- left_join(grouped_exposure_data_for_plotting %>% 
                                                         filter(setting == "recurring") %>%
                                                         select(date_plotted_exposure, transmissions, weekday) %>%
                                                         mutate("week" = lubridate::week(date_plotted_exposure)),
                                                       grouped_exposure_data_for_plotting %>% 
                                                         filter(setting == "recurring") %>%
                                                         group_by("week" = lubridate::week(date_plotted_exposure)) %>% 
                                                         summarise("weekly_total" = sum(transmissions))
  ) %>%
    mutate("weekly_fraction" = transmissions / weekly_total)
  
  single_day_transmissions_with_weekly_totals <- left_join(grouped_exposure_data_for_plotting %>% 
                                                         filter(setting == "single day") %>%
                                                         select(date_plotted_exposure, transmissions, weekday) %>%
                                                         mutate("week" = lubridate::week(date_plotted_exposure)),
                                                       grouped_exposure_data_for_plotting %>% 
                                                         filter(setting == "single day") %>%
                                                         group_by("week" = lubridate::week(date_plotted_exposure)) %>% 
                                                         summarise("weekly_total" = sum(transmissions))
  ) %>%
    mutate("weekly_fraction" = transmissions / weekly_total)
  
  fleeting_transmissions_with_weekly_totals <- left_join(grouped_exposure_data_for_plotting %>% 
                                                          filter(setting == "fleeting") %>%
                                                          select(date_plotted_exposure, transmissions, weekday) %>%
                                                          mutate("week" = lubridate::week(date_plotted_exposure)),
                                                        grouped_exposure_data_for_plotting %>% 
                                                          filter(setting == "fleeting") %>%
                                                          group_by("week" = lubridate::week(date_plotted_exposure)) %>% 
                                                          summarise("weekly_total" = sum(transmissions))
  ) %>%
    mutate("weekly_fraction" = transmissions / weekly_total)
  
  weekly_median_contributions <- tibble(
    "setting" = c("Household", "Recurring", "Single day", "Fleeting"),
    "Mondays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Mon"))$weekly_fraction ),
                  median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Mon"))$weekly_fraction ),
                  median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Mon"))$weekly_fraction ),
                  median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Mon"))$weekly_fraction )
                  ),
    "Tuesdays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Tue"))$weekly_fraction ),
                   median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Tue"))$weekly_fraction ),
                   median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Tue"))$weekly_fraction ),
                   median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Tue"))$weekly_fraction )
                   ),
    "Wednesdays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Wed"))$weekly_fraction ),
                     median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Wed"))$weekly_fraction ),
                     median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Wed"))$weekly_fraction ),
                     median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Wed"))$weekly_fraction )
                     ),
    "Thursdays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Thu"))$weekly_fraction ),
                    median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Thu"))$weekly_fraction ),
                    median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Thu"))$weekly_fraction ),
                    median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Thu"))$weekly_fraction )
                    ),
    "Fridays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Fri"))$weekly_fraction ),
                  median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Fri"))$weekly_fraction ),
                  median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Fri"))$weekly_fraction ),
                  median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Fri"))$weekly_fraction )
                  ),
    "Saturdays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Sat"))$weekly_fraction ),
                    median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Sat"))$weekly_fraction ),
                    median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Sat"))$weekly_fraction ),
                    median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Sat"))$weekly_fraction )
                    ),
    "Sundays" = c(median((household_transmissions_with_weekly_totals %>% filter(weekday == "Sun"))$weekly_fraction ),
                  median((recurring_transmissions_with_weekly_totals %>% filter(weekday == "Sun"))$weekly_fraction ),
                  median((single_day_transmissions_with_weekly_totals %>% filter(weekday == "Sun"))$weekly_fraction ),
                  median((fleeting_transmissions_with_weekly_totals %>% filter(weekday == "Sun"))$weekly_fraction )
                  )
  ) %>%
    pivot_longer(cols=c("Mondays", "Tuesdays", "Wednesdays", "Thursdays", "Fridays", "Saturdays", "Sundays"), names_to="weekday") %>%
    mutate("setting" = factor(setting, levels=c( "Fleeting", "Single day", "Recurring",  "Household"), ordered=TRUE) )
  
  
  fleeting.color <- "#88CCEE"
  single.day.color <- "#44AA99"
  recurring.color <- "#CC6677"
  household.color <- "#882255"
  
  transmissions_by_day_summary_plot <- plot_ly(weekly_median_contributions, type="bar",
          x=~weekday, y=~value,
          color=~setting, 
          colors=c( fleeting.color, single.day.color, recurring.color, household.color),
          legendgroup="by_day_summary") %>%
    layout(
      xaxis=list(
        title = "",
        titlefont=f1,
        tickfont=f2,
        categoryorder = "array",
        categoryarray = c("Mondays", "Tuesdays", "Wednesdays", "Thursdays", "Fridays", "Saturdays", "Sundays")
      ),
      yaxis=list(
        title="Median weekly fraction\nof transmissions events",
        titlefont=f2,
        tickfont=f2
      ),
      legend = list(
        font=f2
      )
    )

  
  write_lines(c(
    paste0("Household transmissions events on Sundays were between"),
    paste0( (weekly_median_contributions %>% filter(setting == "Household"))$value[[7]] / max((weekly_median_contributions %>% filter(setting == "Household"))$value[1:6]) ),
    paste0("times higher (comparing to ", (weekly_median_contributions %>% filter(setting == "Household"))$weekday[which.max((weekly_median_contributions %>% filter(setting == "Household"))$value[1:6])], "), to"),
    paste0( (weekly_median_contributions %>% filter(setting == "Household"))$value[[7]] / min((weekly_median_contributions %>% filter(setting == "Household"))$value[1:6]) ),
    paste0("times higher (comparing to ", (weekly_median_contributions %>% filter(setting == "Household"))$weekday[which.min((weekly_median_contributions %>% filter(setting == "Household"))$value[1:6])], ")."),
    paste0("Fleeting transmissions events on Saturdays were between"),
    paste0( (weekly_median_contributions %>% filter(setting == "Fleeting"))$value[[6]] / max((weekly_median_contributions %>% filter(setting == "Fleeting"))$value[c(1:5,7)]) ),
    paste0("times higher (comparing to ", (weekly_median_contributions %>% filter(setting == "Fleeting", weekday != "Saturdays"))$weekday[which.max((weekly_median_contributions %>% filter(setting == "Fleeting"))$value)], "), to"),
    paste0( (weekly_median_contributions %>% filter(setting == "Fleeting"))$value[[6]] / min((weekly_median_contributions %>% filter(setting == "Fleeting"))$value[c(1:5,7)]) ),
    paste0("times higher (comparing to ", (weekly_median_contributions %>% filter(setting == "Fleeting", weekday != "Saturdays"))$weekday[which.min((weekly_median_contributions %>% filter(setting == "Fleeting"))$value)], ").")
  ),
  file=glue("results/transmissions_by_setting_and_day_of_the_week.txt"))
  
  
  # using "group_by(setting)" really ought to achieve the facet wrap we want, but I'm having issues with repeated legends and yaxis titles.
  # Going manual!

  first.date.to.plot.transmissions <- start.date
  last.date.to.plot.transmissions <- end.date
  
  date.labels <- seq.Date(first.date.to.plot.transmissions, last.date.to.plot.transmissions, by=tick.frequency)
  if (tick.frequency == "month") {custom.tick.labels <- paste0("1 ",format(date.labels, "%b %y"))}
  else custom.tick.labels <- format(date.labels, "%d %b")
  
  actual.line.height<-line.height/3
  
  plot_hh <- grouped_exposure_data_for_plotting %>%
    filter(setting == "household") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,actual.line.height, actual.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=actual.line.height*0.95, text="Household",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        range=c(0,actual.line.height),
        title=""
      ),
      legend=list(
        font=f1
      )
    )
  
  actual.line.height<-line.height/4.5
  
  plot_r <- grouped_exposure_data_for_plotting %>%
    filter(setting == "recurring") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,actual.line.height, actual.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=actual.line.height*0.95, text="Recurring",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f1,
        range=c(0,actual.line.height),
        title="Daily number of transmission events detected by the app"
      )
    )
  
  actual.line.height<-line.height/1.1
  
  plot_sd <- grouped_exposure_data_for_plotting %>%
    filter(setting == "single day") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,actual.line.height, actual.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=actual.line.height*0.95, text="Single day",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f1,
        range=c(0,actual.line.height),
        title=""
      )
    )
  
  actual.line.height<-line.height/2.5
  
  plot_f <- grouped_exposure_data_for_plotting %>%
    filter(setting == "fleeting") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,actual.line.height, actual.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=actual.line.height*0.95, text="Fleeting",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        range=c(0,actual.line.height),
        title=""
      )
    )
  
  transmissions_by_exposure_type_plot <- subplot(
    plot_hh, 
    plot_r, 
    plot_sd, 
    plot_f, 
    nrows=4, shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
    layout(
      xaxis=list(
        titlefont=f1,
        title="Date of exposure"
      )
    )
  
 plots <- list()
 plots$p1 <- transmissions_by_exposure_type_plot
 plots$p2 <- transmissions_by_day_summary_plot
 
 plots
}

# essentially the same again but slightly different aesthetics for the Christmas focused versions
get_transmissions_by_setting_Dec_plot <- function(grouped_exposure_data,
                                              start.date,
                                              end.date,
                                              date.to.annotate.panel.titles,
                                              line.height,
                                              marker.size,
                                              tick.frequency="month",
                                              rho) {
  
  first.date.to.plot.transmissions <- start.date
  last.date.to.plot.transmissions <- end.date
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>% 
    mutate("weekday" = lubridate::wday(date,week_start = 1),
           "date_plotted_exposure" = as.Date("2021-01-03")+randomExpDate) %>%
    group_by(date_plotted_exposure, setting) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
    mutate(weekday=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
    filter(date_plotted_exposure <= (last.date.to.plot.transmissions + 14) & date_plotted_exposure >= (first.date.to.plot.transmissions - 14 ) ) %>% # the plus and minus 14 allow us to calculate the rolling median below
    ungroup()

  
  # compute and save some key stats
  table_total_Christmas_transmissions_with_baseline <- grouped_exposure_data_for_plotting %>% 
    group_by(date_plotted_exposure, weekday) %>%
    summarise("transmissions" = sum(transmissions, na.rm=T)) %>%
    ungroup() %>%
    mutate("baseline" = frollapply(x=transmissions, n=15, FUN=median, fill=NA, align="center"),
           "transmissions_compared_to_baseline" = transmissions / baseline * 100)
  
  # dates of weekends and public holidays in the Christmas period
  dates_hols <- as.Date(c(paste("2021-12-",c("04","05","11","12","18","19","25","26","27","28")),"2022-01-01", "2022-01-08"))
  rows_for_dates_of_hols <- which(table_total_Christmas_transmissions_with_baseline$date_plotted_exposure %in% dates_hols)
  
  # now discard those extra 14 days either side!
  grouped_exposure_data_for_plotting_Christmas_period <- grouped_exposure_data_for_plotting %>% 
    filter(date_plotted_exposure <= last.date.to.plot.transmissions & date_plotted_exposure >= first.date.to.plot.transmissions ) %>% # the plus and minus 14 allow us to calculate the rolling median below. The plots are controlled by x-axis range.
    ungroup()
  
  if (format.Date(start.date, "%Y") == "2021") {
    write_lines(c(
      paste0("Total transmission events:"),
      paste0(sum(grouped_exposure_data_for_plotting_Christmas_period$transmissions, na.rm=T)),
      paste0("Total transmissions events on Christmas Day and the two preceding Saturdays:"),
      paste0(sum((grouped_exposure_data_for_plotting_Christmas_period %>% filter(date_plotted_exposure %in% c(as.Date("2021-12-11"), as.Date("2021-12-18"), as.Date("2021-12-25"))))$transmissions, na.rm=T)),
      paste0("Percentage of total that were on those Saturdays:"),
      paste0(sum((grouped_exposure_data_for_plotting_Christmas_period %>% filter(date_plotted_exposure %in% c(as.Date("2021-12-11"), as.Date("2021-12-18"), as.Date("2021-12-25"))))$transmissions, na.rm=T) / sum(grouped_exposure_data_for_plotting_Christmas_period$transmissions, na.rm=T)),
      paste0("Household transmissions events on Christmas Day were this much higher than the surrounding Saturdays:"),
      paste0( (grouped_exposure_data_for_plotting_Christmas_period %>% 
                 filter(date_plotted_exposure == as.Date("2021-12-25"), 
                        setting == "household"))$transmissions 
              / 
                (grouped_exposure_data_for_plotting_Christmas_period %>% 
                   filter(date_plotted_exposure %in% c(as.Date("2021-12-11"), as.Date("2021-12-18"), as.Date("2021-12-25"), as.Date("2022-01-01"), as.Date("2022-01-08")), 
                          setting == "household"))$transmissions
              * 100 ),
      paste0("Fleeting transmissions events on Christmas Day were this much lower than the surrounding Saturdays:"),
      paste0( 100 - (grouped_exposure_data_for_plotting_Christmas_period %>% 
                       filter(date_plotted_exposure == as.Date("2021-12-25"), 
                              setting == "fleeting"))$transmissions 
              / 
                (grouped_exposure_data_for_plotting_Christmas_period %>% 
                   filter(date_plotted_exposure %in% c(as.Date("2021-12-11"), as.Date("2021-12-18"), as.Date("2021-12-25"), as.Date("2022-01-01"), as.Date("2022-01-08")), 
                          setting == "fleeting"))$transmissions
              * 100 ),
      paste0("Times higher transmissions events on weekends / public holidays:"),
      paste0(min(table_total_Christmas_transmissions_with_baseline$transmissions_compared_to_baseline[rows_for_dates_of_hols], na.rm=T), " to ", max(table_total_Christmas_transmissions_with_baseline$transmissions_compared_to_baseline[rows_for_dates_of_hols], na.rm=T) ),
      paste0("Times higher transmissions events on weekends / public holidays - mean:"),
      paste0(mean(table_total_Christmas_transmissions_with_baseline$transmissions_compared_to_baseline[rows_for_dates_of_hols], na.rm=T)),
      paste0("Weekend / public holiday transmissions over baseline, as a percent of total transmissions:"),
      paste0(sum((table_total_Christmas_transmissions_with_baseline$transmissions - table_total_Christmas_transmissions_with_baseline$baseline)[rows_for_dates_of_hols] , na.rm=T) / sum(grouped_exposure_data_for_plotting$transmissions, na.rm=T) * 100)
    ),
    file=glue("results/Christmas_infection_events_{start.date}-{end.date}.txt"))
  }
  
  date.labels <- seq.Date(first.date.to.plot.transmissions, last.date.to.plot.transmissions, by=tick.frequency)
  if (tick.frequency == "month") {custom.tick.labels <- paste0("1 ",format(date.labels, "%b %y"))}
  else custom.tick.labels <- format(date.labels, "%d %b")

  
  # using "group_by(setting)" really ought to achieve the facet wrap we want, but I'm having issues with repeated legends and yaxis titles.
  # Going manual!
  
  plot_hh <- grouped_exposure_data_for_plotting_Christmas_period %>%
    filter(setting == "household") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=line.height*0.95, text="Household",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions - 1, last.date.to.plot.transmissions + 1),
        tickfont=f3,
        titlefont=f2,
        title=glue("Date of exposure (Dec {format.Date(start.date, '%Y')} - Jan {format.Date(end.date, '%Y')})"),
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f3,
        titlefont=f2,
        range=c(0,line.height),
        title=""
      ),
      legend=list(
        font=f2
      )
    )
  
  plot_r <- grouped_exposure_data_for_plotting_Christmas_period %>%
    filter(setting == "recurring") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=line.height*0.95, text="Recurring",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions - 1, last.date.to.plot.transmissions + 1),
        tickfont=f3,
        titlefont=f2,
        title=glue("Date of exposure (Dec {format.Date(start.date, '%Y')} - Jan {format.Date(end.date, '%Y')})"),
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f3,
        titlefont=f2,
        range=c(0,line.height),
        title="Daily number of transmission events detected by the app"
      )
    )
  
  plot_sd <- grouped_exposure_data_for_plotting_Christmas_period %>%
    filter(setting == "single day") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=line.height*0.95, text="Single day",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions - 1, last.date.to.plot.transmissions + 1),
        tickfont=f3,
        titlefont=f1,
        title=glue("Date of exposure (Dec {format.Date(start.date, '%Y')} - Jan {format.Date(end.date, '%Y')})"),
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f3,
        titlefont=f2,
        range=c(0,line.height),
        title=""
      )
    )
  
  plot_f <- grouped_exposure_data_for_plotting_Christmas_period %>%
    filter(setting == "fleeting") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=line.height*0.95, text="Fleeting",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions - 1, last.date.to.plot.transmissions + 1),
        tickfont=f3,
        titlefont=f2,
        title=glue("Date of exposure (Dec {format.Date(start.date, '%Y')} - Jan {format.Date(end.date, '%Y')})"),
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f3,
        titlefont=f2,
        range=c(0,line.height),
        title=""
      )
    )
  
  transmissions_by_exposure_type_plot <- subplot(plot_hh, plot_r, plot_sd, plot_f, nrows=4, shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
    layout(
      xaxis=list(
        titlefont=f2,
        title=glue("Date of exposure (Dec {format.Date(start.date, '%Y')} - Jan {format.Date(end.date, '%Y')})")
      )#,
      #  yaxis=list(
      #      titlefont=f2,
      #      title="Number of transmissions detected by the app"
      #          )
    )
  
  
  
  transmissions_by_exposure_type_plot
}


# essentially the same again but for later dates, for supplementary
get_transmissions_by_setting_plot_supp <- function(grouped_exposure_data,
                                           start.date,
                                           end.date,
                                           date.to.annotate.panel.titles,
                                           line.height,
                                           marker.size,
                                           tick.frequency="month",
                                           rho) {
  
  small.line.height<-line.height/2
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>% 
    mutate("weekday" = lubridate::wday(date,week_start = 1),
           "date_plotted_exposure" = as.Date("2021-01-03")+randomExpDate) %>%
    group_by(date_plotted_exposure, setting) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
    mutate(weekday=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
    filter(date_plotted_exposure <= end.date & date_plotted_exposure >= start.date) %>%
    ungroup()
  
  
  # using "group_by(setting)" really ought to achieve the facet wrap we want, but I'm having issues with repeated legends and yaxis titles.
  # Going manual!
  
  first.date.to.plot.transmissions <- start.date
  last.date.to.plot.transmissions <- end.date
  
  date.labels <- seq.Date(first.date.to.plot.transmissions, last.date.to.plot.transmissions, by=tick.frequency)
  if (tick.frequency == "month") {custom.tick.labels <- paste0("1 ",format(date.labels, "%b %y"))}
  else custom.tick.labels <- format(date.labels, "%d %b")
  
  plot_hh <- grouped_exposure_data_for_plotting %>%
    filter(setting == "household") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,small.line.height, small.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=small.line.height*0.95, text="Household",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        range=c(0,small.line.height),
        title=""
      ),
      legend=list(
        font=f1
      )
    )
  
  plot_r <- grouped_exposure_data_for_plotting %>%
    filter(setting == "recurring") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,small.line.height, small.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=small.line.height*0.95, text="Recurring",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f1,
        range=c(0,small.line.height),
        title="Daily number of transmissions detected by the app"
      )
    )
  
  plot_sd <- grouped_exposure_data_for_plotting %>%
    filter(setting == "single day") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,small.line.height, small.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=small.line.height*0.95, text="Single day",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f1,
        range=c(0,small.line.height),
        title=""
      )
    )
  
  plot_f <- grouped_exposure_data_for_plotting %>%
    filter(setting == "fleeting") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,small.line.height, small.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,small.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup=~weekday, showlegend=FALSE,
              marker=list(size=marker.size)) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=small.line.height*0.95, text="Fleeting",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.transmissions, last.date.to.plot.transmissions),
        tickfont=f2,
        titlefont=f2,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = custom.tick.labels,
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        range=c(0,small.line.height),
        title=""
      )
    )
  
  transmissions_by_exposure_type_plot <- subplot(
    plot_hh, 
    plot_r, 
    plot_sd, 
    plot_f, 
    nrows=4, shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
    layout(
      xaxis=list(
        titlefont=f1,
        title="Date of exposure"
      )
    )
  transmissions_by_exposure_type_plot
}
