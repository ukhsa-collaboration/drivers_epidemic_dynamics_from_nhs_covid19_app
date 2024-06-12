get_transmissions_plot <- function(grouped_exposure_data,
                                exposure.data.start.date,
                                exposure.data.end.date,
                                line.height,
                                marker.size,
                                rho) {
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>%
    filter(date_plotted_exposure <= exposure.data.end.date & date_plotted_exposure >= as.Date("2021-04-01")) %>% 
    group_by(date_plotted_exposure) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
    mutate("weekday" = factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days))  %>%
    ungroup()
  
  
  first.date.to.plot.infections.main <- exposure.data.start.date
  last.date.to.plot.infections.main <- exposure.data.end.date
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data_for_plotting %>%
    filter(date_plotted_exposure >= first.date.to.plot.infections.main,
           date_plotted_exposure <= last.date.to.plot.infections.main)
 
  Euro.final.day <- grouped_exposure_data_for_plotting %>% 
    filter(date_plotted_exposure == as.Date("2021-07-11"))
  Euro.final.week.before <- grouped_exposure_data_for_plotting %>% 
    filter(date_plotted_exposure >= as.Date("2021-07-04"),
           date_plotted_exposure < as.Date("2021-07-11"))
  Euro.final.week.after <- grouped_exposure_data_for_plotting %>% 
    filter(date_plotted_exposure > as.Date("2021-07-11"),
           date_plotted_exposure <= as.Date("2021-07-18"))
  Euro.final.surrounding.fortnight <- rbind(Euro.final.week.before,
                                            Euro.final.week.after)
  Euro.final.surrounding.Sundays <- Euro.final.surrounding.fortnight %>%
    filter(weekday == "Sun")
  
  # stats to quote / discuss: 
  write_lines(c(
    paste0("Total transmissions from ",first.date.to.plot.infections.main," to ",last.date.to.plot.infections.main,":"),
    paste0(signif(sum(grouped_exposure_data_for_plotting$transmissions, na.rm=T), 6)),
    paste0("transmissions on Euro final day:"),
    paste0(Euro.final.day$transmissions),
    paste0("Mean transmissions for week before:"),
    paste0(mean(Euro.final.week.before$transmissions)),
    paste0("So transmissions on Euro final day were ",Euro.final.day$transmissions / mean(Euro.final.week.before$transmissions), " times higher than in the preceeding week"),
    paste0("Mean transmissions for week after:"),
    paste0(mean(Euro.final.week.after$transmissions)),
    paste0("So transmissions on Euro final day were ",Euro.final.day$transmissions / mean(Euro.final.week.after$transmissions), " times higher than in the following week"),
    paste0("Mean transmissions for surrounding fortnight:"),
    paste0(mean(Euro.final.surrounding.fortnight$transmissions)),
    paste0("So transmissions on Euro final day were ",Euro.final.day$transmissions / mean(Euro.final.surrounding.fortnight$transmissions), " times higher than in the surrounding fortnight"),
    paste0("transmissions on Sunday before and after were respectively:"),
    paste0(Euro.final.surrounding.Sundays$transmissions),
    paste0("So transmissions on Euro final day were ",Euro.final.day$transmissions / Euro.final.surrounding.Sundays$transmissions[[1]], " times higher than on the Sunday before"),
    paste0("and ",Euro.final.day$transmissions / Euro.final.surrounding.Sundays$transmissions[[2]], " times higher than on the Sunday after")
  ),
  file="results/infections_2021-04-01_to_2022-02-28.txt")
 
  #mean(grouped_exposure_data_for_plotting$infections, na.rm=T)
  
  date.labels.exposure.data <- seq.Date(as.Date("2021-04-01"), as.Date("2022-02-28"), by="month")
  
  plot_ly(grouped_exposure_data_for_plotting, colors=weekday.cols) %>%
    # add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
    #                  as.Date("2021-07-11"), as.Date("2021-07-11")),
    #              y=c(0,line.height, line.height, 0),
    #              color=I("lightgrey"), showlegend=FALSE, opacity = 0.5) %>%
    # add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, color=I("darkgrey"), showlegend=FALSE) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup="weekday",
              marker=list(size=marker.size)) %>%
    layout(
      xaxis=list(
        range=c(first.date.to.plot.infections.main, last.date.to.plot.infections.main),
        tickfont=f2,
        titlefont=f1,
        title="Date of exposure",
        tickvals = date.labels.exposure.data,
        ticktext = paste0("1 ",format(date.labels.exposure.data, "%b %y")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f1,
        range=c(0,line.height),
        title="Daily number of transmission events\ndetected by the app"
      ),
      legend=list(
        font=f1,
        y=0.1
      )
    )
  
 
}
