get_infections_plot <- function(grouped_exposure_data,
                                exposure.data.start.date,
                                exposure.data.end.date,
                                line.height,
                                marker.size) {
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>%
    filter(date_plotted_exposure <= exposure.data.end.date & date_plotted_exposure >= as.Date("2021-04-01")) %>% 
    group_by(date_plotted_exposure) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("infections" = sum(positive*weight, na.rm = T)) %>%
    mutate("weekday" = factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days))  %>%
    ungroup()
  
  
  first.date.to.plot.infections.main <- exposure.data.start.date
  last.date.to.plot.infections.main <- exposure.data.end.date
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data_for_plotting %>%
    filter(date_plotted_exposure >= first.date.to.plot.infections.main,
           date_plotted_exposure <= last.date.to.plot.infections.main)
 
  date.labels.exposure.data <- seq.Date(as.Date("2021-04-01"), as.Date("2022-02-28"), by="month")
  
  plot_ly(grouped_exposure_data_for_plotting, colors=weekday.cols) %>%
    add_lines(x=~date_plotted_exposure, y=~infections, color=I("darkgrey"), showlegend=FALSE) %>%
    add_trace(x=~date_plotted_exposure, y=~infections, 
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
        title="Daily number of infection events\ndetected by the app"
      ),
      legend=list(
        font=f1,
        y=0.1
      )
    )
  
 
}
