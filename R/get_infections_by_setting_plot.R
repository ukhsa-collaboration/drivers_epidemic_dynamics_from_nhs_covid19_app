get_infections_by_setting_plot <- function(grouped_exposure_data,
                                  start.date,
                                  end.date,
                                  date.to.annotate.panel.titles,
                                  line.height,
                                  marker.size,
                                  tick.frequency="month") {
  
  grouped_exposure_data_for_plotting <- grouped_exposure_data %>% 
   group_by(date_plotted_exposure, setting) %>% 
    arrange(date_plotted_exposure) %>%
    summarise("infections" = sum(positive*weight, na.rm = T)) %>%
    mutate("weekday"=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
    filter(date_plotted_exposure <= end.date & date_plotted_exposure >= start.date) %>%
    ungroup()

  # using "group_by(setting)" really ought to achieve the facet wrap we want, but I'm having issues with repeated legends and yaxis titles.
  # Going manual!

  first.date.to.plot.infections <- start.date
  last.date.to.plot.infections <- end.date
  
  date.labels <- seq.Date(first.date.to.plot.infections, last.date.to.plot.infections, by=tick.frequency)
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
    add_lines(x=~date_plotted_exposure, y=~infections, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~infections, 
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
        range=c(first.date.to.plot.infections, last.date.to.plot.infections),
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
  
  actual.line.height<-line.height/4
  
  plot_r <- grouped_exposure_data_for_plotting %>%
    filter(setting == "recurring") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,actual.line.height, actual.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~infections, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~infections, 
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
        range=c(first.date.to.plot.infections, last.date.to.plot.infections),
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
        title="Daily number of infection events detected by the app"
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
    add_lines(x=~date_plotted_exposure, y=~infections, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~infections, 
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
        range=c(first.date.to.plot.infections, last.date.to.plot.infections),
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
  
  actual.line.height<-line.height/1.8
  
  plot_f <- grouped_exposure_data_for_plotting %>%
    filter(setting == "fleeting") %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,actual.line.height, actual.line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,actual.line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~infections, showlegend=FALSE, color=I("darkgrey")) %>%
    add_trace(x=~date_plotted_exposure, y=~infections, 
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
        range=c(first.date.to.plot.infections, last.date.to.plot.infections),
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
  
  infections_by_exposure_type_plot <- subplot(
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
  
  infections_by_exposure_type_plot
}

