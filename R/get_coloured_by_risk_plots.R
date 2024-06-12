get_coloured_by_risk_plots <- function(grouped_exposure_data, 
                                       exposure.data.start.date, 
                                       exposure.data.end.date, 
                                       national.data.full, 
                                       rho,
                                       dummy.data) {
  
  contacts_and_risks_by_date_exposure <- grouped_exposure_data %>%
    filter(date_plotted_exposure >= exposure.data.start.date,
           date_plotted_exposure <= exposure.data.end.date) %>% 
    group_by(date_plotted_exposure) %>%
    summarise("total_contacts" = sum(weight, na.rm=T),
              "total_cumRiskScore" = sum(cumRiskScore*weight, na.rm=T)/90
    ) %>%
    mutate("mean_cumRiskScore" = total_cumRiskScore / total_contacts) %>%
    arrange(date_plotted_exposure) %>%
    ungroup() 
  
  # write Supplementary Tables for events data
  write_csv(grouped_exposure_data %>%
    filter(date_plotted_exposure >= exposure.data.start.date,
           date_plotted_exposure <= exposure.data.end.date) %>% 
    group_by(date_plotted_exposure,classification) %>%
    summarise("total_contacts" = sum(weight, na.rm=T),
              "total_cumRiskScore" = sum(cumRiskScore*weight, na.rm=T)/90
    ) %>%
    mutate("mean_cumRiskScore" = total_cumRiskScore / total_contacts) %>%
    arrange(date_plotted_exposure) %>%
    ungroup()  %>% 
    mutate(date_exposure=date_plotted_exposure,type=classification,average_cumulativeRiskScore=mean_cumRiskScore) %>% 
    select(date_exposure,type,total_contacts,average_cumulativeRiskScore),
  file="results/SupplementaryTable1.csv")
  write_csv(grouped_exposure_data %>%
              group_by(date_plotted_exposure, setting) %>% 
              arrange(date_plotted_exposure) %>%
              summarise("transmissions" = pmax(0,sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight, na.rm = T))) %>%
              mutate("weekday"=factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
              filter(date_plotted_exposure <= exposure.data.end.date & date_plotted_exposure >= exposure.data.start.date) %>%
              arrange(date_plotted_exposure) %>%
              ungroup()  %>% 
              mutate(date_exposure=date_plotted_exposure,type=setting,number_of_transmissions=transmissions) %>% 
              select(date_exposure,type,number_of_transmissions),
            file="results/SupplementaryTable2.csv")
   
  # prepare plots
  plots <- list()
  
  line.height <- 180000
  date.labels.exposure.data <- seq.Date(exposure.data.start.date, exposure.data.end.date, by="month")
  
  contacts_coloured_by_risk_plot <- plot_ly(contacts_and_risks_by_date_exposure, colors = viridis(1000, direction=-1)) %>%
    # add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
    #                  as.Date("2021-07-11"), as.Date("2021-07-11")),
    #              y=c(0,line.height, line.height, 0),
    #              color=I("lightgrey"), opacity=0.5, showlegend=FALSE) %>%
    #add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_bars(x=~date_plotted_exposure, y=~total_contacts,
             color=~as.factor(mean_cumRiskScore), colors = viridis(1000, direction=-1), showlegend=FALSE) %>%
    add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                marker = list(color = ~signif(mean_cumRiskScore,2),
                              size=0.1,
                              colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                              titlefont=f1, tickfont=f2, 
                                              len=0.4, x=1.03, y=0.8),
                              colorscale = 'Viridis',
                              reversescale = T,
                              showscale = TRUE),
                showlegend=FALSE
    ) %>%
    layout(
      xaxis=list(
        title="Date of exposure",
        tickfont=f2,
        titlefont=f1,
        range=c(exposure.data.start.date, exposure.data.end.date),
        tickvals = date.labels.exposure.data,
        ticktext = paste0("1 ",format(date.labels.exposure.data, "%b %y")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Daily number of contacts\ndetected by the app"
        #range=c(0,line.height)
      ),
      # legend=list(
      #   title=list(text="Average\ndaily proximity", font=f1)
      # ),
      bargap=0
    )
  
  plots$p1 <- contacts_coloured_by_risk_plot
  
  
  # Supplementary version, with notifications
  
  notifications_by_date_of_notification <- national.data.full %>% 
    select(date, notifications) %>%
    filter(date >= exposure.data.start.date,
           date <= exposure.data.end.date) 
  
  contacts_and_notifications_coloured_by_risk_plot <- plot_ly(contacts_and_risks_by_date_exposure, colors = viridis(1000, direction=-1)) %>%
    # add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
    #                  as.Date("2021-07-11"), as.Date("2021-07-11")),
    #              y=c(0,line.height, line.height, 0),
    #              color=I("lightgrey"), opacity=0.5, showlegend=FALSE) %>%
    #add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), line=list(color="black", dash="dash"), showlegend=FALSE) %>%
    add_bars(x=~date_plotted_exposure, y=~total_contacts,
             color=~as.factor(mean_cumRiskScore), colors = viridis(1000, direction=-1), showlegend=FALSE) %>%
    add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                marker = list(color = ~signif(mean_cumRiskScore,2),
                              size=0.1,
                              colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                              titlefont=f1, tickfont=f2, 
                                              len=0.6, x=1.03, y=0.7),
                              colorscale = 'Viridis',
                              reversescale = T,
                              showscale = TRUE),
                showlegend=FALSE
    ) %>%
    add_lines(data=notifications_by_date_of_notification, x=~date, y=~notifications,
              line=list(width=3), color=I("darkorange"),
              name="Daily\nnotifications", showlegend=TRUE) %>%
    layout(
      xaxis=list(
        title="Date",
        tickfont=f2,
        titlefont=f1,
        range=c(exposure.data.start.date, exposure.data.end.date),
        tickvals = date.labels.exposure.data,
        ticktext = paste0("1 ",format(date.labels.exposure.data, "%b %y")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Daily number of contacts\ndetected by the app"
        #range=c(0,line.height)
      ),
      showlegend=TRUE,
      legend=list(
        font=f1,
        y=0.1
      ),
      bargap=0
    )
    
  plots$p2 <- contacts_and_notifications_coloured_by_risk_plot
  
  # stats to quote / discuss: 
  write_lines(c(
    paste0("Total notifications from 1 April 2021 to 28 Feb 2022:"),
    paste0(signif(sum(notifications_by_date_of_notification$notifications, na.rm=T), 9))
  ),
  file="results/total_notifications_2021-04-01_to_2022-02-28.txt")
  

  # Another supplementary version, by setting
  
  contacts_and_risks_by_date_exposure_and_setting <- grouped_exposure_data %>%
    filter(date_plotted_exposure >= exposure.data.start.date,
           date_plotted_exposure <= exposure.data.end.date) %>% 
    group_by(date_plotted_exposure, setting) %>%
    summarise("total_contacts" = sum(weight, na.rm=T),
              "total_cumRiskScore" = sum(cumRiskScore*weight, na.rm=T)/90
    ) %>%
    mutate("mean_cumRiskScore" = total_cumRiskScore / total_contacts) %>%
    arrange(date_plotted_exposure) %>%
    ungroup() 
  
  # with dummy data we get nonsensical ranges here
  if (dummy.data) {
    household.range <- 0
    recurring.range <- 0
    single.day.range <- 0
    fleeting.range <- 0
  } else {
    # get colour scales for each setting
    household.range <- signif(max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "household"))$mean_cumRiskScore),3) -
      signif(min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "household"))$mean_cumRiskScore),3)
    
    recurring.range <- signif(10*max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "recurring"))$mean_cumRiskScore),3) -
      (signif(10*min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "recurring"))$mean_cumRiskScore),3) + 1)
    
    single.day.range <- signif(10*max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "single day"))$mean_cumRiskScore),2) -
      (signif(10*min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "single day"))$mean_cumRiskScore),2))
    
    fleeting.range <- signif(100*max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "fleeting"))$mean_cumRiskScore),3) -
      signif(100*min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "fleeting"))$mean_cumRiskScore),3)
    
    
  }
  
  
  contacts_coloured_by_risk_by_settings_plot_h <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "household"), 
            colors = viridis(household.range, direction=-1)) %>%
      add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                  marker = list(color = ~signif(mean_cumRiskScore,2),
                                size=0.1,
                                colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                                titlefont=f2, tickfont=f2, 
                                                len=0.8, x=1.03, y=0.5),
                                colorscale = 'Viridis',
                                reversescale = T,
                                showscale = TRUE),
                  showlegend=FALSE
      ) %>%
      add_bars(x=~date_plotted_exposure, y=~total_contacts,
               color=~as.factor(mean_cumRiskScore), colors = viridis(household.range, direction=-1), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-08-15"), y=12000, text="Household",
                      font=f1,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>% 
      layout(
        xaxis=list(
          title="",
          tickfont=f2,
          titlefont=f1,
          range=c(exposure.data.start.date, exposure.data.end.date),
          tickvals = date.labels.exposure.data,
          ticktext = rep("", 11),
          ticks="outside", tickwidth=2, ticklen=10
        ),
        yaxis=list(
          titlefont=f1,
          tickfont=f2,
          title="Daily number of contacts\ndetected by the app"
          #range=c(0,line.height)
        ),
        showlegend=F,
        bargap=0
      )
    
  contacts_coloured_by_risk_by_settings_plot_r <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "recurring"), 
            colors = viridis(recurring.range, direction=-1)) %>%
      add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                  marker = list(color = ~signif(mean_cumRiskScore,2),
                                size=0.1,
                                colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                                titlefont=f2, tickfont=f2, 
                                                len=0.8, x=1.03, y=0.5),
                                colorscale = 'Viridis',
                                reversescale = T,
                                showscale = TRUE),
                  showlegend=FALSE
      ) %>%
      add_bars(x=~date_plotted_exposure, y=~total_contacts,
               color=~as.factor(mean_cumRiskScore), colors = viridis(recurring.range, direction=-1), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-08-15"), y=25000, text="Recurring",
                      font=f1,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>% 
      layout(
        xaxis=list(
          title="",
          tickfont=f2,
          titlefont=f1,
          range=c(exposure.data.start.date, exposure.data.end.date),
          tickvals = date.labels.exposure.data,
          ticktext = rep("", 11),
          ticks="outside", tickwidth=2, ticklen=10
        ),
        yaxis=list(
          titlefont=f1,
          tickfont=f2,
          title="Daily number of contacts\ndetected by the app"
          #range=c(0,line.height)
        ),
        showlegend=F,
        bargap=0
      )
  
  contacts_coloured_by_risk_by_settings_plot_sd <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "single day"), 
            colors = viridis(single.day.range, direction=-1)) %>%
      add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                  marker = list(color = ~signif(mean_cumRiskScore,2),
                                size=0.1,
                                colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                                titlefont=f2, tickfont=f2, 
                                                len=0.8, x=1.03, y=0.5),
                                colorscale = 'Viridis',
                                reversescale = T,
                                showscale = TRUE),
                  showlegend=FALSE
      ) %>%
      add_bars(x=~date_plotted_exposure, y=~total_contacts,
               color=~as.factor(mean_cumRiskScore), colors = viridis(single.day.range, direction=-1), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-08-15"), y=75000, text="Single day",
                      font=f1,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>% 
      layout(
        xaxis=list(
          title="",
          tickfont=f2,
          titlefont=f1,
          range=c(exposure.data.start.date, exposure.data.end.date),
          tickvals = date.labels.exposure.data,
          ticktext = rep("", 11),
          ticks="outside", tickwidth=2, ticklen=10
        ),
        yaxis=list(
          titlefont=f1,
          tickfont=f2,
          title="Daily number of contacts\ndetected by the app"
          #range=c(0,line.height)
        ),
        showlegend=F,
        bargap=0
      )
  
  contacts_coloured_by_risk_by_settings_plot_f <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "fleeting"), 
            colors = viridis(fleeting.range, direction=-1)) %>%
      add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                  marker = list(color = ~signif(mean_cumRiskScore,2),
                                size=0.1,
                                colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                                titlefont=f2, tickfont=f2, 
                                                len=0.8, x=1.03, y=0.5),
                                colorscale = 'Viridis',
                                reversescale = T,
                                showscale = TRUE),
                  showlegend=FALSE
      ) %>%
      add_bars(x=~date_plotted_exposure, y=~total_contacts,
               color=~as.factor(mean_cumRiskScore), colors = viridis(fleeting.range, direction=-1), showlegend=FALSE) %>%
      add_annotations(x=as.Date("2021-08-15"), y=75000, text="Fleeting",
                      font=f1,
                      xref = "x",
                      yref = "y",
                      showarrow = FALSE) %>% 
      layout(
        xaxis=list(
          title="Date of exposure",
          tickfont=f2,
          titlefont=f1,
          range=c(exposure.data.start.date, exposure.data.end.date),
          tickvals = date.labels.exposure.data,
          ticktext = paste0("1 ",format(date.labels.exposure.data, "%b %y")),
          ticks="outside", tickwidth=2, ticklen=10
        ),
        yaxis=list(
          titlefont=f1,
          tickfont=f2,
          title="Daily number of contacts\ndetected by the app"
          #range=c(0,line.height)
        ),
        showlegend=F,
        bargap=0
      )
  
  plots$p3 <- contacts_coloured_by_risk_by_settings_plot_h
  plots$p4 <- contacts_coloured_by_risk_by_settings_plot_r
  plots$p5 <- contacts_coloured_by_risk_by_settings_plot_sd
  plots$p6 <- contacts_coloured_by_risk_by_settings_plot_f
  
  plots
}

get_coloured_by_risk_plots_Euros <- function(grouped_exposure_data, 
                                       exposure.data.start.date, 
                                       exposure.data.end.date, 
                                       national.data.full, 
                                       rho,
                                       dummy.data) {
  
  contacts_and_risks_by_date_exposure <- grouped_exposure_data %>%
    filter(date_plotted_exposure >= exposure.data.start.date,
           date_plotted_exposure <= exposure.data.end.date) %>% 
    group_by(date_plotted_exposure) %>%
    summarise("total_contacts" = sum(weight, na.rm=T),
              "total_cumRiskScore" = sum(cumRiskScore*weight, na.rm=T)/90
    ) %>%
    mutate("mean_cumRiskScore" = total_cumRiskScore / total_contacts) %>%
    arrange(date_plotted_exposure) %>%
    ungroup() 
  
  
  # prepare plots
  plots <- list()
  
  date.labels.exposure.data <- seq.Date(exposure.data.start.date+2, exposure.data.end.date, by="week")
  
  # by setting
  
  contacts_and_risks_by_date_exposure_and_setting <- grouped_exposure_data %>%
    filter(date_plotted_exposure >= exposure.data.start.date,
           date_plotted_exposure <= exposure.data.end.date) %>% 
    group_by(date_plotted_exposure, setting) %>%
    summarise("total_contacts" = sum(weight, na.rm=T),
              "total_cumRiskScore" = sum(cumRiskScore*weight, na.rm=T)/90
    ) %>%
    mutate("mean_cumRiskScore" = total_cumRiskScore / total_contacts) %>%
    arrange(date_plotted_exposure) %>%
    ungroup() 
  
  # get colour scales for each setting
  # with dummy data we get nonsensical ranges here
  if (dummy.data) {
    household.range <- 0
    recurring.range <- 0
    single.day.range <- 0
    fleeting.range <- 0
  } else {
    household.range <- signif(max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "household"))$mean_cumRiskScore),3) -
      signif(min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "household"))$mean_cumRiskScore),3)
    
    recurring.range <- signif(10*max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "recurring"))$mean_cumRiskScore),3) -
      (signif(10*min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "recurring"))$mean_cumRiskScore),3) + 1)
    
    single.day.range <- signif(10*max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "single day"))$mean_cumRiskScore),2) -
      (signif(10*min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "single day"))$mean_cumRiskScore),2))
    
    fleeting.range <- signif(100*max((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "fleeting"))$mean_cumRiskScore),3) -
      signif(100*min((contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "fleeting"))$mean_cumRiskScore),3)
    
    
  }
  
  contacts_coloured_by_risk_by_settings_plot_h <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "household"), 
                                                          colors = viridis(household.range, direction=-1)) %>%
    add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                marker = list(color = ~signif(mean_cumRiskScore,2),
                              size=0.1,
                              colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                              titlefont=f2, tickfont=f2, 
                                              len=0.8, x=1.03, y=0.5),
                              colorscale = 'Viridis',
                              reversescale = T,
                              showscale = TRUE),
                showlegend=FALSE
    ) %>%
    add_bars(x=~date_plotted_exposure, y=~total_contacts,
             color=~as.factor(mean_cumRiskScore), colors = viridis(household.range, direction=-1), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-28"), y=10000, text="Household",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        title="",
        tickfont=f2,
        titlefont=f1,
        range=c(exposure.data.start.date, exposure.data.end.date),
        tickvals = date.labels.exposure.data,
        ticktext = format(date.labels.exposure.data, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Daily number of contacts\ndetected by the app"
        #range=c(0,line.height)
      ),
      showlegend=F,
      bargap=0
    )
  
  contacts_coloured_by_risk_by_settings_plot_r <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "recurring"), 
                                                          colors = viridis(recurring.range, direction=-1)) %>%
    add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                marker = list(color = ~signif(mean_cumRiskScore,2),
                              size=0.1,
                              colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                              titlefont=f2, tickfont=f2, 
                                              len=0.8, x=1.03, y=0.5),
                              colorscale = 'Viridis',
                              reversescale = T,
                              showscale = TRUE),
                showlegend=FALSE
    ) %>%
    add_bars(x=~date_plotted_exposure, y=~total_contacts,
             color=~as.factor(mean_cumRiskScore), colors = viridis(recurring.range, direction=-1), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-28"), y=25000, text="Recurring",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        title="",
        tickfont=f2,
        titlefont=f1,
        range=c(exposure.data.start.date, exposure.data.end.date),
        tickvals = date.labels.exposure.data,
        ticktext = format(date.labels.exposure.data, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Daily number of contacts\ndetected by the app"
        #range=c(0,line.height)
      ),
      showlegend=F,
      bargap=0
    )
  
  contacts_coloured_by_risk_by_settings_plot_sd <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "single day"), 
                                                           colors = viridis(single.day.range, direction=-1)) %>%
    add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                marker = list(color = ~signif(mean_cumRiskScore,2),
                              size=0.1,
                              colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                              titlefont=f2, tickfont=f2, 
                                              len=0.8, x=1.03, y=0.5),
                              colorscale = 'Viridis',
                              reversescale = T,
                              showscale = TRUE),
                showlegend=FALSE
    ) %>%
    add_bars(x=~date_plotted_exposure, y=~total_contacts,
             color=~as.factor(mean_cumRiskScore), colors = viridis(single.day.range, direction=-1), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-28"), y=75000, text="Single day",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        title="",
        tickfont=f2,
        titlefont=f1,
        range=c(exposure.data.start.date, exposure.data.end.date),
        tickvals = date.labels.exposure.data,
        ticktext = format(date.labels.exposure.data, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Daily number of contacts\ndetected by the app"
        #range=c(0,line.height)
      ),
      showlegend=F,
      bargap=0
    )
  
  contacts_coloured_by_risk_by_settings_plot_f <- plot_ly(contacts_and_risks_by_date_exposure_and_setting %>% filter(setting == "fleeting"), 
                                                          colors = viridis(fleeting.range, direction=-1)) %>%
    add_markers(x=~date_plotted_exposure, y=~total_contacts, # adding markers as a workaround, so as to show a legend (couldn't get it to work with the bars directly)
                marker = list(color = ~signif(mean_cumRiskScore,2),
                              size=0.1,
                              colorbar = list(title = "Mean\ncumulative\nrisk score\n \n", 
                                              titlefont=f2, tickfont=f2, 
                                              len=0.8, x=1.03, y=0.5),
                              colorscale = 'Viridis',
                              reversescale = T,
                              showscale = TRUE),
                showlegend=FALSE
    ) %>%
    add_bars(x=~date_plotted_exposure, y=~total_contacts,
             color=~as.factor(mean_cumRiskScore), colors = viridis(fleeting.range, direction=-1), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-28"), y=75000, text="Fleeting",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        title="Date of exposure",
        tickfont=f2,
        titlefont=f1,
        range=c(exposure.data.start.date, exposure.data.end.date),
        tickvals = date.labels.exposure.data,
        ticktext = format(date.labels.exposure.data, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Daily number of contacts\ndetected by the app"
        #range=c(0,line.height)
      ),
      showlegend=F,
      bargap=0
    )
  
  plots$p1 <- contacts_coloured_by_risk_by_settings_plot_h
  plots$p2 <- contacts_coloured_by_risk_by_settings_plot_r
  plots$p3 <- contacts_coloured_by_risk_by_settings_plot_sd
  plots$p4 <- contacts_coloured_by_risk_by_settings_plot_f
  
  plots
}