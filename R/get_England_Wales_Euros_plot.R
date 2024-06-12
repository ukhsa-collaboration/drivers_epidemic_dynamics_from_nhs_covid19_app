get_England_Wales_Euros_plot <- function(grouped_exposure_data, rho){
  
  marker.size = 12
  
  England.color.dark <- "#000040"
  England.color.main <- "#2B57AC"
  England.color.light <- "#4E84FF"
  
  Wales.color.dark <- "#AE2630"
  Wales.color.main <- "#CF1E26"
  Wales.color.light <- "#FF4646"
  
  annotation.line.width <- 2
  line.width <- 4
  
  # Separate by "England" v "Wales"
  
  table_outcome_riskScores_for_plotting_Euros <- grouped_exposure_data %>%
    mutate("ew" = str_sub(ltla,1,1)) %>%
    filter(date_plotted_exposure <= as.Date("2021-07-21") + 7 & date_plotted_exposure >= as.Date("2021-06-01") - 7) %>%
    group_by(date_plotted_exposure, ew) %>%
    summarise("contacts" = sum(weight, na.rm=T),
              "transmissions" = sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight),
              "TPAEN" = transmissions/contacts) %>%
    mutate("weekday" = factor(week.days[lubridate::wday(date_plotted_exposure,week_start = 1)],levels=week.days)) %>%
    arrange(date_plotted_exposure) %>%
    ungroup()
  
  table_outcome_riskScores_for_plotting_Euros_Wales <- table_outcome_riskScores_for_plotting_Euros %>%
    filter(ew =="W") %>%
    filter(date_plotted_exposure <= as.Date("2021-07-21") & date_plotted_exposure >= as.Date("2021-06-01"))
  
  table_outcome_riskScores_for_plotting_Euros_England <- table_outcome_riskScores_for_plotting_Euros %>%
    filter(ew =="E") %>%
    filter(date_plotted_exposure <= as.Date("2021-07-21") & date_plotted_exposure >= as.Date("2021-06-01"))
  
  # frollapply(x=table_outcome_riskScores_for_plotting_Euros_Wales$transmissions, n=15, FUN=median, fill=NA, align="center")
  
  # compute and save some key stats
  table_tournament_only <- table_outcome_riskScores_for_plotting_Euros %>% 
    filter(date_plotted_exposure <= as.Date("2021-07-11") & date_plotted_exposure >= as.Date("2021-06-11")) %>%
    group_by(date_plotted_exposure, weekday) %>%
    summarise("transmissions" = sum(transmissions, na.rm=T)) %>%
    ungroup()
  
  table_total_transmissions_with_baseline <- table_outcome_riskScores_for_plotting_Euros %>% 
    group_by(date_plotted_exposure, weekday) %>%
    summarise("transmissions" = sum(transmissions, na.rm=T)) %>%
    ungroup() %>%
    mutate("baseline" = frollapply(x=transmissions, n=15, FUN=median, fill=NA, align="center"),
           "transmissions_compared_to_baseline" = transmissions / baseline * 100) %>%
    filter(date_plotted_exposure <= as.Date("2021-07-21") & date_plotted_exposure >= as.Date("2021-06-01"))
  
  dates_Wales_matches <- as.Date(paste("2021-06-",c("12","16","20","26")))
  dates_England_matches <- as.Date(c(paste("2021-06-",c("13","18","22","29")),paste("2021-07-",c("03","07","11"))))
  rows_for_dates_of_England_matches <- which(table_total_transmissions_with_baseline$date_plotted_exposure %in% dates_England_matches)
  
  table_total_transmissions_England_by_type <- grouped_exposure_data %>%
    mutate("ew" = str_sub(ltla,1,1)) %>% 
    filter(ew=="E") %>%
    filter(date_plotted_exposure <= as.Date("2021-07-11") & date_plotted_exposure >= as.Date("2021-06-11")) %>%
    group_by(date_plotted_exposure, classification) %>%
    summarise("contacts" = sum(weight, na.rm=T),
              "transmissions" = sum((positive-(1-(1-bg_rate_cases_app)^(rho*(classification=='fleeting'))))*weight),
              "TPAEN" = transmissions/contacts) %>%
    ungroup() %>%
    mutate(final=(date_plotted_exposure==as.Date("2022-07-11")),matches=(date_plotted_exposure %in% dates_England_matches))
  
  write_lines(c(
    paste0("Total transmission events:"),
    paste0(sum(table_total_transmissions_with_baseline$transmissions, na.rm=T)),
    paste0("Total transmission events for English postcode districts:"),
    paste0(sum(table_outcome_riskScores_for_plotting_Euros_England$transmissions, na.rm=T)),
    paste0("Total transmission events for Welsh postcode districts:"),
    paste0(sum(table_outcome_riskScores_for_plotting_Euros_Wales$transmissions, na.rm=T)),
    paste0("Welsh percent of total:"),
    paste0(sum(table_outcome_riskScores_for_plotting_Euros_Wales$transmissions, na.rm=T)/sum(table_total_transmissions_with_baseline$transmissions, na.rm=T) * 100),
    paste0("Total during tournament:"),
    paste0(sum(table_tournament_only$transmissions, na.rm=T)),
    paste0("Total on final day:"),
    paste0(table_tournament_only[31,]$transmissions),
    paste0("Percent on final day:"),
    paste0(table_tournament_only[31,]$transmissions / sum(table_tournament_only$transmissions, na.rm=T) * 100),
    paste0("Final day percent greater than previous Sunday:"),
    paste0(table_tournament_only[31,]$transmissions / sum((table_outcome_riskScores_for_plotting_Euros %>% filter(date_plotted_exposure == as.Date("2021-07-11") - 7))$transmissions, na.rm=T) * 100),
    paste0("Final day percent greater than following Sunday:"),
    paste0(table_tournament_only[31,]$transmissions / sum((table_outcome_riskScores_for_plotting_Euros %>% filter(date_plotted_exposure == as.Date("2021-07-11") + 7))$transmissions, na.rm=T) * 100),
    paste0("Times higher transmission events on days with England matches:"),
    paste0(min(table_total_transmissions_with_baseline$transmissions_compared_to_baseline[rows_for_dates_of_England_matches]), " to ", max(table_total_transmissions_with_baseline$transmissions_compared_to_baseline[rows_for_dates_of_England_matches]) ),
    paste0("Times higher transmission events on days with England matches - mean:"),
    paste0(mean(table_total_transmissions_with_baseline$transmissions_compared_to_baseline[rows_for_dates_of_England_matches], na.rm=T)),
    paste0("Match day transmissions over baseline, as a percent of total transmissions:"),
    paste0(sum((table_total_transmissions_with_baseline$transmissions - table_total_transmissions_with_baseline$baseline)[rows_for_dates_of_England_matches]) / sum(table_tournament_only$transmissions, na.rm=T) * 100),
    paste0("Final match day transmissions over baseline, as a percent of total transmissions:"),
    paste0((table_total_transmissions_with_baseline$transmissions - table_total_transmissions_with_baseline$baseline)[[which(table_total_transmissions_with_baseline$date_plotted_exposure == as.Date("2021-07-11"))]] / sum(table_tournament_only$transmissions, na.rm=T) * 100),
    paste0("fractions of fleeting transmissions for England on a non-match day"),
    paste0(table_total_transmissions_England_by_type %>% filter(!matches) %>% summarise(n=sum(transmissions[classification=="fleeting"],na.rm=T)/sum(transmissions,na.rm=T)) %>% pull(n)),
    paste0("fractions of fleeting transmissions for England on a match day"),
    paste0(table_total_transmissions_England_by_type %>% filter(matches) %>% summarise(n=sum(transmissions[classification=="fleeting"],na.rm=T)/sum(transmissions,na.rm=T)) %>% pull(n)),
    paste0("fractions of one-day transmissions for England on a non-match day"),
    paste0(table_total_transmissions_England_by_type %>% filter(!matches) %>% summarise(n=sum(transmissions[classification=="one-day"],na.rm=T)/sum(transmissions,na.rm=T)) %>% pull(n)),
    paste0("fractions of one-day transmissions for England on a match day"),
    paste0(table_total_transmissions_England_by_type %>% filter(matches) %>% summarise(n=sum(transmissions[classification=="one-day"],na.rm=T)/sum(transmissions,na.rm=T)) %>% pull(n))
  ),
  file="results/Euros.txt")
  
  line.height <- 10500
  match.line.height <- 8500
  # date range to plot goes one day either side of the data, to allow markers to show fully:
  first.date.to.plot.eng.v.wales <- as.Date("2021-05-31")
  last.date.to.plot.eng.v.wales <- as.Date("2021-07-22")
  date.to.annotate.panel.titles <- as.Date("2021-06-05")
  date.labels <- seq.Date(first.date.to.plot.eng.v.wales + 1, last.date.to.plot.eng.v.wales, by=2)
  
  plot_England <- table_outcome_riskScores_for_plotting_Euros_England %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=line.height*0.85, text="ENG v",
                    font=list(
                      family = "Arial, sans-serif",
                      size = 22,
                      color = England.color.main
                    ),
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_lines(x=as.Date("2021-07-11"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-07-11")),]$transmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-11"), y=line.height*0.85, text="ITA",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-07"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-07-07")),]$transmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-07"), y=line.height*0.85, text="DEN",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-03"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-07-03")),]$transmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-03"), y=line.height*0.85, text="UKR",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-29"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-06-29")),]$transmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-29"), y=line.height*0.85, text="GER",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-22"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-06-22")),]$transmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-22"), y=line.height*0.85, text="CZE",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-18"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-06-18")),]$transmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-18"), y=line.height*0.85, text="SCO",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-13"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_England[which(table_outcome_riskScores_for_plotting_Euros_England$date_plotted_exposure == as.Date("2021-06-13")),]$transmissions,match.line.height),  
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-13"), y=line.height*0.85, text="CRO",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, 
              showlegend=FALSE,
              #name="England", legendgroup="country", 
              color=I(England.color.main), line=list(width=line.width)) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup="weekday",
              marker=list(size=marker.size)) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.eng.v.wales, last.date.to.plot.eng.v.wales),
        tickfont=f2,
        titlefont=f1,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = format(date.labels, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        range=c(0,line.height),
        title="Daily number of transmission events\ndetected by the app (England)"
      ),
      legend=list(
        font=f2
      )
    )
  plot_England
  
  line.height <- 86
  match.line.height <- 70
  
  plot_Wales <- table_outcome_riskScores_for_plotting_Euros_Wales %>%
    plot_ly(., colors=weekday.cols) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=line.height*0.85, text="WAL v",
                    font=list(
                      family = "Arial, sans-serif",
                      size = 22,
                      color = Wales.color.main
                    ),
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_lines(x=as.Date("2021-06-26"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_Wales[which(table_outcome_riskScores_for_plotting_Euros_Wales$date_plotted_exposure == as.Date("2021-06-26")),]$transmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-26"), y=line.height*0.85, text="DEN",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-20"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_Wales[which(table_outcome_riskScores_for_plotting_Euros_Wales$date_plotted_exposure == as.Date("2021-06-20")),]$transmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-20"), y=line.height*0.85, text="ITA",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-16"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_Wales[which(table_outcome_riskScores_for_plotting_Euros_Wales$date_plotted_exposure == as.Date("2021-06-16")),]$transmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-16"), y=line.height*0.85, text="TUR",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-12"), 
              y=c(table_outcome_riskScores_for_plotting_Euros_Wales[which(table_outcome_riskScores_for_plotting_Euros_Wales$date_plotted_exposure == as.Date("2021-06-12")),]$transmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-12"), y=line.height*0.85, text="SUI",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=~transmissions, 
              showlegend=FALSE,
              #name="Wales", legendgroup="country", 
              color=I(Wales.color.main), line=list(width=line.width)) %>%
    add_trace(x=~date_plotted_exposure, y=~transmissions, 
              type='scatter', mode='markers',
              color=~weekday, legendgroup="weekday", showlegend=FALSE,
              marker=list(size=marker.size)) %>% 
    layout(
      xaxis=list(
        range=c(first.date.to.plot.eng.v.wales, last.date.to.plot.eng.v.wales),
        tickfont=f2,
        titlefont=f1,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = format(date.labels, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        range=c(0,line.height),
        title="Daily number of transmission events\ndetected by the app (Wales)"
      )
    )
  
  eng.v.wales.transmission.event.counts.plot <- subplot(plot_England, plot_Wales, 
                                                     nrows=2, shareX=TRUE, titleX=TRUE, titleY=TRUE)  %>%
    layout(legend = list(y=0.5, tracegroupgap = 100), margin=0)
  
  
  # Detrended values plots
  
  dates_to_remove_for_England <- c(dates_England_matches, dates_Wales_matches)
  
  subeuros_England <- table_outcome_riskScores_for_plotting_Euros_England %>% 
    ungroup() %>%
    mutate("logTPAEN" = log(TPAEN),
           "logCT" = log(contacts),
           "logInf" = log(transmissions),
           "numdate" = diff_days(date_plotted_exposure,"2021-06-11"))

  subTPAENlm_England <- lm(logTPAEN ~ numdate + weekday,
                            data = subeuros_England %>% 
                             filter(!(date_plotted_exposure %in% dates_to_remove_for_England))
                           )
  subCTlm_England <- lm(logCT ~ numdate + weekday,
                        data = subeuros_England %>% 
                        filter(!(date_plotted_exposure %in% dates_to_remove_for_England))
                        )
  subInflm_England <- lm(logInf ~ numdate + weekday,
                         data = subeuros_England %>% filter(!(date_plotted_exposure %in% dates_to_remove_for_England))
                         )

  subeuros_England_with_trends_and_residuals <- bind_cols(subeuros_England,
                                                          tibble("trendTPAEN" = predict(subTPAENlm_England, subeuros_England),
                                                                 "trendCT" = predict(subCTlm_England, subeuros_England),
                                                                 "trendInf" = predict(subInflm_England, subeuros_England))
                                                          ) %>%
    mutate("resTPAEN" = exp(logTPAEN-trendTPAEN),
           "resContacts" = exp(logCT-trendCT),
           "resTransmissions" = exp(logInf-trendInf)
           )

  
  line.height <- 11
  match.line.height <- 7.7
  annotation.height <- 8.5
  
  plot_England_residuals <- plot_ly(subeuros_England_with_trends_and_residuals)  %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=log10(annotation.height), text="ENG v",
                    font=list(
                      family = "Arial, sans-serif",
                      size = 22,
                      color = England.color.main
                    ),
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_lines(x=as.Date("2021-07-11"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-07-11")),]$resTransmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-11"), y=log10(annotation.height), text="ITA",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-07"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-07-07")),]$resTransmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-07"), y=log10(annotation.height), text="DEN",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-03"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-07-03")),]$resTransmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-03"), y=log10(annotation.height), text="UKR",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-29"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-29")),]$resTransmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-29"), y=log10(annotation.height), text="GER",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-22"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-22")),]$resTransmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-22"), y=log10(annotation.height), text="CZE",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-18"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-18")),]$resTransmissions,match.line.height), 
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-18"), y=log10(annotation.height), text="SCO",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-13"), 
              y=c(subeuros_England_with_trends_and_residuals[which(subeuros_England_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-13")),]$resTransmissions,match.line.height),  
              color=I(England.color.main), 
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-13"), y=log10(annotation.height), text="CRO",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=1,
              showlegend=FALSE, line=list(width=2), color=I("darkgrey")) %>%
    add_lines(x=~date_plotted_exposure, y=~resTransmissions,
              name="Transmission\nevents", line=list(width=line.width), color=I(England.color.main),
              legendgroup="Englishresiduals") %>%
    add_lines(x=~date_plotted_exposure, y=~resContacts,
              name="Contacts", line=list(width=line.width, dash="dash"), color=I(England.color.dark),
              legendgroup="Englishresiduals") %>% 
    add_lines(x=~date_plotted_exposure, y=~resTPAEN,
              name="\nProbability\nof reported\ntransmission", 
              line=list(width=line.width, dash="dot"), color=I(England.color.light),
              legendgroup="Englishresiduals") %>%
    layout(
      xaxis=list(
        range=c(first.date.to.plot.eng.v.wales, last.date.to.plot.eng.v.wales),
        tickfont=f2,
        titlefont=f1,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = format(date.labels, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        title="Detrended values relative to\nstart of tournament (England)",
        type="log",
        range=c(-0.4,log10(line.height + 0.5)),
        tickvals=c(0.5,1,2,4,8)
      ),
      legend=list(
        font=f2
      )
    )
  
  plot_England_residuals
  
  # Wales
  
  dates_to_remove_for_Wales <- c(dates_England_matches, dates_Wales_matches)
  
  
  subeuros_Wales <- table_outcome_riskScores_for_plotting_Euros_Wales %>% 
    ungroup() %>%
    mutate("logTPAEN" = log(TPAEN),
           "logCT" = log(contacts),
           "logInf" = log(transmissions),
           "numdate" = diff_days(date_plotted_exposure,"2021-06-11"))
  
  subTPAENlm_Wales <- lm(logTPAEN ~ numdate + weekday,
                           data = subeuros_Wales %>% 
                             filter(!(date_plotted_exposure %in% dates_to_remove_for_Wales))
  )
  subCTlm_Wales <- lm(logCT ~ numdate + weekday,
                        data = subeuros_Wales %>% 
                          filter(!(date_plotted_exposure %in% dates_to_remove_for_Wales))
  )
  subInflm_Wales <- lm(logInf ~ numdate + weekday,
                         data = subeuros_Wales %>% filter(!(date_plotted_exposure %in% dates_to_remove_for_Wales))
  )
  
  subeuros_Wales_with_trends_and_residuals <- bind_cols(subeuros_Wales,
                                                          tibble("trendTPAEN" = predict(subTPAENlm_Wales, subeuros_Wales),
                                                                 "trendCT" = predict(subCTlm_Wales, subeuros_Wales),
                                                                 "trendInf" = predict(subInflm_Wales, subeuros_Wales))
  ) %>%
    mutate("resTPAEN" = exp(logTPAEN-trendTPAEN),
           "resContacts" = exp(logCT-trendCT),
           "resTransmissions" = exp(logInf-trendInf)
    )
  
  # 
  # line.height <- 5
  # match.line.height <- 3.9
  line.height <- 9
  match.line.height <- 4.9
  annotation.height <- 5.5
  
  
  plot_Wales_residuals <- plot_ly(subeuros_Wales_with_trends_and_residuals)  %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                     as.Date("2021-07-11"), as.Date("2021-07-11")),
                 y=c(0,line.height, line.height, 0),
                 color=I("lightgrey"), showlegend=FALSE) %>%
    add_annotations(x=date.to.annotate.panel.titles, y=log10(annotation.height), text="WAL v",
                    font=list(
                      family = "Arial, sans-serif",
                      size = 22,
                      color = Wales.color.main
                    ),
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_lines(x=as.Date("2021-06-26"), 
              y=c(subeuros_Wales_with_trends_and_residuals[which(subeuros_Wales_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-26")),]$resTransmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-26"), y=log10(annotation.height), text="DEN",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-20"), 
              y=c(subeuros_Wales_with_trends_and_residuals[which(subeuros_Wales_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-20")),]$resTransmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-20"), y=log10(annotation.height), text="ITA",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-16"), 
              y=c(subeuros_Wales_with_trends_and_residuals[which(subeuros_Wales_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-16")),]$resTransmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-16"), y=log10(annotation.height), text="TUR",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-06-12"), 
              y=c(subeuros_Wales_with_trends_and_residuals[which(subeuros_Wales_with_trends_and_residuals$date_plotted_exposure == as.Date("2021-06-12")),]$resTransmissions,match.line.height), 
              color=I(Wales.color.main),
              line=list(width=annotation.line.width, dash="dot"), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-06-12"), y=log10(annotation.height), text="SUI",
                    font=f2,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=~date_plotted_exposure, y=1,
              showlegend=FALSE, line=list(width=2), color=I("darkgrey")) %>%
    add_lines(x=~date_plotted_exposure, y=~resTransmissions,
              name="Transmission\nevents", line=list(width=line.width), color=I(Wales.color.main),
              legendgroup="Welshresiduals") %>%
    add_lines(x=~date_plotted_exposure, y=~resContacts,
              name="Contact\nevents", line=list(width=line.width, dash="dash"), color=I(Wales.color.dark),
              legendgroup="Welshresiduals") %>% 
    add_lines(x=~date_plotted_exposure, y=~resTPAEN,
              name="\nProbability\nof reported\ntransmission", 
              line=list(width=line.width, dash="dot"), color=I(Wales.color.light),
              legendgroup="Welshresiduals") %>%
    layout(
      xaxis=list(
        range=c(first.date.to.plot.eng.v.wales, last.date.to.plot.eng.v.wales),
        tickfont=f2,
        titlefont=f1,
        title="Date of exposure",
        tickvals = date.labels,
        ticktext = format(date.labels, "%d %b")
        #ticks="outside", 
        #tickwidth=2, ticklen=30
      ),
      yaxis=list(
        tickfont=f2,
        titlefont=f2,
        title="Detrended values relative to\nstart of tournament (Wales)",
        type="log",
        range=c(-0.8,log10(line.height + 0.5)),
        tickvals=c(0.25,0.5,1,2,4,8)
      ),
      legend=list(
        font=f2
      )
    )
  
  eng.v.wales.residuals.plot <- subplot(plot_England_residuals, plot_Wales_residuals, 
                                        nrows=2, shareX=TRUE, titleX=TRUE, shareY=TRUE)  %>%
    layout(legend = list(tracegroupgap = 340), margin=0)
  

  
  plots <- list()
  plots$p1 <- eng.v.wales.transmission.event.counts.plot
  plots$p2 <- eng.v.wales.residuals.plot
  plots$p3 <- subplot(plot_England_residuals,
                      plot_England,
                      plot_Wales,
                      plot_Wales_residuals,
                      nrows=4, shareX=TRUE, titleX=TRUE, titleY=TRUE)  %>%
    layout(legend = list(tracegroupgap = 270))

  
  plots
}