get_national_plot <- function(national.data.full, first.date.to.plot, last.date.to.plot) {
  
  write_csv(national.data.full %>%  
    mutate(date_notification=date,R_app=CRxTPAEN,lower.R_app=lower.CRxTPAEN,upper.R_app=upper.CRxTPAEN) %>%
    select(date_notification,contact_rate,lower.contact.rate,upper.contact.rate,TPAEN,lower.TPAEN,upper.TPAEN,R_app,lower.R_app,upper.R_app),
    file="results/SupplementaryTable3.csv")
  
  date.labels <- seq.Date(as.Date("2020-12-01"), as.Date("2023-04-01"), by="month")
  
  national.data.dec.2021.jan.2022 <- national.data.full %>%
    filter(date >= as.Date("2021-12-01"), date <= as.Date("2022-01-11"))
  
  national.data.dec.2022.jan.2023 <- national.data.full %>%
    filter(date >= as.Date("2022-12-01"), date <= as.Date("2023-01-11"))
  
  # checking total positive tests over the two Christmas periods 
  sum(national.data.dec.2021.jan.2022$test_positive, na.rm=T)
  sum(national.data.dec.2022.jan.2023$test_positive, na.rm=T)
  
  # # compute and save some key stats. Note I have hard-coded "[1:822]" to discard the last few "NA" entries
  # write_lines(c(
  #   paste0("Fraction of variance of Rt explained by changes in contact rates:"),
  #   paste0(cor( log(national.data.full$CRxTPAEN)[1:822], log(national.data.full$contact_rate)[1:822] )^2),
  #   paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
  #   paste0(cor( log(national.data.full$CRxTPAEN)[1:822], log(national.data.full$TPAEN)[1:822] )^2),
  #   paste0("Relation between TPAEN and contact rates:"),
  #   paste0(cor( log(national.data.full$TPAEN)[1:822], log(national.data.full$contact_rate)[1:822] )^2)
  # ),
  # file="results/contributions_to_Rt_overall.txt")
  # 
  # write_lines(c(
  #   paste0("Fraction of variance of Rt explained by changes in contact rates:"),
  #   paste0(var(log(national.data.full$contact_rate)[1:822]) / var(log(national.data.full$CRxTPAEN)[1:822]) ),
  #   paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
  #   paste0(var(log(national.data.full$TPAEN)[1:822]) / var(log(national.data.full$CRxTPAEN)[1:822]) ),
  #   paste0("Relation between TPAEN and contact rates:"),
  #   paste0(cov(log(national.data.full$TPAEN)[1:822], log(national.data.full$contact_rate)[1:822]) / var(log(national.data.full$CRxTPAEN)[1:822]))
  # ),
  # file="results/contributions_to_Rt_overall_2.txt")
  # 
  # national.data.may.to.july <- national.data.full %>%
  #   filter(date >= as.Date("2021-05-01"),
  #          date <= as.Date("2021-07-31"))
  # 
  # write_lines(c(
  #   paste0("May to July 2021:"),
  #   paste0("Fraction of variance of Rt explained by changes in contact rates:"),
  #   paste0(cor( log(national.data.may.to.july$CRxTPAEN), log(national.data.may.to.july$contact_rate) )^2),
  #   paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
  #   paste0(cor( log(national.data.may.to.july$CRxTPAEN), log(national.data.may.to.july$TPAEN) )^2),
  #   paste0("Relation between TPAEN and contact rates:"),
  #   paste0(cor( log(national.data.may.to.july$TPAEN), log(national.data.may.to.july$contact_rate) )^2)
  # ),
  # file=glue("results/contributions_to_Rt_May_to_July_2021.txt"))
  # 
  # national.data.dec <- national.data.full %>%
  #   filter(date >= as.Date("2021-12-01"),
  #          date <= as.Date("2021-12-31"))
  # 
  # write_lines(c(
  #   paste0("December 2021:"),
  #   paste0("Fraction of variance of Rt explained by changes in contact rates:"),
  #   paste0(cor( log(national.data.dec$CRxTPAEN), log(national.data.dec$contact_rate) )^2),
  #   paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
  #   paste0(cor( log(national.data.dec$CRxTPAEN), log(national.data.dec$TPAEN) )^2),
  #   paste0("Relation between TPAEN and contact rates:"),
  #   paste0(cor( log(national.data.dec$TPAEN), log(national.data.dec$contact_rate) )^2)
  # ),
  # file=glue("results/contributions_to_Rt_December_2021.txt"))
  
  line.height <- 82
  CR <- plot_ly(national.data.full) %>%  
    filter(date <= last.date.to.plot) %>%
    add_lines(x=as.Date("2022-12-06"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-12-06"), y=line.height*0.9, text="Version 5",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_lines(x=as.Date("2022-04-01"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-04-01"), y=line.height*0.9, text="End of\nfree testing",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-02-24"), y=line.height*0.7, text="End of\nlegal\nrestrictions",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-01-27"), y=line.height*0.9, text="End of\nplan B",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2022-01-11"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2022-01-11"), y=line.height*0.7, text="LFD role\nchanges",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-11-27"), y=line.height*0.8, text="First\nmeasures\nagainst\nOmicron",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-08-16"), y=line.height*0.7, text="Advice\nchange",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-08-02"), y=line.height*0.9, text="Logic\nchange",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-07-19"), y=line.height*0.7, text="Step 4",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-05-17"), y=line.height*0.9, text="Step 3",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-04-12"), y=line.height*0.7, text="Step 2",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-03-29"), y=line.height*0.9, text="Step 1b",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_annotations(x=as.Date("2021-03-08"), y=line.height*0.7, text="Step 1a",
                    font=f3,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    add_lines(x=~date, y=~contact_rate, name="", color=I("#66c2a5"), 
              showlegend=FALSE, line=list(width=4, opacity=1)) %>%
    add_ribbons(x=~date, ymin=~lower.contact.rate, ymax=~upper.contact.rate, color=I("#66c2a5"), opacity=0.6, showlegend=FALSE)  %>% 
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Contact rate of test-positive\napp users CR(t)",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      ),
      legend=list(
        font=f1
      )
    )
  
  
  
  line.height <- 12
  TPAEN <- plot_ly(national.data.full) %>%  
    filter(date <= last.date.to.plot) %>%
    add_lines(x=as.Date("2022-12-06"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-04-01"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-01-11"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=~date, y=~TPAEN*100, name="", color=I("#fc8d62"), 
              showlegend=FALSE, line=list(width=4, opacity=1)) %>%
    add_ribbons(x=~date, ymin=~lower.TPAEN*100, ymax=~upper.TPAEN*100, color=I("#fc8d62"), opacity=0.6, showlegend=FALSE)  %>% 
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="% of users reporting testing positive\n after exposure notification, TPAEN(t)",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      )
    )
 
  
  line.height <- 3.8
  CRxTPAEN <- plot_ly(national.data.full) %>%
    filter(date <= last.date.to.plot) %>%
    add_lines(x=as.Date("2022-12-06"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-04-01"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-01-11"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    #add_lines(x=c(first.date.to.plot, last.date.to.plot), y=0.6, line=list(dash="dot", width=4, color="black"), showlegend=FALSE) %>%
    add_lines(x=~date, y=~CRxTPAEN, color=I("#8da0cb"), 
              showlegend=FALSE, line=list(width=4, opacity=1), name="") %>%
    add_ribbons(x=~date, ymin=~lower.CRxTPAEN, ymax=~upper.CRxTPAEN, color=I("#8da0cb"), opacity=0.6, showlegend=FALSE)  %>% 
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="App-based indicator \nR<sub>app</sub>(t) = CR(t) x TPAEN(t)",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      )
    )
  
  change_scale_relative <- function(x){
    exp(x)-1
  }
  inverse_change_scale_relative <- function(x){
    log(1+x)
  }
  
  line.height <- 0.6*2.3
  neg.line.height <- -0.5*2.3
  R_decomp <- plot_ly(national.data.full %>%
                        filter(date <= last.date.to.plot)) %>%
    add_lines(x=as.Date("2022-12-06"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-04-01"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-02-24"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-01-27"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2022-01-11"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-11-27"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-16"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-08-02"), y=c(-line.height,line.height), color=I("darkgrey"),
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-07-19"), y=c(-line.height,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-05-17"), y=c(-line.height,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-04-12"), y=c(-line.height,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-29"), y=c(-line.height,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-03-08"), y=c(-line.height,line.height), color=I("darkgrey"), 
              line=list(width=3), showlegend=FALSE) %>%
    add_bars(x=~date, y=~log(contact_rate/median(contact_rate, na.rm=T)), color=I("#66c2a5"), opacity=0.8,
             name="CR(t)") %>%
    add_bars(x=~date, y=~log(TPAEN/median(TPAEN, na.rm=T)), color=I("#fc8d62"), opacity=0.8,
             name="TPAEN(t)") %>%
    add_lines(x=~date, y=~log(contact_rate/median(contact_rate, na.rm=T))+log(TPAEN/median(TPAEN, na.rm=T)), color=I("#8da0cb"),
              line=list(width=2, opacity=1), showlegend=FALSE) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = paste0("1 ", format(date.labels, "%b %y")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Relative change in R<sub>app</sub>(t)\nwith respect to reference",
        titlefont=f4,
        tickfont=f1,
        range=c(neg.line.height,line.height),
        tickvals = inverse_change_scale_relative(c(-0.5,-0.25,0,0.5,1,2)),
        ticktext = c("-50%","-25%","0%","+50%","+100%","+200%"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      legend=list(
        title=list(text="Contribution", font=f1),
        font=f1
      ),
      #barmode="stack",
      bargap = 0
    )
  
  R_decomp
  
  # version for short paper
  
  line.height <- 2.7#3.3
  R_short <- plot_ly(national.data.full) %>%
    filter(date <= last.date.to.plot) %>%
    # add_lines(x=as.Date("2022-12-06"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-04-01"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-01-11"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-08-16"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-08-02"), y=c(0,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    #add_lines(x=c(first.date.to.plot, last.date.to.plot), y=0.6, line=list(dash="dot", width=4, color="black"), showlegend=FALSE) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                   as.Date("2021-07-11"), as.Date("2021-07-11")),
               y=c(0,line.height, line.height, 0),
               color=I("grey90"), line=list(width=0), showlegend=FALSE) %>%
    add_lines(x=~date, y=~CRxTPAEN, color=I("#8da0cb"), 
              showlegend=FALSE, line=list(width=4, opacity=1), name="") %>%
    add_ribbons(x=~date, ymin=~lower.CRxTPAEN, ymax=~upper.CRxTPAEN, color=I("#8da0cb"), opacity=0.6, showlegend=FALSE)  %>% 
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = format(date.labels, "%b %y")
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="App-based R(t) indicator, R<sub>app</sub>(t)",
        titlefont=f4,
        tickfont=f1,
        range=c(0,line.height)
      )
    )

  change_scale_relative <- function(x){
    exp(x)-1
  }
  inverse_change_scale_relative <- function(x){
    log(1+x)
  }
  
  line.height <- 0.6*2#.3
  neg.line.height <- -0.5*2#.3
  decomp_short <- plot_ly(national.data.full %>%
                        filter(date <= last.date.to.plot)) %>%
    # add_lines(x=as.Date("2022-12-06"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-04-01"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-02-24"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-01-27"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2022-01-11"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-11-27"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-08-16"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-08-02"), y=c(-line.height,line.height), color=I("darkgrey"),
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-07-19"), y=c(-line.height,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-05-17"), y=c(-line.height,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-04-12"), y=c(-line.height,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-03-29"), y=c(-line.height,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    # add_lines(x=as.Date("2021-03-08"), y=c(-line.height,line.height), color=I("darkgrey"), 
    #           line=list(width=3), showlegend=FALSE) %>%
    add_polygons(x=c(as.Date("2021-06-11"), as.Date("2021-06-11"),
                   as.Date("2021-07-11"), as.Date("2021-07-11")),
               y=c(neg.line.height,line.height, line.height, neg.line.height),
               color=I("grey90"), line=list(width=0), showlegend=FALSE) %>%
    add_bars(x=~date, y=~log(contact_rate/median(contact_rate, na.rm=T)), color=I("#66c2a5"), opacity=0.8,
             name="Contact rate", legendgroup="1") %>%
    add_bars(x=~date, y=~log(TPAEN/median(TPAEN, na.rm=T)), color=I("#fc8d62"), opacity=0.8,
              name="\nProbability of\nreported infection", legendgroup="2") %>%
    # add_lines(x=~date, y=~log(contact_rate/median(contact_rate, na.rm=T))+log(TPAEN/median(TPAEN, na.rm=T)), color=I("#8da0cb"),
    #           line=list(width=2, opacity=1), showlegend=FALSE) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Date of notification",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = paste0("1 ", format(date.labels, "%b %y")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Relative change compared to median",
        titlefont=f4,
        tickfont=f1,
        range=c(neg.line.height,line.height),
        tickvals = inverse_change_scale_relative(c(-0.5,-0.25,0,0.5,1,2)),
        ticktext = c("-50%","-25%","0%","+50%","+100%","+200%"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      legend=list(
        title=list(text="Contribution\n", font=f1),
        font=f1,
        tracegroupgap=20
      ),
      #barmode="stack",
      bargap = 0
    )
  
  decomp_short
  
  
  lockdown.CR.baseline <- mean(
    (national.data.full %>%
      filter(date >= as.Date("2021-02-01") & date <= as.Date("2021-04-30")))$contact_rate, na.rm=T)
  
  line.height <- 45
  line.col.2021.to.2022 <- "#44806e"
  line.col.2022.to.2023 <- "#49d6ac"

  
  CR.christmases <- plot_ly(national.data.full) %>%  
    filter(date <= last.date.to.plot) %>%
    add_lines(x=as.Date("2022-12-25"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3, color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=as.Date("2021-12-25"), y=c(0,line.height), color=I("darkgrey"),
              line=list(width=3, color="black", dash="dash"), showlegend=FALSE) %>%
    add_lines(x=~date, y=~contact_rate, name="", color=I(line.col.2021.to.2022), 
              showlegend=FALSE, line=list(width=4, opacity=1)) %>%
    add_ribbons(x=~date, ymin=~lower.contact.rate, ymax=~upper.contact.rate, 
                color=I(line.col.2021.to.2022), opacity=0.6, showlegend=FALSE)  %>% 
    add_lines(x=~date, y=~lockdown.CR.baseline, name="", 
              line=list(width=3, color="black", dash="dot"), 
              showlegend=FALSE) %>%
    add_lines(x=~(date[366:nrow(national.data.full)] - 365), y=~contact_rate[366:nrow(national.data.full)], name="", color=I(line.col.2022.to.2023), 
              showlegend=FALSE, line=list(width=4, opacity=1)) %>%
    add_ribbons(x=~(date[366:nrow(national.data.full)] - 365), ymin=~lower.contact.rate[366:nrow(national.data.full)], ymax=~upper.contact.rate[366:nrow(national.data.full)], 
                color=I(line.col.2022.to.2023), opacity=0.6, showlegend=FALSE)  %>% 
    add_annotations(x=as.Date("2022-02-21"), y=17, text="2021-2022",
                    font=list(
                      family = "Arial, sans-serif",
                      size = 32,
                      color = line.col.2021.to.2022
                    ),
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_annotations(x=as.Date("2022-02-21"), y=33, text="2022-2023",
                    font=list(
                      family = "Arial, sans-serif",
                      size = 32,
                      color = line.col.2022.to.2023
                    ),
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    add_annotations(x=as.Date("2021-11-15"), y=16, text="Mean lockdown\ncontact level",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Date of notification",
        titlefont=f1,
        tickfont=f1,
        range=c(as.Date("2021-11-01"), as.Date("2022-02-28")),
        tickvals = date.labels,
        ticktext = paste0("1 ", format(date.labels, "%b")),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="Contact rate of\ntest-positive app users CR(t)",
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      legend=list(
        font=f1
      )
    )
  
  #CR.christmases
  
  plots <- list()
  
  plots$p1 <- CR
  plots$p2 <- TPAEN
  plots$p3 <- CRxTPAEN
  plots$p4 <- R_decomp
  plots$p5 <- CR.christmases
  plots$p6 <- R_short
  plots$p7 <- decomp_short
  
  plots
}
