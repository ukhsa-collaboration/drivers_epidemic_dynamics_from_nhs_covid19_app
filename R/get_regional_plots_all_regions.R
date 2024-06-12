ten_regions_plot <- function(regional.data.full, first.date.to.plot, last.date.to.plot) {
  
  date.labels <- seq.Date(as.Date("2020-12-01"), as.Date("2023-04-01"), by="month")
  
  # set up colour scheme to highlight London 
  regional.colours <- tibble(
                            "Region" = sort(unique(regional.data.full$Region)),
                            "reg.col" = c(rep("darkgrey", 2), "red", rep("darkgrey", 7))
  )
    
  regional.data.full <- left_join(regional.data.full, regional.colours)  %>% ungroup()
  
  line.height <- 160
  CR.regions <- plot_ly() %>% 
#filter(Region == "North East") %>%
    add_lines(data=regional.data.full %>% filter(Region != "London"),
              x=~date, y=~contact_rate, color=~Region, legendgroup=~factor(reg.col), 
              line=list(width=4, opacity=0.2), colors=~reg.col, showlegend=FALSE) %>%
    add_lines(data=regional.data.full %>% filter(Region == "London"),
              x=~date, y=~contact_rate, color=I("red"), name="London", 
              line=list(width=4, opacity=1), showlegend=FALSE) %>%
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
        title=glue("Contact rate of test-positive\napp users CR(t)"),
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      showlegend=FALSE
    )

  line.height <- 14
  TPAEN.regions <- plot_ly() %>%
    add_lines(data=regional.data.full %>%
                    filter(date <= last.date.to.plot) %>%
                    filter(Region != "London"), 
              x=~date, y=~TPAEN*100, color=~Region, legendgroup=~Region, showlegend=FALSE, opacity=1,
            line=list(width=5, opacity=0.2), colors=~reg.col) %>%
    add_lines(data=regional.data.full %>% 
                      filter(date <= last.date.to.plot)  %>%
                      filter(Region == "London"),
              x=~date, y=~TPAEN*100, color=I("red"), name="London", 
              line=list(width=4, opacity=1), showlegend=FALSE) %>%
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
        title=glue("Percentage of notified users who\ngo on to test positive TPAEN(t)"),
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      showlegend=FALSE
    )
  
  line.height <- 4
  CRxTPAEN.regions <- plot_ly() %>%
    add_lines(data=regional.data.full %>%
                    filter(date <= last.date.to.plot) %>%
                    filter(Region != "London"), 
              x=~date, y=~contact_rate*TPAEN, color=~Region, legendgroup=~Region, showlegend=FALSE, opacity=1,
            line=list(width=5, opacity=0.2), colors=~reg.col) %>%
    add_lines(data=regional.data.full %>% 
                      filter(date <= last.date.to.plot)  %>%
                      filter(Region == "London"),
              x=~date, y=~contact_rate*TPAEN, color=I("red"), name="London", 
              line=list(width=4, opacity=1)) %>%
    layout(
      xaxis=list(
        gridcolor = toRGB("darkgrey"),
        title="",
        titlefont=f1,
        tickfont=f1,
        range=c(first.date.to.plot, last.date.to.plot + 1),
        tickvals = date.labels,
        ticktext = paste0("1 ", format(date.labels, "%b %y")),
        ticks="ouside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        gridcolor = toRGB("darkgrey"),
        title=glue("App-based indicator\nR<sub>app</sub>(t) = CR(t) x TPAEN(t)"),
        titlefont=f1,
        tickfont=f1,
        range=c(0,line.height)
      ),
      showlegend=TRUE,
      legend=list(font=f1)
    )


  
  plot.list <- list()
  plot.list$p1 <- CR.regions
  plot.list$p2 <- TPAEN.regions
  plot.list$p3 <- CRxTPAEN.regions
  #plot.list$R <- R.regions
  plot.list
}

