get_R_estimates_plot <- function(national.data.full = national.data.full){
  
  England.color.main <- "#2B57AC"
  Wales.color.main <- "#CF1E26"
  
  # load official estimates from 
  # https://statswales.gov.wales/Catalogue/Health-and-Social-Care/coronavirus-covid-19/reproduction-r-number
  # and 
  # https://www.gov.uk/guidance/the-r-value-and-growth-rate#latest-r-and-growth-rate-for-england 
  
  official_estimates_England <- read_csv("data/published_R_time_series_England.csv", show_col_types = FALSE)
  official_estimates_Wales <- read_csv("data/published_R_time_series_Wales.csv", show_col_types = FALSE)
  
  official_estimates_plot <- plot_ly() %>%
    add_lines(x=c(as.Date("2020-05-12"), as.Date("2023-03-15")), y=1, line=list(dash="dash", width=4, color="black"), showlegend=FALSE) %>%
    add_ribbons(data=official_estimates_England, x=~date, ymin=~England.lower, ymax=~England.upper, 
                color=I(England.color.main), opacity=0.8, name="England", legendgroup="offical") %>%
    add_ribbons(data=official_estimates_Wales, x=~date, ymin=~Wales.lower, ymax=~Wales.upper,
                color=I(Wales.color.main), opacity=0.8, name="Wales", legendgroup="offical") %>%
    layout(
      xaxis=list(
        #title="Date",
        #range=c(as.Date("2021-01-01"), as.Date("2022-12-31")),
        tickfont=f1,
        titlefont=f1
      ),
      yaxis=list(
        range=c(0.5, 1.7),
        tickfont=f1,
        titlefont=f1,
        title="Estimated R(t)"
      ),
      legend=list(
        font=f1
      )
    )
  
  # save_image(official_estimates_plot, file="plots/official_estimates.png", width=1800, height=800)

  # LSHTM EpiNow2 estimates
  
  LSHTM_dates <- sort(seq.Date(from=as.Date("2022-06-27"), to=as.Date("2020-07-27"), by=-70))
  # workarounds to get nearest available date
  LSHTM_dates[[1]] <- as.Date("2020-07-31")
  LSHTM_dates[[6]] <- as.Date("2021-07-13")
  LSHTM_dates[[10]] <- as.Date("2022-05-09")
  
  LSHTM_estimates <- lapply(LSHTM_dates, function(d) read_csv(glue("data/EpiNow2/rt_{d}.csv"), show_col_types = FALSE) %>%
                              filter(country == "United Kingdom") %>%
                              filter(type == "estimate"))
  
  LSHTM_colors <- colorRampPalette(c("#1b7837", "darkblue", "purple", "darkorange"))(length(LSHTM_dates))
  
  LSHTM_estimates_plot <- plot_ly() %>%
    add_lines(x=c(as.Date("2020-05-12"), as.Date("2023-03-15")), y=1, line=list(dash="dash", width=4, color="black"), showlegend=FALSE) %>%
    layout(
      xaxis=list(
        #title="Date",
        #range=c(as.Date("2021-01-01"), as.Date("2022-12-31")),
        tickfont=f1,
        titlefont=f1
      ),
      yaxis=list(
        range=c(0.5, 1.7),
        tickfont=f1,
        titlefont=f1,
        title="Estimated R(t)"
      ),
      legend=list(
        font=f2#,
        #title=list(text="Estimate\ncalculated:")
      )
    )
  
  for (x in 1:length(LSHTM_dates)) {
    LSHTM_estimates_plot <- LSHTM_estimates_plot %>%
      add_lines(data=LSHTM_estimates[[x]], x=~date, y=~median, 
                color=I(LSHTM_colors[[x]]), name=format(LSHTM_dates[[x]], "%b %y"), showlegend=TRUE,
                line=list(width=3), legendgroup="LSHTM") %>%
      add_ribbons(data=LSHTM_estimates[[x]], x=~date, ymin=~lower_90, ymax=~upper_90, 
                  color=I(LSHTM_colors[[x]]), opacity=0.5, showlegend=FALSE, legendgroup="LSHTM")
  }
  
  #save_image(LSHTM_estimates_plot, file="plots/LSHTM_estimates.png", width=1800, height=800)
  
  # Estimates based on LocalCovidTracker
  
  R_LCT_ltlas <- read_csv("data/R_estimates_LCT_ltlas.csv", show_col_types = FALSE)
  projected_cases_LCT_ltlas <- read_csv("data/projected_cases_LCT_ltlas.csv", show_col_types = FALSE)
  
  R_EngWales_estimates_LCT <- full_join(R_LCT_ltlas, projected_cases_LCT_ltlas) %>% 
    filter(date <= as.Date("2023-03-15")) %>% 
    group_by(date) %>%
    summarise("EngWales.R" = sum(R * scaled_per_capita, na.rm=TRUE) / sum(scaled_per_capita, na.rm=TRUE),
              "EngWales.lower.R" = sum(lower * scaled_per_capita, na.rm=TRUE) / sum(scaled_per_capita, na.rm=TRUE),
              "EngWales.upper.R" = sum(upper * scaled_per_capita, na.rm=TRUE) / sum(scaled_per_capita, na.rm=TRUE))
  
  R_estimates_LCT_plot <-  plot_ly(R_EngWales_estimates_LCT) %>%
    add_lines(x=c(as.Date("2020-05-12"), as.Date("2023-03-15")), y=1, line=list(dash="dash", width=4, color="black"), showlegend=FALSE) %>%
    add_ribbons(x=~date, ymin=~EngWales.lower.R, ymax=~EngWales.upper.R, 
                color=I("forestgreen"), opacity=0.3, showlegend=FALSE, legendgroup="LCT") %>%
    add_lines(x=~date, y=~EngWales.R, 
              color=I("forestgreen"), opacity=1, showlegend=FALSE, legendgroup="LCT", line=list(width=3)) %>%
    layout(
      xaxis=list(
        #title="Date",
        #range=c(as.Date("2021-01-01"), as.Date("2022-12-31")),
        tickfont=f1,
        titlefont=f1
      ),
      yaxis=list(
        range=c(0.5, 2.3),
        tickfont=f1,
        titlefont=f1,
        title="Estimated R(t)"
      ),
      legend=list(
        font=f1
      )
    )
  
  # save_image(R_estimates_LCT_plot, file="plots/R_estimates_LCT_plot.png", width=1800, height=800)
  
  # app-based indicator of R  

  R_estimates_app <- national.data.full %>%
    select(date,
           "R_app" = CRxTPAEN,
           "R_app_lower" = lower.CRxTPAEN,
           "R_app_upper" = upper.CRxTPAEN) %>%
    filter(date <= as.Date("2023-03-15"))
  
  date.labels <- seq.Date(as.Date("2020-04-01"), as.Date("2023-03-15"), by="month")
  
  R_estimates_app_plot <-  plot_ly(R_estimates_app) %>%
    #add_lines(x=c(as.Date("2020-05-12"), as.Date("2023-03-15")), y=0.6, line=list(dash="dot", width=4, color="black"), showlegend=FALSE) %>%
    add_ribbons(x=~date, ymin=~R_app_lower, ymax=~R_app_upper, 
                color=I("purple"), opacity=0.3, showlegend=FALSE, legendgroup="LCT") %>%
    add_lines(x=~date, y=~R_app, 
              color=I("purple"), opacity=1, showlegend=FALSE, legendgroup="LCT", line=list(width=3)) %>%
    layout(
      xaxis=list(
        #title="Date",
        #range=c(as.Date("2021-01-01"), as.Date("2022-12-31")),
        tickfont=f1,
        titlefont=f1,
        tickvals = date.labels,
        ticktext = paste0("1 ", format(date.labels, "%b %y")),
        ticks="ouside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        range=c(0,2.7),
        tickfont=f1,
        titlefont=f1,
        title="App-based indicator R<sub>app</sub>(t)"
      ),
      legend=list(
        font=f1
      )
    )
  
  # save_image(R_estimates_app_plot, file="plots/R_estimates_app_plot.png", width=1800, height=800)
  
  # assemble all plots
  
  subplot(
    official_estimates_plot %>% add_annotations(x=as.Date("2021-08-01"), y=1.7, text="SAGE / UKHSA",
                                                font=f1,
                                                xref = "x",
                                                yref = "y",
                                                showarrow = FALSE),
    LSHTM_estimates_plot %>% add_annotations(x=as.Date("2021-08-01"), y=1.7, text="EpiNow2",
                                             font=f1,
                                             xref = "x",
                                             yref = "y",
                                             showarrow = FALSE),
    R_estimates_LCT_plot %>% add_annotations(x=as.Date("2021-08-01"), y=2.2, text="LocalCovidTracker",
                                             font=f1,
                                             xref = "x",
                                             yref = "y",
                                             showarrow = FALSE),
    R_estimates_app_plot %>% add_annotations(x=as.Date("2021-08-01"), y=2.2, text="App-based indicator R<sub>app</sub>(t)",
                                             font=f1,
                                             xref = "x",
                                             yref = "y",
                                             showarrow = FALSE),
    nrows=4,
    shareX=TRUE,
    titleY=TRUE) %>%
    layout(legend = list(tracegroupgap = 400))

}


get_R_estimates_plot_supplementary <- function(){
  
  England.color.main <- "#2B57AC"
  Wales.color.main <- "#CF1E26"
  
  individual_estimates <- read_csv("data/hist_R_cleaned_internal_models_01012021_20122022_MK_cleaned.csv", show_col_types = FALSE)
  
  estimate.names <- sort(unique(individual_estimates$Full_Name))
  
  # omit estimates with fewer than 5 dates available:
  names.to.remove <- vector()
  for (this.name in estimate.names) {
    this.name.rows <- which(individual_estimates$Full_Name == this.name)
    print(this.name)
    print(length(this.name.rows))
    if (length(this.name.rows) <= 5) names.to.remove <- c(names.to.remove, this.name)
  }
  
  names.to.keep <- setdiff(estimate.names, names.to.remove)
  individual_estimates_cleaned <- individual_estimates %>% filter(Full_Name %in% names.to.keep)
  estimate.names.cleaned <- sort(unique(individual_estimates_cleaned$Full_Name))
  estimate.names.cleaned
  
  # sanity check
  stopifnot(names.to.keep == estimate.names.cleaned)
  
  for (estimate.name in estimate.names.cleaned) {
    assign(glue("individual.R.{estimate.name}"), 
           plot_ly() %>%
             add_lines(x=c(as.Date("2021-01-01"), as.Date("2022-12-31")), y=1, line=list(dash="dash", width=4, color="black"), showlegend=FALSE) %>%
             add_ribbons(data=individual_estimates_cleaned %>% filter(Geography == "England", Full_Name == estimate.name),
                         x=~Date, ymin=~LowerBound, ymax=~UpperBound, legendgroup=~Geography, name=~Geography, color=I(England.color.main), opacity=0.7) %>%
             add_ribbons(data=individual_estimates_cleaned %>% filter(Geography == "Wales", Full_Name == estimate.name),
                         x=~Date, ymin=~LowerBound, ymax=~UpperBound, legendgroup=~Geography, name=~Geography, color=I(Wales.color.main), opacity=0.7) %>%
             add_annotations(text=estimate.name, 
                             x=as.Date("2022-01-01"), y=3, 
                             font=f1,
                             xref = "x",
                             yref = "y",
                             showarrow = FALSE) %>%
             layout(
               # title=list(
               #   text=estimate.name,
               #   font=f1,
               #   y=0.95
               # ),
               xaxis=list(
                 range=c(as.Date("2021-01-01"), as.Date("2022-12-31")),
                 tickfont=f1,
                 titlefont=f1
               ),
               yaxis=list(
                 range=c(min(individual_estimates_cleaned$LowerBound),max(individual_estimates_cleaned$UpperBound)),
                 tickfont=f1,
                 titlefont=f1,
                 title="Estimated R(t)"
               ),
               legend=list(
                 font=f1
               )
             )
    )
    
    #save_image(get(glue("individual.R.{estimate.name}")), file=glue("plots/individual_R_{estimate.name}.png"), width=1200, height=400)
  }
    
    individual.R.estimates <- subplot(list(style(get("individual.R.JBC Covasim"),showlegend=FALSE), style(get("individual.R.JBC Epidemia"),showlegend=FALSE),
                                           style(get("individual.R.JBC Epidemia Admissions"),showlegend=FALSE), style(get("individual.R.JBC Epidemia Cases"),showlegend=FALSE),
                                           style(get("individual.R.JBC Epidemia ONS positivity"),showlegend=FALSE), style(get("individual.R.JBC EpiEstim"),showlegend=FALSE),
                                           style(get("individual.R.JBC Genomic Surveillance"),showlegend=FALSE), style(get("individual.R.JBC OpenABM"),showlegend=FALSE),
                                           style(get("individual.R.JBC OxfordCSML"),showlegend=FALSE), style(get("individual.R.JBC-University of London"),showlegend=FALSE),
                                           style(get("individual.R.Manchester-Oxford-Lancaster DetSEIRwithNBmcmc"),showlegend=FALSE), style(get("individual.R.PHE-Cambridge Admissions and ONS data"),showlegend=FALSE),
                                           style(get("individual.R.PHE-Cambridge Deaths and ONS"),showlegend=FALSE), get("individual.R.PHE-Cambridge Regional and Age data")), 
                                      nrows=7,
                                      shareX=TRUE, shareY=TRUE)
    
    individual.R.estimates

}














