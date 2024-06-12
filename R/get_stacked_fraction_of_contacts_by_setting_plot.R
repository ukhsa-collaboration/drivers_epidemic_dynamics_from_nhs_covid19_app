get_stacked_fraction_of_contacts_by_setting_plot <- function(grouped_exposure_data,
                                                             exposure.data.end.date){
  
  fleeting.color <- "#88CCEE"
  single.day.color <- "#44AA99"
  recurring.color <- "#CC6677"
  household.color <- "#882255"
  
  
  table_contacts_to_plot <- grouped_exposure_data %>%
    filter(date_plotted_exposure <= as.Date("2023-01-31") & date_plotted_exposure >= as.Date("2021-04-01")) %>% 
    group_by(date_plotted_exposure,setting) %>%
    summarise("contacts" = sum(weight, na.rm=T))
  # 
  # table_overall_contacts_to_plot <- table_contacts_to_plot %>%
  #   group_by(date_plotted_exposure) %>%
  #   summarise("total_contacts" = sum(contacts, na.rm=T))
  # 
  # plot_ly(table_overall_contacts_to_plot) %>%
  #   add_lines(x=~date_plotted_exposure, y=~total_contacts)

  table_fraction_of_contacts_to_plot <- table_contacts_to_plot %>%
    group_by(date_plotted_exposure) %>%
    mutate("fraction_of_contacts" = contacts/sum(contacts, na.rm=T)) %>%
    ungroup()  
  
  wider_table_fraction_of_contacts_to_plot <- table_fraction_of_contacts_to_plot %>%
    select(-contacts) %>%
    pivot_wider(names_from = setting, values_from = fraction_of_contacts)
  
  wider_table_fraction_of_contacts_to_plot_Dec_2021 <- wider_table_fraction_of_contacts_to_plot %>% 
    filter(date_plotted_exposure >= as.Date("2021-12-01")) %>% 
    filter(date_plotted_exposure <= as.Date("2022-01-11"))
  
  wider_table_fraction_of_contacts_to_plot_Dec_2022 <- wider_table_fraction_of_contacts_to_plot %>% 
    filter(date_plotted_exposure >= as.Date("2022-12-01")) %>% 
    filter(date_plotted_exposure <= as.Date("2023-01-11"))
  
  wider_table_fraction_of_contacts_to_plot_main <- wider_table_fraction_of_contacts_to_plot %>%
    filter(date_plotted_exposure <= exposure.data.end.date) %>% 
    filter(date_plotted_exposure >= as.Date("2021-04-01"))
  
  #head(wider_table_fraction_of_contacts_to_plot)
  
  write_lines(c(
  # stats to quote / discuss:
  paste0("OVERALL:"),
  paste0("fraction of contacts which were household:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_main$household),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_main$household),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_main$household)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_main$household),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_main$household)]),
  paste0("fraction of contacts which were recurring:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_main$recurring),3)), 
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_main$recurring),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_main$recurring)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_main$recurring),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_main$recurring)]),
  paste0("fraction of contacts which were single day:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_main$`single day`),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_main$`single day`),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_main$`single day`)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_main$`single day`),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_main$`single day`)]),
  paste0("fraction of contacts which were fleeting:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_main$fleeting),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_main$fleeting),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_main$fleeting)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_main$fleeting),3), " on ", wider_table_fraction_of_contacts_to_plot_main$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_main$fleeting)]),
  
  # stats to quote / discuss for December 2021:
  paste0("CHRISTMAS 2021:"),
  paste0("fraction of contacts which were household:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2021$household),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2021$household),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2021$household)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2021$household),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2021$household)]),
  paste0("fraction of contacts which were recurring:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2021$recurring),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2021$recurring),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2021$recurring)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2021$recurring),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2021$recurring)]),
  paste0("fraction of contacts which were single day:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2021$`single day`),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2021$`single day`),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2021$`single day`)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2021$`single day`),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2021$`single day`)]),
  paste0("fraction of contacts which were fleeting:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2021$fleeting),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2021$fleeting),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2021$fleeting)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2021$fleeting),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2021$fleeting)]),
  
  # stats to quote / discuss for December 2022:
  paste0("CHRISTMAS 2022:"),
  paste0("fraction of contacts which were household:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2022$household),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2022$household),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2022$household)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2022$household),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2022$household)]),
  paste0("fraction of contacts which were recurring:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2022$recurring),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2022$recurring),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2022$recurring)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2022$recurring),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2022$recurring)]),
  paste0("fraction of contacts which were single day:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2022$`single day`),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2022$`single day`),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2022$`single day`)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2022$`single day`),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2022$`single day`)]),
  paste0("fraction of contacts which were fleeting:"),
  paste0("median ", signif(median(wider_table_fraction_of_contacts_to_plot_Dec_2022$fleeting),3)) ,
  paste0("min ", signif(min(wider_table_fraction_of_contacts_to_plot_Dec_2022$fleeting),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.min(wider_table_fraction_of_contacts_to_plot_Dec_2022$fleeting)]),
  paste0("max ", signif(max(wider_table_fraction_of_contacts_to_plot_Dec_2022$fleeting),3), " on ", wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure[which.max(wider_table_fraction_of_contacts_to_plot_Dec_2022$fleeting)])

  ),
  file="results/fraction.of.contacts.by.setting.txt")
  
  
  plots <- list()
  plots$p1 <- plot_ly(wider_table_fraction_of_contacts_to_plot_main, 
                                               type="bar", 
                                               x=~date_plotted_exposure, y=~fleeting, name="Fleeting", color=I(fleeting.color), legendgroup="frac_of_contacts") %>%
    add_trace(x=~date_plotted_exposure, y=~`single day`, name="Single day", color=I(single.day.color), legendgroup="frac_of_contacts") %>%
    add_trace(x=~date_plotted_exposure, y=~recurring, name="Recurring", color=I(recurring.color), legendgroup="frac_of_contacts") %>%
    add_trace(x=~date_plotted_exposure, y=~household, name="Household", color=I(household.color), legendgroup="frac_of_contacts") %>%
    layout(
      xaxis=list(
        title="Date of exposure",
        range=c(as.Date("2021-04-01"), exposure.data.end.date),
        titlefont=f1,
        tickfont=f1
      ),
      yaxis=list(
        title="Fraction of contacts",
        titlefont=f1,
        tickfont=f1
      ),
      legend=list(
        font=f1),
      barmode="stack",
      bargap = 0
    )
  
  date.labels.Dec.2021 <- seq.Date(min(wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure),
                                   max(wider_table_fraction_of_contacts_to_plot_Dec_2021$date_plotted_exposure), 
                                   by=4)
  
  plots$p2 <- plot_ly(wider_table_fraction_of_contacts_to_plot_Dec_2021, 
                        type="bar", 
                        x=~date_plotted_exposure, y=~fleeting, name="Fleeting", color=I(fleeting.color), legendgroup="frac_of_contacts_Dec2021") %>%
    add_trace(x=~date_plotted_exposure, y=~`single day`, name="Single day", color=I(single.day.color), legendgroup="frac_of_contacts_Dec2021") %>%
    add_trace(x=~date_plotted_exposure, y=~recurring, name="Recurring", color=I(recurring.color), legendgroup="frac_of_contacts_Dec2021") %>%
    add_trace(x=~date_plotted_exposure, y=~household, name="Household", color=I(household.color), legendgroup="frac_of_contacts_Dec2021") %>%
    add_annotations(x=as.Date("2021-12-22"), y=0.05, text="2021 to 2022",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>%
    # add_annotations(x=as.Date("2022-01-15"), y=0.05, text="2022",
    #                 font=f1,
    #                 xref = "x",
    #                 yref = "y",
    #                 showarrow = FALSE) %>%
    layout(
      xaxis=list(
        title="",
        titlefont=f2,
        tickfont=f3,
        tickvals = date.labels.Dec.2021,
        ticktext = " ",
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        title="Fraction of contacts",
        titlefont=f2,
        tickfont=f2
      ),
      legend=list(
        font=f2),
      barmode="stack",
      bargap = 0
    )
  
  date.labels.Dec.2022 <- seq.Date(min(wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure),
                                   max(wider_table_fraction_of_contacts_to_plot_Dec_2022$date_plotted_exposure), 
                                   by=4)
  
  plots$p3 <- plot_ly(wider_table_fraction_of_contacts_to_plot_Dec_2022, 
                      type="bar", 
                      x=~date_plotted_exposure, y=~fleeting, name="Fleeting", color=I(fleeting.color), legendgroup="frac_of_contacts_Dec2022") %>%
    add_trace(x=~date_plotted_exposure, y=~`single day`, name="Single day", color=I(single.day.color), legendgroup="frac_of_contacts_Dec2022") %>%
    add_trace(x=~date_plotted_exposure, y=~recurring, name="Recurring", color=I(recurring.color), legendgroup="frac_of_contacts_Dec2022") %>%
    add_trace(x=~date_plotted_exposure, y=~household, name="Household", color=I(household.color), legendgroup="frac_of_contacts_Dec2022") %>%
    add_annotations(x=as.Date("2022-12-22"), y=0.05, text="2022 to 2023",
                    font=f1,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE) %>% 
    # add_annotations(x=as.Date("2023-01-15"), y=0.05, text="2023",
    #                 font=f1,
    #                 xref = "x",
    #                 yref = "y",
    #                 showarrow = FALSE) %>%
    layout(
      xaxis=list(
        title="Date of exposure",
        titlefont=f2,
        tickfont=f3,
        tickvals = date.labels.Dec.2022,
        ticktext = format(date.labels.Dec.2022, "%d %b"),
        ticks="outside", tickwidth=2, ticklen=10
      ),
      yaxis=list(
        title="Fraction of contacts",
        titlefont=f2,
        tickfont=f2
      ),
      legend=list(
        font=f2),
      barmode="stack",
      bargap = 0
    )
  
  plots
}