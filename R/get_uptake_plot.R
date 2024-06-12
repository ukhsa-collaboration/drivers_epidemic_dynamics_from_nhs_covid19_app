get_uptake_plot <- function(cleaned.app.data, analytics.data.start.date, analytics.data.end.date) {
  
  daily_users_data <- cleaned.app.data %>%
    filter(date <= analytics.data.end.date) %>%
    group_by(date) %>%
    summarise("total.users" = sum(users, na.rm=T)) %>%
    mutate("weekly.rolling.mean.total.users" = frollmean(total.users, n=7, fill=NA, align="center"))
  
  date.labels <- seq.Date(analytics.data.start.date, analytics.data.end.date, by="month")
  
  plot_ly(daily_users_data) %>%
    add_lines(x=~date, y=~weekly.rolling.mean.total.users,
              line=list(width=3)) %>% 
    layout(
      xaxis=list(
        title="",
        tickfont=f2,
        tickvals = date.labels,
        ticktext = paste0("1 ",format(date.labels, "%b %y"))
      ),
      yaxis=list(
        titlefont=f1,
        tickfont=f2,
        title="Active app users\n(rolling 7-day mean)",
        range=c(0,18500000)
      )
    )
}


