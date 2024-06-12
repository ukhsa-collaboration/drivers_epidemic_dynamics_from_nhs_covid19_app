get_google_mobility_plot <- function() {
  
  google_mobility_data <- read_csv("data/googlemobilitydataset290922.csv", show_col_types = FALSE)
  
  plot_ly(google_mobility_data) %>%
    add_lines(x=~Date, y=~`Retail and recreation`, name='Retail and recreation',
              line=list(width=3)) %>%
    add_lines(x=~Date, y=~`Grocery and pharmacy`, name='Grocery and pharmacy',
              line=list(width=3)) %>%
    add_lines(x=~Date, y=~`Parks`, name='Parks',
              line=list(width=3)) %>%
    add_lines(x=~Date, y=~`Transit stations`, name='Transit stations',
              line=list(width=3)) %>%
    add_lines(x=~Date, y=~`Workplaces`, name='Workplaces',
              line=list(width=3)) %>%
    add_lines(x=~Date, y=~`Residential`, name='Residential',
              line=list(width=3)) %>%
    add_lines(x=~Date, y=100, line=list(width=3, dash="dash"), showlegend=FALSE, color=I("black")) %>%
    layout(
      xaxis=list(
        tickfont=f1,
        titlefont=f1
      ),
      yaxis=list(
        tickfont=f1,
        titlefont=f1,
        title="Volume of visits"
      ),
      legend=list(
        font=f1
      )
    )
  
}

