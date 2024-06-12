# analytics data 
cleaned.app.data <- read_csv(glue("data/private/clean_timeseries_24Sep20_9May23.csv"), show_col_types = FALSE)

dummy.app.data <- cleaned.app.data %>%
  mutate(across(c(3:11,13:16), \(x) mean(x, na.rm=T)))

write_csv(dummy.app.data, "data/dummyprivate/clean_timeseries_24Sep20_9May23.csv")

# regional data
regions <- c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "South West", "East of England", "South East", "London", "Wales", "EnglandAndWales")

for (r in regions) {
  
  original.regional.TPAEN.data <- read_csv(glue("data/private/full_TPAEN_{r}_1Jan21_9May23.csv"), show_col_types = FALSE)
  
  dummy.regional.TPAEN <- original.regional.TPAEN.data %>%
    mutate(across(2:11, \(x) mean(x, na.rm=T)))
  
  write_csv(dummy.regional.TPAEN, glue("data/dummyprivate/full_TPAEN_{r}_1Jan21_9May23.csv"))
  
}

# exposure data summary
table_outcome_riskScores <- read_csv(file="data/private/table_riskScores.csv", show_col_types=FALSE) %>% 
  filter(maxRiskScore > 100)

dummy.riskScores <- table_outcome_riskScores %>%
  group_by(classification) %>%
  mutate(across(c(3,5:16,18,23:25,27), \(x) mean(x, na.rm=T))) %>%
  ungroup()

# since this is a very large file, subsample to get fewer rows
set.seed(123)
example.rows <- sample(1:nrow(dummy.riskScores), 1000)
dummy.riskScores <- dummy.riskScores[example.rows, ]

write_csv(dummy.riskScores, file="data/dummyprivate/table_riskScores.csv")
