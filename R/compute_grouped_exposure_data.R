compute_grouped_exposure_data <- function(table_outcome_riskScores, 
                                          ltla.data, 
                                          dummy.data){
  
  if (!dummy.data) { 
    grouped_data <- table_outcome_riskScores %>% 
    mutate("setting" = factor(
      if_else(classification=='household',classification,if_else(number_exposures==1,"fleeting",if_else(peak_duration<number_exposures,"recurring","single day"))),
      levels=c('household','recurring','single day','fleeting'))) %>%
    group_by(date,ltla) %>% 
    mutate(n_grouped=n()) %>% 
    left_join(., ltla.data %>% select(date,
                                      ltla,
                                      notifications,
                                      test_positive,
                                      uptake)) %>% 
    mutate("weight" = pmax(notifications,1)/n_grouped,
           "weekday" = lubridate::wday(date,week_start = 1),
#           "date_peak_exposure" = as.Date("2021-01-03")+lastExpDate-(((((lastExpDate-1)%%7)+1)-peak_day)%%7),
           "date_plotted_exposure" = as.Date("2021-01-03")+randomExpDate)  %>%
    ungroup() 
  } else { # use randomised "setting"s
    grouped_data <- table_outcome_riskScores %>% 
      mutate("setting" = factor(
        sample(c('household','recurring','single day','fleeting'), size=nrow(table_outcome_riskScores), replace=T),
        levels=c('household','recurring','single day','fleeting'))) %>%
      group_by(date,ltla) %>% 
      mutate(n_grouped=n()) %>% 
      left_join(., ltla.data %>% select(date,
                                        ltla,
                                        notifications,
                                        test_positive,
                                        uptake)) %>% 
      mutate("weight" = pmax(notifications,1)/n_grouped,
             "weekday" = lubridate::wday(date,week_start = 1),
             #           "date_peak_exposure" = as.Date("2021-01-03")+lastExpDate-(((((lastExpDate-1)%%7)+1)-peak_day)%%7),
             "date_plotted_exposure" = as.Date("2021-01-03")+randomExpDate)  %>%
      ungroup() 
  }
  
  grouped_data
}