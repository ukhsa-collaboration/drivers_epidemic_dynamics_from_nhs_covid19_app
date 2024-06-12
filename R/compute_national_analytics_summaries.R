compute_national_analytics_summaries <- function(cleaned.app.data, dummy.data){
  
  # cleaned.app.data has a row per combination of date and area.
  # summarise into a "national" dataframe with one row per date and mean uptake, total positive tests and total notifications 
  national.data <- cleaned.app.data %>% 
    group_by(date) %>% 
    summarise(uptake = mean(uptake,na.rm = T), 
              test_positive = sum(test_positive,na.rm = T),
              # test_positive_PCR = sum(test_positive_PCR,na.rm = T),
              # test_positive_LFD = sum(test_positive_LFD,na.rm = T),
              # test_positive_NHS = sum(test_positive_NHS,na.rm = T),
              # test_positive_private = sum(test_positive_private,na.rm = T),
              # test_positive_free_NHS_LFD = sum(test_positive_free_NHS_LFD,na.rm = T),
              notifications = sum(notifications,na.rm = T),
              keys = sum(shared_keys, na.rm = T)
    ) %>%
    mutate("k" = if_else(as.Date(date)<'2021-06-11',0.4, keys / test_positive)  # fixed k=0.4 before we got detailed data on this from 11 June 2021
    )
  
  # make numerator and denominator for ENPIC := Notifications / (uptake * positives * k)
  national.data <- national.data %>%
    mutate(enpic = notifications / test_positive) %>% 
    mutate(contact_rate_numerator = notifications) %>%
    mutate(contact_rate_denominator = uptake * test_positive * k) %>%
    mutate(contact_rate = contact_rate_numerator/contact_rate_denominator) # %>%
  
  if (dummy.data) { # we cannot compute meaningful CIs on the dummy data; setting the upper and lower bounds to match the central estimate
    national.data$lower.contact.rate <- national.data$contact_rate
    national.data$upper.contact.rate <- national.data$contact_rate
  } else {
    logldiff <- sapply(seq(0,100,0.1), function(e){
      dpois(national.data$contact_rate_numerator, lambda = national.data$contact_rate_numerator, log = TRUE) - 
        dpois(national.data$contact_rate_numerator, lambda = e * national.data$contact_rate_denominator, log = TRUE)
    })
    
    # each row of logldiff corresponds to a date. 
    # The 401 columns correspond to the seq(0,40,0.1) of possible enpic values,
    # where the 40 is a bit arbitrary and should be adjusted if enpic climbs much higher.
    # To get 95% CIs we look at a particular date and test the possible values of enpic to see which
    # are the most likely "which(x < 1.92)", e.g. for the 20th date right now they're 101 102 103 104 
    # get the range of these, e.g. 101 104
    # subtract 1 and multiply by 0.1 to get back to the sequence seq(0,40,0.1), e.g. 10.0 10.3
    # and that gives you your CIs for that date.
    
    CIs <- suppressWarnings(t(apply(logldiff,1,function(x){0.1*(range(which(x<1.92))-1)})))
    
    national.data <- national.data %>%
      mutate("lower.contact.rate"=CIs[,1]) %>%
      mutate("upper.contact.rate"=CIs[,2])
    
  }
 
  # add the TPAEN estimates
  TPAEN <- read_csv(glue("data/private/full_TPAEN_EnglandAndWales_1Jan21_9May23.csv") , show_col_types = FALSE) %>%
    select(date, "TPAEN" = mean, "lower.TPAEN" = `2.5%`, "upper.TPAEN" = `97.5%`)
  national.data.full <- left_join(national.data, TPAEN)
  
  # get confidence intervals on CRxTPAEN 
  national.data.full <- national.data.full %>%
    filter(test_positive > 0) %>% # if test_positive = 0 (due to being the last day of data) then we get contact_rate=NaN and we can't take logs in the next step
    mutate(sigma.contact.rate = suppressWarnings((log(upper.contact.rate) - log(lower.contact.rate))/(1.96*2) ) )  %>%
    mutate(sigma.TPAEN = (log(upper.TPAEN) - log(lower.TPAEN))/(1.96*2) ) %>%
    mutate(sigma.total = sqrt(sigma.contact.rate^2 + sigma.TPAEN^2) ) %>%
    mutate(CRxTPAEN = contact_rate*TPAEN) %>%
    mutate(lower.CRxTPAEN = contact_rate*TPAEN*exp(-1.96*sigma.total)) %>%
    mutate(upper.CRxTPAEN = contact_rate*TPAEN*exp(+1.96*sigma.total))
  
  # tidying and censoring
  national.data.full <- national.data.full %>% filter(date < Sys.Date())
  
  rows.to.right.censor.TPAEN.estimates <- which(national.data.full$date %in% seq.Date(as.Date("9May23", format="%d%B%y")-5, as.Date("9May23", format="%d%B%y") + 1, by="day" ))
  
  national.data.full$TPAEN[rows.to.right.censor.TPAEN.estimates] <- NA
  national.data.full$CRxTPAEN[rows.to.right.censor.TPAEN.estimates] <- NA
  
  national.data.full
}