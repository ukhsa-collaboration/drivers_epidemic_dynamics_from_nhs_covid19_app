compute_regional_analytics_summaries <- function(cleaned.app.data, dummy.data) {
    
  # get the region data and add it to the cleaned app data - useful for later
  region.data <- read_csv("data/Merged_PD_Demo_Geo_2021-01-21_v2.csv", show_col_types = FALSE)
  region.data <- region.data %>%
    select("ltla_name" = local_authority,
           "Region" = region) 
  region.data <- unique(region.data)
  ltla.data <- left_join(cleaned.app.data, region.data)
  
  # remove empty rows
  if (any(is.na(ltla.data$ltla_name))) ltla.data <- ltla.data[- which(is.na(ltla.data$ltla_name)), ]
  
  ltla.data <- ltla.data %>%
    mutate(k=if_else(as.Date(date)<'2021-06-11',0.4, shared_keys / test_positive) ) %>%  # fixed k=0.4 before we got detailed data on this from 11 June 2021
    mutate(enpic = notifications / test_positive) %>%
    mutate(contact_rate_numerator = notifications) %>%
    mutate(contact_rate_denominator = uptake * test_positive * k) %>%
    mutate(contact_rate = contact_rate_numerator / contact_rate_denominator)
  
  ltla.data$k[is.infinite(ltla.data$k)] <- NA # remove Inf values from where there are no positive tests in an LTLA on a date
  
  write_csv(ltla.data, "data/private/ltla.data.csv")
  
  regional.data.full <- ltla.data %>%
    group_by(date, Region) %>% 
    summarise(uptake = mean(uptake,na.rm=T), 
              test_positive=sum(test_positive,na.rm=T),
              notifications=sum(notifications,na.rm=T),
              k=mean(k,na.rm=T))
  
  regional.data.full <- regional.data.full %>%
    mutate(contact_rate_numerator = notifications) %>%
    mutate(contact_rate_denominator = uptake * test_positive * k) %>%
    mutate(contact_rate = contact_rate_numerator/contact_rate_denominator)
  
  if (dummy.data) { # we cannot compute meaningful CIs on the dummy data; setting the upper and lower bounds to match the central estimate
    regional.data.full$lower.contact.rate <- regional.data.full$contact_rate
    regional.data.full$upper.contact.rate <- regional.data.full$contact_rate
  } else {
    logldiff <- sapply(seq(0,100,0.1), function(e){
      dpois(regional.data.full$contact_rate_numerator, lambda = regional.data.full$contact_rate_numerator, log = TRUE) - 
        dpois(regional.data.full$contact_rate_numerator, lambda = e * regional.data.full$contact_rate_denominator, log = TRUE)
    })
    
    CIs <- suppressWarnings(t(apply(logldiff,1,function(x){0.1*(range(which(x<1.92))-1)})))
    
    regional.data.full$lower.contact.rate <- CIs[,1]
    regional.data.full$upper.contact.rate <- CIs[,2]
  }
  
  # get TPAEN data
  regions.alphabetical <- sort(unique(regional.data.full$Region))
  
  TPAEN <- bind_rows(read_csv("data/private/full_TPAEN_East Midlands_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[1]]),
                     read_csv("data/private/full_TPAEN_East of England_1Jan21_9May23.csv", show_col_types = FALSE)
                     %>% mutate("Region" = regions.alphabetical[[2]]),
                     read_csv("data/private/full_TPAEN_London_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[3]]),
                     read_csv("data/private/full_TPAEN_North East_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[4]]),
                     read_csv("data/private/full_TPAEN_North West_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[5]]),
                     read_csv("data/private/full_TPAEN_South East_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[6]]),
                     read_csv("data/private/full_TPAEN_South West_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[7]]),
                     read_csv("data/private/full_TPAEN_Wales_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[8]]),
                     read_csv("data/private/full_TPAEN_West Midlands_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[9]]),
                     read_csv("data/private/full_TPAEN_Yorkshire and The Humber_1Jan21_9May23.csv", show_col_types = FALSE) 
                     %>% mutate("Region" = regions.alphabetical[[10]])
  ) %>%
    select(date, Region, "TPAEN" = mean, "lower.TPAEN"=`2.5%`, "upper.TPAEN"=`97.5%`)  
  
  regional.data.full <- left_join(regional.data.full, TPAEN)
  
  # get confidence intervals on CRxTPAEN
  regional.data.full <- regional.data.full %>%
    mutate(sigma.contact.rate = suppressWarnings((log(upper.contact.rate) - log(lower.contact.rate))/(1.96*2) ) ) %>%
    mutate(sigma.TPAEN = (log(upper.TPAEN) - log(lower.TPAEN))/(1.96*2) ) %>%
    mutate(sigma.total = sqrt(sigma.contact.rate^2 + sigma.TPAEN^2) ) %>%
    mutate(lower.CRxTPAEN = contact_rate*TPAEN*exp(-1.96*sigma.total)) %>%
    mutate(upper.CRxTPAEN = contact_rate*TPAEN*exp(+1.96*sigma.total))
  
  # tidying and censoring
  regional.data.full <- regional.data.full %>% filter(date < Sys.Date()) 
  
  rows.to.right.censor.TPAEN.estimates <- which(regional.data.full$date %in% seq.Date(as.Date("9May23", format="%d%B%y")-5, as.Date("9May23", format="%d%B%y") + 1, by="day" ))
  
  regional.data.full$TPAEN[rows.to.right.censor.TPAEN.estimates] <- NA
  
  # regional summaries for map plots
  regional.summaries <- regional.data.full %>%
    filter(date <= analytics.data.end.date) %>%
    group_by(Region) %>%
    summarise("contact_rate_median" = median(contact_rate, na.rm=TRUE),
              "TPAEN_median" = median(TPAEN*100, na.rm=TRUE),
              "app_based_R_median" = median(contact_rate*TPAEN, na.rm=TRUE)
    )
  write_csv(regional.summaries, file="results/regional.summaries.csv")
  
  regional.data.full

}
