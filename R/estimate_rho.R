# library(data.table)
# library(ggplot2)
# require(cowplot)
# require(Matrix)
# library(stats)

estimate_rho <- function(table_outcome_riskScores) {
  # modified from: https://r-forge.r-project.org/projects/lbrm/
  lb_auglag <- function(x, y, start, tol = 1e-7, ceps = 1e-7, control = list(),
                        control.optim = list()) {
    stopifnot(length(start) == ncol(x))
    elog <- function(x) {
      ans <- rep_len(-Inf, length(x))
      b <- x > 0
      ans[b] <- log(x[b])
      ans
    }
    posXColSum = colSums(x[y==1,])
    negX = x[y==0,]
    neg_log_likelihood_auglag <- function(x, y, beta) {
      #eta <- tcrossprod(beta, x) # todo: precompute column sums of beta
      negEta = drop(tcrossprod(beta, negX))
      -drop(crossprod( beta, posXColSum))- sum(elog(1 - exp(negEta)))
    }
    # neg_log_likelihood_auglag <- function(x, y, beta) {
    #   eta <- drop(tcrossprod(beta, x))
    #   b <- (y == 1)
    #   -sum(eta[b]) - sum(elog(1 - exp(eta[!b])))
    # }
    gradient_auglag <- function(x, y, beta) {
      negEta = drop(tcrossprod(beta, negX))
      mu <- pmin(exp(negEta), 1 - .Machine$double.neg.eps)
      -posXColSum -drop(crossprod(negX, (0 - mu) / (1 - mu)))
    }
    nloglike <- function(beta) neg_log_likelihood_auglag(x, y, beta)
    gradient <- function(beta) gradient_auglag(x, y, beta)
    control$eps <- tol
    control$trace <- TRUE
    s <- optim(par = start, fn = nloglike, gr = gradient,
               method = "L-BFGS-B",
               lower = ceps, control = control) # lower can't be exactly 0 because the objective is undefined for log(1-exp(0))
    return(s)
  }
  
  dt_1 = table_outcome_riskScores %>% 
    filter(!is.na(bg_rate_cases_app)) %>%
    filter(date>="2021-04-01" & date<"2022-02-28") %>%
    filter(number_exposures<=1) %>%
    mutate(date=as.factor(as.Date("2021-01-03")+randomExpDate),ReportPositive=(positive==1),ExposureRisk=cumRiskScore,BackgroundRisk=-log(1-bg_rate_cases_app)) %>% 
    select(date,ReportPositive,ExposureRisk,BackgroundRisk)
  dt_2_6 = table_outcome_riskScores %>% 
    filter(!is.na(bg_rate_cases_app)) %>%
    filter(date>="2021-04-01" & date<"2022-02-28") %>%
    filter(number_exposures>=2 & number_exposures<=6) %>%
    mutate(date=as.factor(as.Date("2021-01-03")+randomExpDate),ReportPositive=(positive==1),ExposureRisk=cumRiskScore,BackgroundRisk=-log(1-bg_rate_cases_app)) %>% 
    select(date,ReportPositive,ExposureRisk,BackgroundRisk)
  dt<-dt_1
  inputMatrix = -sparse.model.matrix( !ReportPositive ~ 0+date:ExposureRisk+BackgroundRisk, dt)
  system.time(model <- lb_auglag(inputMatrix, as.numeric(!dt[,'ReportPositive']), start = rep(1,ncol(inputMatrix))))
  oldvalue<-model$value+1; while(model$value<oldvalue-0.01){ oldvalue<-model$value; 
  system.time(model <- lb_auglag(inputMatrix, as.numeric(!dt[,'ReportPositive']), start = model$par)) 
  }
  coeffs_1 = setNames(model$par,colnames(inputMatrix))
  dt<-dt_2_6
  inputMatrix = -sparse.model.matrix( !ReportPositive ~ 0+date:ExposureRisk+BackgroundRisk, dt)
  system.time(model <- lb_auglag(inputMatrix, as.numeric(!dt[,'ReportPositive']), start = rep(1,ncol(inputMatrix))))
  oldvalue<-model$value+1; while(model$value<oldvalue-0.01){ oldvalue<-model$value; 
  system.time(model <- lb_auglag(inputMatrix, as.numeric(!dt[,'ReportPositive']), start = model$par)) 
  }
  coeffs_2_6 = setNames(model$par,colnames(inputMatrix))
  
  # coeffs_1["BackgroundRisk"] is the rho coefficient for contacts with a single exposure window (~0.47)
  # coeffs_2_6["BackgroundRisk"] is the rho coefficient for contacts with 2 or more exposure windows (~0.38)
  
  rho <- coeffs_2_6["BackgroundRisk"] + (coeffs_1["BackgroundRisk"] - coeffs_2_6["BackgroundRisk"]) # ~0.47
    
  return(rho)
}

