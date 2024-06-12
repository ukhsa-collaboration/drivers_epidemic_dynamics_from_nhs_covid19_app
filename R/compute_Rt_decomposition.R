compute_Rt_decomposition <- function(national.data.full, save.plots.as.pdf, save.plots.as.png) {

  national.data.full.original <- national.data.full
  
  # compute and save some key stats. Note I have hard-coded "[1:822]" to discard the last few "NA" entries
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(cor( log(national.data.full$CRxTPAEN)[1:822], log(national.data.full$contact_rate)[1:822] )^2),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(cor( log(national.data.full$CRxTPAEN)[1:822], log(national.data.full$TPAEN)[1:822] )^2),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cor( log(national.data.full$TPAEN)[1:822], log(national.data.full$contact_rate)[1:822] ))
  ),
  file="results/contributions_to_Rt_overall.txt")

  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(var(log(national.data.full$contact_rate)[1:822]) / var(log(national.data.full$CRxTPAEN)[1:822]) ),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(var(log(national.data.full$TPAEN)[1:822]) / var(log(national.data.full$CRxTPAEN)[1:822]) ),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cov(log(national.data.full$TPAEN)[1:822], log(national.data.full$contact_rate)[1:822]) / var(log(national.data.full$CRxTPAEN)[1:822]))
  ),
  file="results/contributions_to_Rt_overall_decomposed.txt")

  national.data.full <- national.data.full.original %>%
    filter(date < as.Date("2022-03-01"))
  
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(cor( log(national.data.full$CRxTPAEN), log(national.data.full$contact_rate) )^2),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(cor( log(national.data.full$CRxTPAEN), log(national.data.full$TPAEN) )^2),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cor( log(national.data.full$TPAEN), log(national.data.full$contact_rate) ))
  ),
  file="results/contributions_to_Rt_20212022.txt")
  
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(var(log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)) ),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(var(log(national.data.full$TPAEN)) / var(log(national.data.full$CRxTPAEN)) ),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cov(log(national.data.full$TPAEN), log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)))
  ),
  file="results/contributions_to_Rt_20212022_decomposed.txt")
  
  national.data.full <- national.data.full.original %>%
    filter(date < as.Date("2021-12-01"))
  
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(cor( log(national.data.full$CRxTPAEN), log(national.data.full$contact_rate) )^2),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(cor( log(national.data.full$CRxTPAEN), log(national.data.full$TPAEN) )^2),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cor( log(national.data.full$TPAEN), log(national.data.full$contact_rate) ))
  ),
  file="results/contributions_to_Rt_preOmicron.txt")
  
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(var(log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)) ),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(var(log(national.data.full$TPAEN)) / var(log(national.data.full$CRxTPAEN)) ),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cov(log(national.data.full$TPAEN), log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)))
  ),
  file="results/contributions_to_Rt_preOmicron_decomposed.txt")
  
  var_decomposed<-tibble(period="pre-Omicron",
                         component=c("log CR","log TPAEN","interaction"),
                         `contribution to variance of log Rt`=c(
                           var(log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)),
                           var(log(national.data.full$TPAEN)) / var(log(national.data.full$CRxTPAEN)),
                           2*cov(log(national.data.full$TPAEN), log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN))
                         )
                         )
  
  national.data.full <- national.data.full.original %>%
    filter(date >= as.Date("2021-12-01") & date <= as.Date("2023-03-15"))
  
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(cor( log(national.data.full$CRxTPAEN), log(national.data.full$contact_rate) )^2),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(cor( log(national.data.full$CRxTPAEN), log(national.data.full$TPAEN) )^2),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cor( log(national.data.full$TPAEN), log(national.data.full$contact_rate) ))
  ),
  file="results/contributions_to_Rt_Omicron.txt")
  
  write_lines(c(
    paste0("Fraction of variance of Rt explained by changes in contact rates:"),
    paste0(var(log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)) ),
    paste0("Fraction of variance of Rt explained by changes in TPAEN:"),
    paste0(var(log(national.data.full$TPAEN)) / var(log(national.data.full$CRxTPAEN)) ),
    paste0("Relation between TPAEN and contact rates:"),
    paste0(cov(log(national.data.full$TPAEN), log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)))
  ),
  file="results/contributions_to_Rt_Omicron_decomposed.txt")
  
  var_decomposed<-bind_rows(var_decomposed,tibble(period="Omicron",
                         component=c("log CR","log TPAEN","interaction"),
                         `contribution to variance of log Rt`=c(
                           var(log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN)),
                           var(log(national.data.full$TPAEN)) / var(log(national.data.full$CRxTPAEN)),
                           2*cov(log(national.data.full$TPAEN), log(national.data.full$contact_rate)) / var(log(national.data.full$CRxTPAEN))
                         )
                         )
  )

  # prepare and save Supplementary figure S3
  plot_var_decomposed <- var_decomposed %>% mutate(component=factor(component,levels=c("log CR","log TPAEN","interaction")),
                            period=factor(period,levels=c("pre-Omicron","Omicron"))) %>%
    ggplot(aes(x=component)) + 
    geom_col(aes(y=100*`contribution to variance of log Rt`,fill=component)) + 
    geom_hline(yintercept = 0) +
    scale_fill_manual(values=c("log CR"="#66c2a5","log TPAEN"="#fc8d62","interaction"="grey")) +
    ylab('% contribution to variance of log Rt') + 
    theme_classic() +
    facet_wrap(~period,ncol=2) +
    theme(legend.position = "none")
    
  if (save.plots.as.png) ggsave(plot_var_decomposed, device = "png", file="plots/plot_var_decomposed.png",height = 5,width=7)
  if (save.plots.as.pdf) ggsave(plot_var_decomposed, device = "pdf", file="plots/plot_var_decomposed.pdf",height = 5,width=7)
  
}

