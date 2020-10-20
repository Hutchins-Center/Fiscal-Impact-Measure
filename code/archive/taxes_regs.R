source("fim.R")
library(stargazer)


# get log levels
taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
history$gstx = rowSums(history[,taxpieces], na.rm = T)

comp = c("gdp", "c",   "ch",  "gdph","yptmr" ,"yptmd" ,"g", "gfrcf",  "gf", "gs", "gfh", "gsh", "gfrpt", "gfrpri" ,"gfrcp" ,"gfrs", "gftfp", "gsrpt" ,"gsrpri", "gsrcp" ,"gsrs","gstx",  "gstfp", "gdppothq", "dc", "gfeg")

history[,paste0(comp, "_qa")] = sapply(history[,comp], function(x) q_a(x))
history[,paste0(comp, "_ln")] = sapply(history[,comp], function(x) log(x))

# test how well we would have forecasted state and local revenues using CBO's projections of state and local purchases, starting in april 2018 
cbo_proj_gs_g = c(6.4, 5.2,	4.3,	4.8,	5.0)  # from April 2018 10-year economic projections, nominal growth in state and local consumption and investment.
dates = as.Date(c("2017-12-31", "2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31"))
gstx_est1 = numeric(5)
gstx_est1[1] = history$gstx[which(history$date == dates[1])-1]*(1+(cbo_proj_gs_g[1]/100))
for(i in 2:length(dates)){
  gstx_est1[i] = history$gstx[which(history$date == dates[i])-1]* (1+(cbo_proj_gs_g[i]/100))
}
history$gstx_est1 = gstx_est1[match(history$date, dates)]
history$cbo_proj_gs_g = cbo_proj_gs_g[match(history$date, dates)]

lp(history[,c("date", "gstx_est1", "gstx")], labels = c("Estimated using CBO projected growth rates in purchases", "realized"), start.date = "2017-01-01", end.date = dates[4])
lp(history[,c("date", "gs_qa", "cbo_proj_gs_g")], labels = c("Realized growth", "CBO projection"), start.date = "2017-01-01", end.date = dates[4])


# test how well we would have estimated state and local revenues using state and local purchases growth
# remark on fed transfers
history$est_revenues = history$gs + history$gfeg
history$est_revenues_ln = log(history$est_revenues)
history$est_revenues_qa = q_a(history$est_revenues)

grid.arrange(ggplot(history,aes(gs_qa, gstx_qa)) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Purchases growth vs. tax revenue growth (all sources)",  x = "Purchases", y = "Tax revenues", subtitle = "Quarterly annualized growth rates (%)"), 
             ggplot(history,aes(gs_ln, gstx_ln)) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Log purchases vs. log tax revenues",   x = "Purchases", y = "Tax revenues", subtitle = "Log levels"),
             ggplot(history,aes(gs_qa, est_revenues_qa)) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Purchases growth vs. revenues growth",   x = "Purchases", y = "Revenues", subtitle = "Quarterly annualized growth rates (%). \nRevenues include own tax revenues and federal grants"),
             ggplot(history,aes(gs_ln, est_revenues_ln)) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Log purchases vs. log revenues",   x = "Purchases", y = "Revenues", subtitle = "Log levels.  Revenues include own tax revenues and federal grants"),
             nrow = 2, ncol = 2)
# predictions

est = c(paste0(taxpieces, "_est"))
preddates = which(history$date>="2016-01-01")
for(i in 1:length(est)){
  # history[-preddates,est[i]] = history[-preddates,taxpieces[i]]
  for(j in preddates){
    history[j,est[i]] = history[j-1,taxpieces[i]]*(1+(history[j,"gs_qa"]/100))
  }
}

library(gridExtra)
grid.arrange(
  ggplot(history[preddates,],aes_string(taxpieces[1], est[1])) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Income taxes, actual vs. predicted", x="Actual ($)", y="Predicted ($)", subtitle = "Levels ($) predicted using growth in state and local purchases"), 
  ggplot(history[preddates,],aes_string(taxpieces[2], est[2])) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Production & import taxes, actual vs. predicted", x="Actual ($)", y="Predicted ($)", subtitle = "Levels ($) predicted using growth in state and local purchases"), 
  ggplot(history[preddates,],aes_string(taxpieces[3], est[3])) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Corporate taxes, actual vs. predicted", x="Actual ($)", y="Predicted ($)", subtitle = "Levels ($) predicted using growth in state and local purchases"), 
  ggplot(history[preddates,],aes_string(taxpieces[4], est[4])) + geom_point() + geom_smooth(method='lm') + uni.theme + labs(title = "Payroll taxes, actual vs. predicted", x="Actual ($)", y="Predicted ($)", subtitle = "Levels ($) predicted using growth in state and local purchases"), nrow=2, ncol=2)



# Some exploratory regressions on state and local taxes, transfers
lm1 <- lm(gstx_ln ~ shift(gstx_ln, 1) + shift(gstx_ln, 2) , history)
lm2 <- lm(gstx_ln ~ shift(gstx_ln, 1) + shift(gstx_ln, 2)  + gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2) , history)
lm3 <- lm(gstx_ln ~ shift(gstx_ln, 1) + shift(gstx_ln, 2)  + gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2)  + gdph_ln + shift (gdph_ln, 1) + shift(gdph_ln, 2), history)
lm8 <- lm(gstx_ln ~ gfeg_ln + shift(gfeg_ln, 1) + shift(gfeg_ln, 2) + gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2), history)
lm9 <- lm(gstx_ln ~ ch_ln + shift(ch_ln, 1) + shift(ch_ln, 2) + gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2), history)


lm4 <- lm(gsrpri_ln ~ gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2)  + shift(gsrpri_ln, 1) + shift(gsrpri_ln, 2) , history)
lm5 <- lm(gsrcp_ln ~ gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2)  + shift(gsrcp_ln, 1) + shift(gsrcp_ln, 2) , history)
# Payroll taxes don't appear to be super related to expenditures (makes sense)
lm6 <- lm(gsrs_ln ~  gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2)  + shift(gsrs_ln, 1) + shift(gsrs_ln, 2) , history)
lm7 <- lm(gsrpt_ln ~  gs_ln + shift(gs_ln, 1) + shift(gs_ln, 2)  + shift(gsrpt_ln, 1) + shift(gsrpt_ln, 2) , history)


summary(lm8)


stargazer()

stargazer(lm1, lm2, lm3, dep.var.labels = "Log S&L tax revenues (t)", covariate.labels = c("Log S&L tax revenues (t-1)", "Log S&L tax revenues (t-2)", "Log S&L purchases (t)" , "Log S&L purchases (t-1)", "Log S&L purchases (t-2)",  "Log real GDP (t)", "Log real GDP (t-1)","Log real GDP (t-2)"), out = "sl_taxes_reg1.html")
stargazer(lm4, lm5, lm6, lm7, dep.var.labels = c("Production/Import Taxes", "Corporate Taxes", "Payroll Taxes", "Income Taxes"), covariate.labels = c("Log S&L purchases (t)" , "Log S&L purchases (t-1)", "Log S&L purchases (t-2)", "Log S&L purchases (t-3)", "Log S&L tax revenues (t-1)", "Log S&L tax revenues (t-2)", "Log S&L tax revenues (t-3)", "Log S&L tax revenues (t-1)", "Log S&L tax revenues (t-2)", "Log S&L tax revenues (t-3)", "Log S&L tax revenues (t-1)", "Log S&L tax revenues (t-2)", "Log S&L tax revenues (t-3)", "Log S&L tax revenues (t-1)", "Log S&L tax revenues (t-2)", "Log S&L tax revenues (t-3)"), out = "sl_taxes_reg2.html")