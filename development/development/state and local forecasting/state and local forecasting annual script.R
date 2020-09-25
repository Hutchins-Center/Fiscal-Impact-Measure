source("fim.R")
library(xtable)
library(stargazer)
library(dplyr)

# annual data 
series1 = c("GDP", "C","CH","GDPH","JC", "JGDP", "JGF", "JGS", "PTGH","PTGSH","PTGFH", "YPTMR", "YPTMD", "GTFP", "YPOG", "YPTX", "YTPI", "YCTLG", "G", "GFRCF", "GRCSI", "GDPH", "DC",	"PTGFH", "PTGSH", "GF", "GS", "GFH", "GSH", "	GFRPT", "GFRPRI", "GFRCP", "GFRS","GFRPT","	GFRPRI","	GFRCP","	GFRS","	GFTFP","	GFEG","	GSRPT","	GSRPRI","	GSRCP","	GSRS","	GSTFP","	GSET", "YP")
series2 = c("USPHPI", "CASUSXAM", "FNMFHFA", "GDPPOT", "GDPPOTH")
data1 = haver.data(series1, "usna", start = as.Date("1970-01-01"), frequency = "annual")
data2 = haver.data(series2, "usecon", start = as.Date("1970-01-01"), frequency = "annual")
data1 <- data.frame(date = as.Date(rownames(data1), f="%Y"), data1)
data2 <- data.frame(date = as.Date(rownames(data2), f="%Y"), data2)

xx = merge(data1, data2, by = "date")

#function to take y/y growth
yy <- function(x){
  j=c()
  for(i in 5:length(x)){
    j[i] = (-1 + (x[i]/(x[i-1])))*100
  }
  j[1:4] = NA
  j
}
#function to detrend data
detrend <- function(x){
  t <- 1:length(x)
  t2 <- t^2
  dat <- data.frame(x,t,t2)
  tfit <- lm(x~t, dat)
  # tfit <- lm(x~t+t2, dat)
  j = x-predict(tfit)
  j[length(j)] = NA 
  j
}
# calculate output gaps
xx$gdpgap = (xx$gdp-xx$gdppot)/xx$gdppot
xx$gdpgap_h = (xx$gdph-xx$gdppoth)/xx$gdppoth

taxpieces = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs")
xx$gstx = rowSums(xx[,taxpieces], na.rm = T) # tax revenue total
xx$gsnet = xx$gs - xx$gfeg # state and local purchases less federal grants
xx$fshare = fmap$fshare[match(as.character(format(xx$date, f="%Y")),fmap$year )]
xx$yfptmd = xx$yptmd*(xx$fshare) # federal medicaid expenditures
xx$gstfpnet = xx$gstfp - xx$yfptmd # state and local transfers = state and local social benefits - medicaid grants from fed gov't
# xx$hpx = xx$casusxam
xx$hpx = xx$usphpi

# calculate tax rates
xx$taxes_gdp = xx$gstx / xx$gdp
xx$taxes_yp = xx$gstx / xx$yp
xx[,paste0(taxpieces, "_gdp")] = sapply(xx[,taxpieces], function(x) x/xx$gdp)
xx[,paste0(taxpieces, "_yp")] = sapply(xx[,taxpieces], function(x) x/xx$yp)
# calculate shares of gdp
xx$hpx_gdp = xx$hpx / xx$gdp
xx$hpx_yp = xx$hpx / xx$yp


# linear time trend var
xx$t = 1:nrow(xx)
comp = c("gdp", "c",   "ch",  "gdph","yptmr" ,"yptmd" ,"g", "gfrcf",  "gf", "gs","gsnet", "gfh", "gsh", "gfrpt", "gfrpri" ,"gfrcp" ,"gfrs", "gftfp", "gsrpt" ,"gsrpri", "gsrcp" ,"gsrs","gstx",  "gstfp","gstfpnet", "gfeg", "hpx", "yp", "gdpgap", "gdpgap_h", "hpx_yp", "hpx_gdp", "taxes_yp", "taxes_gdp", paste0(taxpieces, "_gdp"), paste0(taxpieces, "_yp"))
reals = paste0(comp, "_h")
detrended = paste0(comp, "_dt")
diffs = paste0(comp, "_d")

xx[,paste0(comp, "_yy")] = sapply(xx[,comp], function(x) yy(x))

xx[,paste0(comp, "_l1")] = sapply(xx[,comp], function(x) shift(x, n=1))
xx[,paste0(comp, "_l2")] = sapply(xx[,comp], function(x) shift(x, n=2))
xx[,paste0(comp, "_l3")] = sapply(xx[,comp], function(x) shift(x, n=3))
xx[,paste0(comp, "_l5")] = sapply(xx[,comp], function(x) shift(x, n=5))

xx[,reals] = sapply(xx[,comp], function(x) 100*x/xx$jc)
# gdp has its own deflator
xx$gdp_h = xx$gdph 
xx$gdpgap_h = (xx$gdph-xx$gdppoth)/xx$gdppoth
# calculate tax rates, real
xx$taxes_gdp_h = xx$gstx_h / xx$gdp_h
xx$taxes_yp_h = xx$gstx_h / xx$yp_h
# calculate shares of gdp, real
xx$hpx_gdp_h = xx$hpx_h / xx$gdp_h
xx$hpx_yp_h = xx$hpx_h / xx$yp_h

xx[,paste0(reals, "_l1")] = sapply(xx[,reals], function(x) shift(x, n=1))
xx[,paste0(reals, "_l2")] = sapply(xx[,reals], function(x) shift(x, n=2))
xx[,paste0(reals, "_l3")] = sapply(xx[,reals], function(x) shift(x, n=3))
xx[,paste0(reals, "_l5")] = sapply(xx[,reals], function(x) shift(x, n=5))

xx[,detrended] = sapply(xx[,comp], function(x) detrend(x))
xx$hpx_dt = xx$hpx # house prices shouldn't be detrended
xx[,paste0(detrended, "_l1")] = sapply(xx[,detrended], function(x) shift(x, n=1))
xx[,paste0(detrended, "_l2")] = sapply(xx[,detrended], function(x) shift(x, n=2))
xx[,paste0(detrended, "_l3")] = sapply(xx[,detrended], function(x) shift(x, n=3))
xx[,paste0(detrended, "_l5")] = sapply(xx[,detrended], function(x) shift(x, n=5))

xx[,diffs] = sapply(xx[,comp], function(x) c(NA, diff(x)))
xx[,paste0(diffs, "_l1")] = sapply(xx[,diffs], function(x) shift(x, n=1))
xx[,paste0(diffs, "_l2")] = sapply(xx[,diffs], function(x) shift(x, n=2))
xx[,paste0(diffs, "_l3")] = sapply(xx[,diffs], function(x) shift(x, n=3))
xx[,paste0(diffs, "_l5")] = sapply(xx[,diffs], function(x) shift(x, n=5))


regs <- function(y,x, d=xx){
  spec = as.formula(paste0(y,"~",paste(x,collapse = "+")))
  lm(spec,d)
}

noms_x <- list(c("t", "gdp", "gdp_l1", "gdp_l2"), 
                c("t","c", "c_l1", "c_l2"),
                c("t","yp", "yp_l1", "yp_l2"),
                c("t","hpx", "hpx_l1", "hpx_l3", "hpx_l5"))
reals_x <- list(c("t","gdp_h", "gdp_h_l1", "gdp_h_l2"), 
               c("t","c_h", "c_h_l1", "c_h_l2"),
               c("t","yp_h", "yp_h_l1", "yp_h_l2"),
               c("t","hpx_h", "hpx_h_l1", "hpx_h_l3", "hpx_h_l5"))
dt_x <- list(c("gdp_dt", "gdp_dt_l1", "gdp_dt_l2"), 
               c("c_dt", "c_dt_l1", "c_dt_l2"),
               c("yp_dt", "yp_dt_l1", "yp_dt_l2"),
               c("hpx_dt", "hpx_dt_l1", "hpx_dt_l3", "hpx_dt_l5"))
d_x <- list(c("gdp_d", "gdp_d_l1", "gdp_d_l2"), 
             c("c_d", "c_d_l1", "c_d_l2"),
             c("yp_d", "yp_d_l1", "yp_d_l2"),
             c("hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"))

noms_allx <- append(noms_x, list(unique(unlist(noms_x))))
reals_allx <- append(reals_x, list(unique(unlist(reals_x))))
dt_allx <- append(dt_x, list(unique(unlist(dt_x))))
d_allx <- append(d_x, list(unique(unlist(d_x))))

# run regressions
nom_fits <- lapply(noms_allx, function(X){regs(y="gstx", x=X)})
real_fits <- lapply(reals_allx, function(X){regs(y="gstx_h", x=X)})
dt_fits <- lapply(dt_allx, function(X){regs(y="gstx_dt", x=X)})
d_fits <- lapply(d_allx, function(X){regs(y="gstx_d", x=X)})

nom_fits_gsrpt <- lapply(noms_allx, function(X){regs(y="gsrpt", x=X)})
real_fits_gsrpt <- lapply(reals_allx, function(X){regs(y="gsrpt_h", x=X)})
d_fits_gsrpt <- lapply(d_allx, function(X){regs(y="gsrpt_d", x=X)})

nom_fits_gsrpri <- lapply(noms_allx, function(X){regs(y="gsrpri", x=X)})
real_fits_gsrpri  <- lapply(reals_allx, function(X){regs(y="gsrpri_h", x=X)})
d_fits_gsrpri <- lapply(d_allx, function(X){regs(y="gsrpri_d", x=X)})

# lapply(pieces_gdp_rates_fits, function(x) summary(x))

lag_test <- list(c("gdp_dt"),
                 c("gdp_dt", "gdp_dt_l1"),
                 c("gdp_dt", "gdp_dt_l1", "gdp_dt_l2"))

# regs of tax rate on house prices, output gap
gdp_rates <- list(c("hpx_gdp", "hpx_gdp_l1","hpx_gdp_l3","hpx_gdp_l5"), 
              c("hpx_gdp", "hpx_gdp_l1","hpx_gdp_l3","hpx_gdp_l5", "gdpgap", "gdpgap_l1", "gdpgap_l2"))
gdp_rates_d <- list(c("hpx_gdp_d", "hpx_gdp_d_l1","hpx_gdp_d_l3","hpx_gdp_d_l5"), 
                                 c("hpx_gdp_d", "hpx_gdp_d_l1","hpx_gdp_d_l3","hpx_gdp_d_l5", "gdpgap_d", "gdpgap_d_l1", "gdpgap_d_l2"))
gdp_rates_h <- list(c("hpx_gdp_h", "hpx_gdp_h_l1","hpx_gdp_h_l3","hpx_gdp_h_l5"), 
                    c("hpx_gdp_h", "hpx_gdp_h_l1","hpx_gdp_h_l3","hpx_gdp_h_l5", "gdpgap_h", "gdpgap_h_l1", "gdpgap_h_l2"))

yp_rates <- list(c("hpx_yp", "hpx_yp_l1","hpx_yp_l3","hpx_yp_l5"), 
                  c("hpx_yp", "hpx_yp_l1","hpx_yp_l3","hpx_yp_l5", "gdpgap", "gdpgap_l1", "gdpgap_l2"))
yp_rates_d <- list(c("hpx_yp_d", "hpx_yp_d_l1","hpx_yp_d_l3","hpx_yp_d_l5"),
                               c("hpx_yp_d", "hpx_yp_d_l1","hpx_yp_d_l3","hpx_yp_d_l5", "gdpgap_d", "gdpgap_d_l1", "gdpgap_d_l2"))
yp_rates_h <- list(c("hpx_yp_h", "hpx_yp_h_l1","hpx_yp_h_l3","hpx_yp_h_l5"), 
                               c("hpx_yp_h", "hpx_yp_h_l1","hpx_yp_h_l3","hpx_yp_h_l5", "gdpgap_h", "gdpgap_h_l1", "gdpgap_h_l2"))

gdp_rates_fits <- lapply(gdp_rates, function(X){regs(y="taxes_gdp", x=X)})
gdp_rates_fits_d <- lapply(gdp_rates_d, function(X){regs(y="taxes_gdp_d", x=X)})
gdp_rates_fits_h <- lapply(gdp_rates_h, function(X){regs(y="taxes_gdp_h", x=X)})
yp_rates_fits <- lapply(yp_rates, function(X){regs(y="taxes_yp", x=X)})
yp_rates_fits_d <- lapply(yp_rates_d, function(X){regs(y="taxes_yp_d", x=X)})
yp_rates_fits_h <- lapply(yp_rates_h, function(X){regs(y="taxes_yp_h", x=X)})
pieces_gdp_rates_fits <- lapply(paste0(taxpieces, "_gdp"), function(Y){regs(y=Y, x=gdp_rates[[2]])})
pieces_gdp_rates_fits_d <- lapply(paste0(taxpieces, "_gdp_d"), function(Y){regs(y=Y, x=gdp_rates_d[[2]])})

# Produce some out-of-sample forecasts
samp = xx$date<="2010-01-01"
insamp = xx[which(samp),]
outsamp = xx[which(!samp),]

# run regressions on restricted sample
nom_fits0 <- lapply(noms_allx, function(X){regs(y="gstx", x=X, d=insamp)})
real_fits0 <- lapply(reals_allx, function(X){regs(y="gstx_h", x=X, d=insamp)})
dt_fits0 <- lapply(dt_allx, function(X){regs(y="gstx_dt", x=X, d=insamp)})
d_fits0 <- lapply(d_allx, function(X){regs(y="gstx_d", x=X, d=insamp)})
nom_fits_gsrpt0 <- lapply(noms_allx, function(X){regs(y="gsrpt", x=X, d=insamp)})
real_fits_gsrpt0 <- lapply(reals_allx, function(X){regs(y="gsrpt_h", x=X, d=insamp)})
d_fits_gsrpt0 <- lapply(d_allx, function(X){regs(y="gsrpt_d", x=X, d=insamp)})
nom_fits_gsrpri0 <- lapply(noms_allx, function(X){regs(y="gsrpri", x=X, d=insamp)})
real_fits_gsrpri0  <- lapply(reals_allx, function(X){regs(y="gsrpri_h", x=X, d=insamp)})
d_fits_gsrpri0 <- lapply(d_allx, function(X){regs(y="gsrpri_d", x=X, d=insamp)})
gdp_rates_fits0 <- lapply(gdp_rates, function(X){regs(y="taxes_gdp", x=X, d=insamp)})
gdp_rates_fits_d0 <- lapply(gdp_rates_d, function(X){regs(y="taxes_gdp_d", x=X, d=insamp)})
gdp_rates_fits_h0 <- lapply(gdp_rates_h, function(X){regs(y="taxes_gdp_h", x=X, d=insamp)})
yp_rates_fits0 <- lapply(yp_rates, function(X){regs(y="taxes_yp", x=X, d=insamp)})
yp_rates_fits_d0 <- lapply(yp_rates_d, function(X){regs(y="taxes_yp_d", x=X, d=insamp)})
yp_rates_fits_h0 <- lapply(yp_rates_h, function(X){regs(y="taxes_yp_h", x=X, d=insamp)})
pieces_gdp_rates_fits0 <- lapply(paste0(taxpieces, "_gdp"), function(Y){regs(y=Y, x=gdp_rates[[2]], d=insamp)})
pieces_gdp_rates_fits_d0 <- lapply(paste0(taxpieces, "_gdp_d"), function(Y){regs(y=Y, x=gdp_rates_d[[2]], d=insamp)})

# out of sample predictions

outcast <- function(y, x, d=xx, sample){
  insamp = d[which(sample),]
  outsamp = d[which(!sample),]
  fit = regs(y,x,insamp)
  outdat = d[,c("date", y)]
  outdat[which(!sample),paste0(y,"_out")] = predict(fit,outsamp)
  # outdat$residuals = outdat[,y] - outdat[,paste0(y,"_out")]
  outdat$y_name = y
  melt(outdat, id = c("date", "y_name"))
}

pieces_gdp_rates_fits_d0 <- Reduce(rbind,lapply(paste0(taxpieces, "_gdp_d"), function(Y){outcast(y=Y, x=gdp_rates_d[[2]], sample = c(xx$date<="2010-01-01"))}))
ggplot(pieces_gdp_rates_fits_d0, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + facet_wrap(~y_name) + uni.theme
pieces_gdp_rates_fits0 <- Reduce(rbind,lapply(paste0(taxpieces, "_gdp"), function(Y){outcast(y=Y, x=gdp_rates[[2]], sample = c(xx$date<="2010-01-01"))}))
ggplot(pieces_gdp_rates_fits0, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + facet_wrap(~y_name) + uni.theme
gdp_rates_fits0 <- outcast(y="taxes_gdp", x=gdp_rates[[2]], sample = c(xx$date<="2010-01-01"))
ggplot(gdp_rates_fits0, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + facet_wrap(~y_name) + uni.theme


preffit0 <- regs(y="gstx", x=c("t", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"))
preffit0o <- outcast(y="gstx", x=c("t", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"), sample = c(xx$date<="2009-01-01"))
ggplot(preffit0o, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + uni.theme

preffit1 <- regs(y="gstx", x=c("t", "gstx_l1", "gstx_l2", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"))
preffit1o <- outcast(y="gstx", x=c("t", "gstx_l1", "gstx_l2", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"), sample = c(xx$date<="2009-01-01"))
ggplot(preffit1o, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + uni.theme

preffit0 <- regs(y="gstx_d", x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"))
preffit0o <- outcast(y="gstx_d", x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"), sample = c(xx$date<="2009-01-01"))
ggplot(preffit0o, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + uni.theme

preffit1 <- regs(y="gstx_d", x=c("gstx_d_l1", "gstx_d_l2", "gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"))
preffit1o <- outcast(y="gstx_d", x=c("gstx_d_l1", "gstx_d_l2", "gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"), sample = c(xx$date<="2009-01-01"))
ggplot(preffit1o, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + uni.theme


preffitpieces <- lapply(taxpieces, function(X){regs(y=X, x=c("t", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"))})
preffitpieces <- lapply(preffitpieces, function(x) summary(x))
names(preffitpieces) = taxpieces

preffitpieces <- Reduce(rbind,lapply(taxpieces, function(X){outcast(y=X, x=c("t", "gdp", "gdp_l1", "gdp_l2","hpx", "hpx_l1", "hpx_l3", "hpx_l5"), sample = c(xx$date<="2009-01-01"))}))
ggplot(preffitpieces, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + facet_wrap(~y_name) + uni.theme



taxpieces_d = paste0(taxpieces, "_d")
preffitpieces <- lapply(taxpieces_d, function(X){regs(y=X, x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"))})
preffitpieces <- lapply(preffitpieces, function(x) summary(x))
names(preffitpieces) = taxpieces_d


preffitpieces <- Reduce(rbind,lapply(taxpieces_d, function(X){outcast(y=X, x=c("gdp_d", "gdp_d_l1", "gdp_d_l2","hpx_d", "hpx_d_l1", "hpx_d_l3", "hpx_d_l5"), sample = c(xx$date<="2009-01-01"))}))

ggplot(preffitpieces, aes(x=date, y=value, group = variable)) + geom_line(aes(color=variable)) + facet_wrap(~y_name) + uni.theme




