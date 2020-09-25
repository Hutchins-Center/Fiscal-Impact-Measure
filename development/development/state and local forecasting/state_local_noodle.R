source("fim_figures.R")

xxsave = xx

xx = xxsave
series4 <- c("LE", "LR", "LXGFML", "LXGSML", "LASGOVA",	"LALGOVA",	"LAFGVXA",	"LAFGT")
data4 <- pull_data(series4, "usecon", start.date = as.Date("1980-01-01"))
xx <- merge(xx, data4, by = "date")

xx <- mutate(xx,
             ls = lasgova + lalgova, 
             lf = lafgvxa - lafgt,
             le = le - lf - ls, 
             gsh = gs / jgs, 
             gfh = gf / jgf, 
             gstx = gsrpri + gsrs + gsrpt + gsrcp, 
             gftx = gfrpri + gfrs + gfrpt + gfrcp, 
             gstxh = gstx/jc, 
             gftxh = gftx/jc)

series4 <- c( "ls", "lf", "le")
series5 <- c("gsh", "gfh", "gdph")
series6 <- c("gs", "gf", "gdp")
series7 <- c("gstx", "gftx", "c")
series8 <- c("gstxh", "gftxh", "ch")

colorz = c("blue", "red", "green")

xx[,paste0(c(series4,series5,series6, series7, series8), "_index")] = lapply(xx[,c(series4,series5,series6, series7, series8)], function(x) 100*x/x[xx$date == "2008-03-31"])

lp(xx[,c("date", paste0(series4, "_index"))], labelz = c("State and local", "Federal", "Private"), colorz = colorz, start.date = "2000-01-01", end.date = "2018-01-01", sub = "Index, 2008 = 100", t = "Total employment in sector -- ex. Census and Postal workers")
lp(xx[,c("date", paste0(series5, "_index"))], labelz = c("state and local", "federal", "gdp"), colorz = colorz, start.date = "2000-01-01", end.date = "2018-01-01", sub = "Index, 2008 = 100", t = "Real GDP and purchases")
lp(xx[,c("date", paste0(series6, "_index"))], labelz = c("state and local", "federal", "gdp"), colorz = colorz, start.date = "2000-01-01", end.date = "2018-01-01", sub = "Index, 2008 = 100", t = "Nominal GDP and purchases")

lp(xx[,c("date", paste0(series7, "_index"))], labelz = c("state and local", "federal", "c"), colorz = colorz, start.date = "2000-01-01", end.date = "2018-01-01", sub = "Index, 2008 = 100", t = "Nominal Tax Revenues and consumption")
lp(xx[,c("date", paste0(series8, "_index"))], labelz = c("state and local", "federal", "c"), colorz = colorz, start.date = "2000-01-01", end.date = "2018-01-01", sub = "Index, 2008 = 100", t = "Real Tax Revenues and consumption")
