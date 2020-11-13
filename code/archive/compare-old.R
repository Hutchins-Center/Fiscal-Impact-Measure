library(zoo)
# define first sample test dates
last_q = thismonth = "2018-12-31"
first_proj = "2019-03-31"
# source("fim_calculations.R")

# define second sample test dates
last_q = nxtmonth = "2019-03-31"
first_proj = "2019-6-30"
# source("fim_calculations.R")


# define daf of interest
daf = "fim-projections-"
# daf = "xx-"

thismonth_folder <- paste0(as.character(format(as.yearmon(thismonth), f = "%m-%Y")), "-old")
this <- read.csv(paste0(thismonth_folder, "/", daf, thismonth,".csv"), stringsAsFactors = F)
this$projection_date = thismonth_folder

nxtmonth_folder <- paste0(as.character(format(as.yearmon(nxtmonth), f = "%m-%Y")), "-old")
nxt <- read.csv(paste0(nxtmonth_folder, "/", daf, nxtmonth,".csv"), stringsAsFactors = F)
nxt$projection_date = nxtmonth_folder

mast <- rbind(this, nxt)
mast$date = as.Date(mast$date)

library(dplyr)
mast <- filter(mast, date >= "2017-01-01" & date <= "2022-01-01")


comps  = grep("cont", colnames(mast), value = T)
# comps = c("yptmd", "yptmr", "gtfp", "gftfp","gstfp", "ysptmd", "yfptmd","gftfpnet", "gstfpnet")
# comps = c("fshare", "yptmr","yptmd","yfptmd","gfegnet",  "gftfp", "gftfpnet", "gstfpnet", "ysptmd", "yfptmd", "gtfp")
thismonth = as.Date(thismonth)
nxtmonth = as.Date(nxtmonth)

library(ggplot2)
library(wesanderson)
uni.theme = theme_bw() + theme(legend.position = "bottom", legend.margin = unit(c(rep(-.8, 4)),"cm"), panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank() , plot.margin=unit(c(1.2,.5,.5,.5),"cm") , plot.title = element_text(size=12), plot.subtitle = element_text(size=10) , plot.caption = element_text(size = 7), legend.text=element_text(size=10))
colorz = c(wes_palette("Royal2")[5],  "grey", sort(wes_palette("Darjeeling1"))[1],wes_palette("Royal2")[3], wes_palette("Darjeeling1"))


lapply(comps, function(x){
  ggplot(mast, aes_string(x = "date", y = x, fill = "projection_date")) + uni.theme + 
    geom_rect(data = mast, aes(xmin = as.Date(thismonth) + 90, xmax = max(mast$date, na.rm = T), ymin=-Inf, ymax=+Inf), fill = colorz[1], alpha = 0.004) +
    geom_rect(data = mast, aes(xmin = as.Date(nxtmonth) + 90, xmax = max(mast$date, na.rm = T), ymin=-Inf, ymax=+Inf), fill = colorz[2], alpha = 0.007) + 
    # geom_bar() +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=colorz) + labs(title = x) + 
    geom_vline(xintercept = as.Date(thismonth), color = colorz[1]) + geom_vline(xintercept = as.Date(nxtmonth), color = colorz[2])
  
})

# nxt_growth = this$gftfp_g[which(this$date == nxtmonth)]
# this_level = this$gftfp[which(this$date == thismonth)]
# nxt_level = nxt$gftfp[which(nxt$date == nxtmonth)]
# 
# this_level*(1+nxt_growth) - nxt_level

