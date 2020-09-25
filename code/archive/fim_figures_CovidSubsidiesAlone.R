# Plot figures
source("fim_calculations.R")

library(ggplot2)
library(gridExtra)
library(grid)
library(wesanderson)

# Do you want to render the output to PDFs and JPGs?
render = T

# set plot formats, theme 
uni.theme = theme_bw() + theme(legend.position = "bottom", panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank() , plot.margin=unit(c(1.2,.5,.5,.5),"cm") , plot.title = element_text(size=12, face = "bold"), plot.subtitle = element_text(size=10) , plot.caption = element_text(size = 9), legend.text=element_text(size=10), legend.title=element_blank(),legend.spacing.y = unit(2, 'cm')) # , legend.margin = unit(c(rep(-.8, 4)),"cm")

guidez =  guides(fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"), ncol = 1), colour = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.05, "cm"), ncol = 1))

taxes_transfers_green = rgb(27, 149, 83,  maxColorValue = 255)
subsidies_orange = rgb(218, 165, 32, maxColorValue = 255)
dark_golden_rod = rgb(184,134,11, maxColorValue = 255)
state_local_purple = rgb(174, 104, 169,  maxColorValue = 255)
federal_blue = rgb(33, 152, 199,  maxColorValue = 255)
total_pink = rgb(231, 97, 159, maxColorValue = 255)
colourz = c("black",taxes_transfers_green, subsidies_orange, state_local_purple, federal_blue, total_pink, wes_palette("Royal2"), wes_palette("Royal2"))


# helper function for line plots
lp <- function(data, labelz = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = last_proj_date, colorz = colourz) {
  df_plot <- data
  colnames(df_plot) = c("date", labelz)
  df_plot <- melt(df_plot, id = "date")
  df_plot <- df_plot[which(df_plot$date <= end.date & df_plot$date >= start.date), ]
  
  leg_exists = NULL
  if(is.null(labelz)){
    leg_exists = theme(legend.position="none")
  }
  
  shade = data.frame(start = as.Date(Sys.Date()), end = as.Date(end.date,  "%Y-%m-%d"))
  
  ggplot() + geom_line(data = df_plot, aes_string("date", "value", colour = "variable")) +     
    uni.theme + 
    scale_x_date(breaks = 0, date_breaks = "4 years", date_labels = "%Y", limits = as.Date(c(start.date,end.date))) + 
    labs(title = t, subtitle = sub, caption = cap) + ylab(ylabel) + xlab("") +
    scale_colour_manual(values=colorz) + leg_exists
}

# helper function for bar plots
bp <- function(data, labelz = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = last_proj_date, colorz = colorz) {
  df_plot <- data
  colnames(df_plot) = c("date", labelz)
  df_plot <- melt(df_plot, id = "date")
  
  leg_exists = NULL
  if(is.null(labelz)){
    leg_exists = theme(legend.position="none")
  }
  
  ggplot() + geom_bar(data = df_plot, aes_string("date", "value", fill = "variable"), stat = "identity", width = 50) + 
    uni.theme + 
    scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = "%Y", limits = as.Date(c(start.date,end.date))) + 
    labs(title = t, subtitle = sub, caption = cap) + ylab(ylabel) + xlab("") +
    scale_fill_manual(values=colorz) + leg_exists 
  
}

# data frames for projection and recession shading
projection = data.frame(start = as.Date(last_hist_date + 40) , end = last_proj_date)
recessions = data.frame(start = hist$date[which(diff(hist$recessq)==2)], end = c(hist$date[which(diff(hist$recessq)==-2)],last_hist_date)[-1])

# gg objects for recession and projection shading
projection_shade =  geom_rect(data = projection, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'yellow', alpha = 0.1)
recession_shade = geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.3)

# gg object for the moving-average line
moving_average = function(){
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-Quarter Moving-Average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-Quarter Moving-Average")) +
  scale_shape_manual(" ", values=c("4-Quarter Moving-Average" ="black"))
}

# construct figures

fimbars1 = bp(data = fim[,c("date","fim_bars")], 
              labelz = c(" Quarterly fiscal impact"), 
              start.date = "2000-01-01", 
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Total", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = total_pink, 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 0.75) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 7.5), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-4,8) + 
  guidez


fimbars2 = bp(data = fim[,c("date",c("state_local_cont", "federal_cont", "taxes_transfers_cont", "subsidies_cont"))], 
              labelz = c(" State & Local", " Federal", " Taxes & Transfers", " Subsidies"), 
              start.date = "2000-01-01",
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Components", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(state_local_purple, federal_blue, taxes_transfers_green, subsidies_orange), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 0.75) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y=7.5), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-4,8) + 
  guidez

fimbars3 = bp(data = fim[,c("date",c("state_local_cont", "state_taxes_transfers_cont", "state_subsidies_cont", "federal_cont", "federal_taxes_transfers_cont", "federal_subsidies_cont"))], 
              labelz = c(" State & Local Purchases", " State & Local Taxes", " State & Local Subsides"," Federal Purchases", " Federal Taxes", " Federal Subsidies"), 
              start.date = "2000-01-01", 
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Components by government", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(state_local_purple, "pink", dark_golden_rod, federal_blue, "lightblue", subsidies_orange), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 0.75) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 7.5), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-4,8) + 
  guidez


fimbars4 = bp(data = fim[,c("date",other_cont, "purchases_cont")], 
              labelz = c(" Health Outlays", " Social Benefits", " Noncorporate Taxes", " Corporate Taxes", " Purchases"), 
              start.date = "2000-01-01",
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Tax and Transfer Components", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(taxes_transfers_green, "lightgreen", "lightgoldenrod", "darkorange", "lightgrey"), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 0.75) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 7.5), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-4,8) + 
  guidez

# render to pdf and jpg

if(render){
  rmarkdown::render('Fiscal-Impact-Expanded.RMD', output_file = paste0(subdir, '/Fiscal-Impact-Expanded.pdf'), output_format = "pdf_document")
  rmarkdown::render('Fiscal-Impact.RMD', output_file = paste0(subdir, '/Fiscal-Impact.pdf'), output_format = "pdf_document")
  
  jpeg(filename = paste0(subdir, '/FIMbars-1.jpg'), res = 400, height = 6, width = 8, units = "in")
  fimbars1
  dev.off()
  
  jpeg(filename = paste0(subdir, '/FIMbars-2.jpg'), res = 400, height = 6, width = 8, units = "in")
  fimbars2
  dev.off()
  
}