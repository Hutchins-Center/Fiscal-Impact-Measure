# Plot figures
source("fim_calculations.R")

library(ggplot2)
library(gridExtra)
library(grid)
library(wesanderson)

# Do you want to render the output to PDFs and JPGs?
render = T

#REMOVING PROJECTIONS DUE TO COVID19
#added end dates to the figures on lines 87, 105, 122 and 140

# set plot formats, theme 
uni.theme = theme_bw() + theme(legend.position = "bottom", panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank() , plot.margin=unit(c(1.2,.5,.5,.5),"cm") , plot.title = element_text(size=12, face = "bold"), plot.subtitle = element_text(size=10) , plot.caption = element_text(size = 9), legend.text=element_text(size=10), legend.title=element_blank(),legend.spacing.y = unit(2, 'cm')) # , legend.margin = unit(c(rep(-.8, 4)),"cm")

guidez =  guides(fill = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.4, "cm"), ncol = 1), colour = guide_legend(keywidth = unit(0.8, "cm"), keyheight = unit(0.05, "cm"), ncol = 1))

taxes_transfers_green = rgb(27, 149, 83,  maxColorValue = 255)
state_local_purple = rgb(174, 104, 169,  maxColorValue = 255)
federal_blue = rgb(33, 152, 199,  maxColorValue = 255)
total_pink = rgb(231, 97, 159, maxColorValue = 255)
colourz = c("black",taxes_transfers_green, state_local_purple, federal_blue, total_pink, wes_palette("Royal2"), wes_palette("Royal2"))


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
recessions = data.frame(start = hist$date[which(diff(hist$recessq)==2)], end = hist$date[which(diff(hist$recessq)==-2)][-1])

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
              end.date = "2020-06-01",
              t = "Hutchins Center Fiscal Impact Measure: Total", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = total_pink, 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-2,4) + 
  guidez


fimbars2 = bp(data = fim[,c("date",c("state_local_cont", "federal_cont", "taxes_transfers_cont"))], 
              labelz = c(" State & Local", " Federal", " Taxes & Transfers"), 
              start.date = "2000-01-01",
              end.date = "2020-06-01",
              t = "Hutchins Center Fiscal Impact Measure: Components", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(state_local_purple, federal_blue, taxes_transfers_green), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-2,4) + 
  guidez

fimbars3 = bp(data = fim[,c("date",c("state_local_cont", "state_taxes_transfers_cont", "federal_cont", "federal_taxes_transfers_cont"))], 
              labelz = c(" State & Local Purchases", " State & Local Taxes", " Federal Purchases", " Federal Taxes"), 
              start.date = "2000-01-01", 
              end.date = "2020-06-01",
              t = "Hutchins Center Fiscal Impact Measure: Components by government", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(state_local_purple, "pink", federal_blue, "lightblue"), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-2,4) + 
  guidez


fimbars4 = bp(data = fim[,c("date",other_cont, "purchases_cont")], 
              labelz = c(" Health Outlays", " Social Benefits", " Noncorporate Taxes", " Corporate Taxes", " Purchases"), 
              start.date = "2000-01-01",
              end.date = "2020-06-01",
              t = "Hutchins Center Fiscal Impact Measure: Tax and Transfer Components", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(taxes_transfers_green, "lightgreen", "lightgoldenrod", "darkorange", "lightgrey"), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = 4), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  ylim(-2,4) + 
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















################################
### ADDENDA: Reference figures
################################

# gdp1 <- lp(data = xx[, c("date", "gdppothq", "gdph")], start.date = "2015-01-01", end.date = "2022-01-01", labelz = c("Real Potential GDP", "Real GDP"), colorz = c(taxes_transfers_green, "lightgreen")) + 
#   projection_shade
# yy <- mutate(xx, 
#              gdph_g = q_a(gdph), 
#              gdppothq_g = q_a(gdppothq))
# 
# gdp2 <- lp(data = xx[, c("date", "gdppothq_g", "gdph_g")], start.date = "2015-01-01", end.date = "2027-01-01", labelz = c("Real Potential GDP", "Real GDP"), colorz = c(taxes_transfers_green, "lightgreen")) + projection_shade
# 
# 
# lp(data = budg[, c("date", "gftfp_unadj", "gftfp")], start.date = "2015-01-01", end.date = "2027-01-01", labelz = c("Raw", "Adj"), colorz = c(taxes_transfers_green, "lightgreen")) + projection_shade
# lp(data = fim[, c("date", "federal_social_benefits", "federal_health_outlays")], start.date = "2015-01-01", end.date = "2027-01-01", labelz = c("Raw", "Adj"), colorz = c(taxes_transfers_green, "lightgreen")) + geom_vline(xintercept = as.Date(last_hist_date))


# gfpct <- lp(data = xx[,c("date", "gfb_gdp","gf_gdp")], c("Current law", "Alternative: Discretionary caps do not expire"), t = "Federal consumption & invesment, current law and alternative", sub = "Share of GDP (%)")
# gflev <- lp(xx[,c("date", "gfb","gf")], c("Current law", "Alternative: Discretionary caps do not expire"), t = "Federal consumption & invesment, current law and alternative", sub = "Billions ($)")
# gpct <- lp(xx[,c("date", "gs_gdp", "gf_gdp")], c("State & Local", "Federal*"), t = "Government consumption & invesment", sub = "Share of GDP (%)", c = "*Alternative discretionary spending")
# glev <- lp(xx[,c("date", "gs", "gf")], c("State & Local", "Federal*"), t = "Government consumption & invesment", sub = "Billions ($)", c = "*Alternative discretionary spending")
# gtfpct <- lp(xx[,c("date", "gstfp_gdp", "gftfp_gdp")], c("State & Local", "Federal"), t = "Government benefits to persons", sub = "Share of GDP (%)", c = "Includes Medicare and Medicaid")
# gtflev <- lp(xx[,c("date", "gstfp", "gftfp")], c("State & Local", "Federal"), t = "Government benefits to persons", sub = "Billions ($)", c = "Includes Medicare and Medicaid")
# yptpct <- lp(xx[,c("date", "yptmr_gdp", "yptmd_gdp")], c("Medicare", "Medicaid"), t = "Health Spending", sub = "Share of GDP (%)")
# yptlev <- lp(xx[,c("date", "yptmr", "yptmd")], c("Medicare", "Medicaid"), t = "Health Spending", sub = "Billions ($)")
# ytlev <- lp(xx[,c("date", "yptx", "ytpi", "yctlg", "grcsi")], c("Personal*", "Production & Import", "Corporate", "Payroll"), t = "Current taxes", sub = "Billions ($)", c = "*Alternative assumption (no sunset on TCJA provisions)")
# ytpct <- lp(xx[,c("date", "yptx_gdp", "ytpi_gdp", "yctlg_gdp", "grcsi_gdp")], c("Personal*", "Production & Import", "Corporate", "Payroll"), t = "Current taxes", sub = "Share of GDP (%)", c = "*Alternative assumption (no sunset on TCJA provisions)") + guides(fill=guide_legend(nrow=2,byrow=TRUE))
# yptxlev <- lp(xx[,c("date", "yptx", "yptxb")], c("Alternative: No sunset", "Current law"), t = "Personal Current Taxes under current law and alternative assumptions, federal", sub = "Billions ($)")
# yptxpct <- lp(xx[,c("date", "yptx_gdp", "yptxb_gdp")], c("Alternative: No sunset", "Current law"), t = "Personal Current Taxes", sub = "Share of GDP (%)")
# 
# pdf(paste0("component-projections-",Sys.Date(),".PDF"), height = 8.5, width = 11)
# grid.arrange(fimbars, fimcomps,  nrow = 2, top = textGrob("FIM Projections", gp=gpar(fontsize=15,font=2)), heights = c(1.5,1.5))
# grid.arrange(gflev, gfpct, glev, gpct,  nrow = 2, ncol = 2, top = textGrob("Government consumption & invesment, actual and projected, 2000-2028", gp=gpar(fontsize=15,font=2)), heights = c(1,1))
# grid.arrange(gtflev, gtfpct, yptlev, yptpct,   nrow = 2, ncol = 2, top = textGrob("Government benefits, actual and projected, 2000-2028", gp=gpar(fontsize=15,font=2)))
# grid.arrange(ytlev, ytpct, yptxlev, yptxpct,  nrow = 2, ncol = 2, top = textGrob("Taxes, actual and projected, 2000-2028", gp=gpar(fontsize=15,font=2, vjust = -10)))
# # grid.arrange(ytlev, ytpct, yptxlev, yptxpct,  nrow = 2, ncol = 2, top = textGrob("Addenda", gp=gpar(fontsize=15,font=2, vjust = -10)))
# dev.off()
# 
# 
# lp(xx[,c("date", "deficit_gdp", "deficitb_gdp")], c("alternative", "current law"))
# 
# # "Total" =  c(0.7, 3.6, 1.1, 0.4, 0.0, 0.5),
# annualproj = data.frame(Federal = c(1.0, 6.9, 0.9, -0.5, -1.4, 0.3), "State_local" = c(0.5, 1.6, 1.3, 1.0, 0.8, 0.6), yr = c("Actual, 2017", "2018", "2019", "2020", "2021-2022", " 2023-2028"))
# annualproj = melt(annualproj, id = "yr")
# annualproj = annualproj[order(annualproj$yr),]
# ggplot(annualproj, aes(x=yr, y=value, group = variable)) + geom_line(aes(colour = variable)) +  uni.theme + ylab("Percent") +scale_colour_manual(values=colorz)
