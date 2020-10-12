## ---------------------------
##
## Script name: Figures
##
## Purpose of script:
## Create data visualizations for FIM
## Authors: Manuel Alcalá Kovalski, Sage Belz, Kadija Yilla
##
## Date Created: 2020-10-07
##
## Email: malcalakovalski@brookings.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


## load up the packages we will need:

packages <-
  c('ggplot2', 
    'ggtext',
    'gridExtra',
    'grid',
    'wesanderson',
    'tinytex')
lapply(packages, require, character.only = TRUE)

# Do you want to render the output to PDFs and JPGs?
render = T


######COVID19 changes#####
#manually added end date for recesions that's equal to the start of the current month
end_date_reccession = as.Date("2020-07-31") #last date a month after the end of the current quarter

##want to add subsidies to taxes and transfers
fim <- fim %>% 
  mutate(taxes_transfers_cont = taxes_transfers_cont + subsidies_cont,
         state_taxes_transfers_cont = state_taxes_transfers_cont + state_subsidies_cont,
         federal_taxes_transfers_cont = federal_taxes_transfers_cont + federal_subsidies_cont
  )

##renamed "Taxes and Transfers" with "taxes, transfers, and subsidies"
######

# set plot formats, theme 
uni.theme <- theme_bw() + 
  theme(legend.position = "bottom", 
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        plot.margin=unit(c(1.2,.5,.5,.5),"cm"),
        plot.title = element_text(size=12, face = "bold"),
        plot.subtitle = element_text(size=10) , 
        plot.caption = element_text(size = 9),
        legend.text=element_text(size=10), 
        legend.title=element_blank(),
        legend.spacing.y = unit(2, 'cm')
        ) # , legend.margin = unit(c(rep(-.8, 4)),"cm")

guidez <- guides(
  fill = guide_legend(keywidth = unit(0.8, "cm"),
                      keyheight = unit(0.4, "cm"), 
                      ncol = 1),
  colour = guide_legend(keywidth = unit(0.8, "cm"),
                        keyheight = unit(0.05, "cm"), 
                        ncol = 1)
  )

taxes_transfers_green = rgb(27, 149, 83,  maxColorValue = 255)
state_local_purple = rgb(174, 104, 169,  maxColorValue = 255)
federal_blue = rgb(33, 152, 199,  maxColorValue = 255)
total_pink = rgb(231, 97, 159, maxColorValue = 255)

colourz = c("black",
            taxes_transfers_green,
            state_local_purple,
            federal_blue,
            total_pink,
            wes_palette("Royal2"),
            wes_palette("Royal2")
            )

#max value for the y axis
max_y = ceiling(max(apply(fim[121:nrow(fim),2:7], 2, FUN=max, na.rm = TRUE))) + 1
#minimum valye for the y axis
min_y = floor(min(apply(fim[121:nrow(fim),2:7], 2, FUN=min, na.rm = TRUE))) - 1


# helper function for line plots
lp <- function(data, 
               labelz = NULL,
               ylabel = NULL,
               t = NULL,
               sub = NULL,
               cap = NULL,
               start.date = "2000-01-01",
               end.date = last_proj_date,
               colorz = colourz){
                                  df_plot <- data
                                  colnames(df_plot) = c("date", labelz)
                                  df_plot <- melt(df_plot, id = "date")
                                  df_plot <- df_plot[which(df_plot$date <= end.date & df_plot$date >= start.date), ]
                                  
                                  leg_exists = NULL
                                  if(is.null(labelz)){
                                    leg_exists = theme(legend.position="none")
                                    }
  
  shade <-
     data.frame(start = as.Date(Sys.Date()), 
                end = as.Date(end.date,
                              "%Y-%m-%d")
                )
  
  ggplot() + 
    geom_line(data = df_plot, 
              aes_string("date", "value", colour = "variable")
              ) +     
    uni.theme + 
    scale_x_date(breaks = 0,
                 date_breaks = "4 years",
                 date_labels = "%Y",
                 limits = as.Date(c(start.date, end.date))
                 ) + 
    labs(title = t,
         y = ylabel,
         x = "",
         subtitle = sub,
         caption = cap) +
    scale_colour_manual(values=colorz) + 
    leg_exists
}

# helper function for bar plots
bp <- function(data, labelz = NULL, ylabel = NULL, t = NULL, sub = NULL, cap = NULL, start.date = "2000-01-01", end.date = last_proj_date, colorz = colorz) {
  df_plot <- data
  colnames(df_plot) = c("date", labelz)
  df_plot <- reshape2::melt(df_plot, id = "date")
  
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
recessions = data.frame(start = hist$date[which(diff(hist$recessq)==2)], end = c(hist$date[which(diff(hist$recessq)==-2)],end_date_reccession)[-1])

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
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = max_y), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  #ylim(min_y, max_y) + 
  guidez

fimbars2 = bp(data = fim[,c("date",c("state_local_cont", "federal_cont", "taxes_transfers_cont"))], 
              labelz = c(" State & Local Purchases", " Federal Purchases", " Taxes, Transfers, & Subsidies"), 
              start.date = "2000-01-01",
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Components", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(state_local_purple, federal_blue, taxes_transfers_green), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = max_y+2), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  #ylim(min_y, max_y+2) + 
  guidez


fimbars3 = bp(data = fim[,c("date",c("state_local_cont", "state_taxes_transfers_cont", "federal_cont", "federal_taxes_transfers_cont"))], 
              labelz = c(" State & Local Purchases", " State & Local Taxes, Transfers, & Subsidies", " Federal Purchases", " Federal Taxes, Transfers, & Subsidies"), 
              start.date = "2000-01-01", 
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Components by government", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(state_local_purple, "pink", federal_blue, "lightblue"), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average")) +  
  geom_point(data = fim, aes(x = date, y = fim_bars_ma, colour = "4-quarter moving-average"), size = 1) +
  scale_color_manual(" ", values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
  geom_text(aes(x=Sys.Date()+350, y = max_y), label = "Projection", cex = 2) + 
  projection_shade + 
  recession_shade + 
  #ylim(min_y, max_y + 4) + 
  guidez


 


fimbars4 = bp(data = fim[,c("date",other_cont, "purchases_cont", "subsidies_cont")], 
              labelz = c(" Health Outlays", " Social Benefits", " Noncorporate Taxes", 
                         " Corporate Taxes", " Purchases", " Subsidies"), 
              start.date = "2000-01-01",
              end.date = last_proj_date,
              t = "Hutchins Center Fiscal Impact Measure: Tax, Transfer, and Subsidy Components", 
              sub = "Fiscal Policy Contribution to Real GDP Growth, percentage points", 
              colorz = c(taxes_transfers_green, "lightgreen", "lightgoldenrod", "darkorange", "lightgrey", "rosybrown"), 
              cap = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") + 
  geom_line(data = fim,
            aes(x = date,
                y = fim_bars_ma,
                colour = "4-quarter moving-average")
            ) +  
  geom_point(data = fim,
             aes(x = date,
                 y = fim_bars_ma,
                 colour = "4-quarter moving-average"),
             size = 1) +
  scale_color_manual(" ",
                     values = c("4-quarter moving-average" = "black",
                                "4-quarter moving-average" =" black")
                     ) +
  geom_text(aes(x = Sys.Date()+350,
                y = max_y), 
            label = "Projection",
            cex = 2
            ) + 
  projection_shade + 
  recession_shade + 
  #ylim(min_y, max_y) + 
  guidez

# render to pdf and jpg
if(render){
  rmarkdown::render('Fiscal-Impact-Expanded.RMD',
                    output_file = paste0(subdir,
                                         '/Fiscal-Impact-Expanded-',
                                         Sys.Date(),
                                         '.pdf'),
                    output_format = "pdf_document")
  
  rmarkdown::render('Fiscal-Impact.RMD',
                    output_file = paste0(subdir,
                                         '/Fiscal-Impact-',
                                         Sys.Date(),
                                         '.pdf'),
                    output_format = "pdf_document")
  
  jpeg(filename = paste0(subdir, '/FIMbars-1-', Sys.Date(), '.jpg'),
       res = 400, 
       height = 6, 
       width = 8, 
       units = "in")
  fimbars1
  dev.off()
  
  jpeg(filename = paste0(subdir, '/FIMbars-2-', Sys.Date(), '.jpg'),
       res = 400,
       height = 6,
       width = 8,
       units = "in")
  fimbars2
  dev.off()
}
