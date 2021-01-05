packages <- 
  c('ggplot2', 'ggtext','gridExtra','grid','wesanderson','tinytex')
lapply(packages, require, character.only = TRUE)

# CONSTANTS --------------------------------------------------------------------------------------
start = as_date("2000-01-01")
end_date_reccession = last_hist_date + months(1)
max_y <-
  fim %>%
  filter(date >= '2000-03-31') %>%
  select(fim_bars:subsidies_cont) %>%
  max() %>%
  ceiling() + 1
min_y <-
  fim %>%
  filter(date >= '2000-03-31') %>%
  select(fim_bars:subsidies_cont) %>%
  min() %>%
  floor() - 1

# Theme --------------------------------------------------------------------------------------
# set plot formats, theme 
uni.theme <- function() {
  theme_bw() +
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
}

## Legend formatting -------------------------------------------------------------------------
guidez <- guides(
fill = guide_legend(keywidth = unit(0.8, "cm"),
                    keyheight = unit(0.4, "cm"), 
                    ncol = 1),
colour = guide_legend(keywidth = unit(0.8, "cm"),
                      keyheight = unit(0.05, "cm"), 
                      ncol = 1)
)



total_pink <- rgb(231, 97, 159, maxColorValue = 255)
# Shading -------------------------------------------------------------------------------------

recessions <-
  data.frame(start = hist$date[which(diff(hist$recessq)==2)],
                        end = c(hist$date[which(diff(hist$recessq)==-2)],
                                end_date_reccession)[-1]
                        )
recession_shade <-
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf),
            fill = 'grey', alpha = 0.3)



# Functions -----------------------------------------------------------------------------------
fim_plot <-
  function(df, category){
    df %>%
      ggplot() +
      geom_bar(aes(x = date, y = value, fill = variable),
               stat = 'identity', width = 50) +
      geom_line(
        aes(x = date,
            y = fim_bars_ma,
            colour = "4-quarter moving-average")
      ) +
      geom_point(
        aes(x = date,
            y = fim_bars_ma,
            colour = "4-quarter moving-average"), size = 1
      ) +
      labs(
        title = glue("Hutchins Center Fiscal Impact Measure: Total"),
        x = '',
        y = '',
        subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
        caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis and Congressional Budget Office data; \ngrey shaded areas indicate recessions and yellow shaded areas indicate projection.") +
      geom_text(aes(x = Sys.Date()+350,
                    y = max_y), 
                label = "Projection",
                cex = 2
      ) +
      annotate("rect", xmin = last_hist_date + 40, xmax = last_proj_date,
               ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
      geom_rect(data = recessions,
                aes(x = NULL, y = NULL,
                    xmin = start, xmax = end, 
                    ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.3) +
      guidez +
      scale_x_date(breaks = 0, date_breaks = "4 years", date_labels = '%Y',
                   limits = c(start_date, last_proj_date)) +
      scale_color_manual(" ", 
                         values=c("4-quarter moving-average" ="black", "4-quarter moving-average" ="black")) +
      scale_fill_discrete(labels = "Quarterly fiscal impact")+
      uni.theme() 
  }
# Load data -----------------------------------------------------------------------------------
contributions <-
  fim %>%
  filter(date > start_date & date <= last_proj_date) %>%
  select(date, starts_with('fim'), ends_with('cont'), recession)
# Figures -------------------------------------------------------------------------------------
## Regular -----------------------------------------------------------------------------------
total <-
  contributions %>%
  select(date, fim_bars, fim_bars_ma) %>%
  pivot_longer(cols = fim_bars, names_to = 'variable') %>%
  fim_plot() +
  scale_fill_manual(labels = "Quarterly fiscal impact", values = total_pink)
 
components <-
  contributions %>%
  select(date, state_local_cont, federal_cont, taxes_transfers_cont, fim_bars_ma) %>%
  pivot_longer(cols = -c(date, fim_bars_ma), names_to = 'variable') %>%
  fim_plot()
## Expanded ----------------------------------------------------------------------------------

components_govt <-
  
taxes_transfers <-
