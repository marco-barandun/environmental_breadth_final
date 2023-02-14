### Analysis of the tree species for publication

library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")
library("viridis")

base::setwd("."); getwd()
dir_archive <- "/Users/marco/Documents/env_breadth_archive/"

# -------------------------------------------------------------------------------------------------------
##### Import and filter data

niche_data <- read_csv("./../3_generated_data/niche_data_final.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)); range(niche_data$e_breadth)


zones <- read_csv("./../3_generated_data/zones.csv")

niche_data <- left_join(niche_data, zones, by = "Species"); ds <- niche_data

# -------------------------------------------------------------------------------------------------------
##### Defining function for single normal plot

my_plotting <- function(df, 
                        xc, 
                        yc,
                        gfc,
                        binc,
                        gf_sel = growthform,
                        ttitle,
                        xxlab,
                        yylab,
                        plot_type,
                        binning,
                        y_lim_s,
                        y_lim_n,
                        zonec) {
  
  df <- df %>% filter(.data[[gfc]] == gf_sel)
  mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))
  
  p <- ggplot(data = df, 
              aes(y = .data[[yc]])) +
    ggtitle(paste(ttitle, gf_sel)) +
    xlab(xxlab) +
    ylab(yylab) +
    theme(text = element_text(size = 25)) +
    theme_classic() +
    scale_fill_manual(values = mycolors) +
    geom_density_2d_filled(data = df, 
                        aes(x = .data[[xc]]),
                        bins = 20, show.legend = FALSE) +
    stat_summary_bin(data = df,
                  color = "darkgrey",
                  aes(x = df[[xc]]),
                  breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05)),
                  fun = mean, 
                  fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                  fun.max = function(x) mean(x)+sd(x)/sqrt(length(x))) +
    coord_cartesian(ylim=c(y_lim_s, y_lim_n)) +
    
    geom_smooth(
        aes(x = .data[[xc]], linetype = .data[[zonec]]),
        colour = "black",
        method = "lm",
        na.rm = TRUE,
        show.legend = FALSE)

  
  return(p)
}

# -------------------------------------------------------------------------------------------------------
##### Creating plot grid

nine_plots <- function(ds,
                       growthform = "tree",
                       plot.type = "p_mean",
                       bin.ning = "eq_points") {
  
  
  #------------- Latitudinal range
  
  
  median_lrange_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                                 xc = "lat_median_n", 
                                 yc = "lat_range_sd_n",
                                 gfc = "growthform",
                                 binc = "bin",
                                 gf_sel = growthform,
                                 ttitle = "Northern hemisphere",
                                 xxlab = 'Latitudinal median',
                                 yylab = 'Latitudinal range',
                                 plot_type = plot.type,
                                 binning = bin.ning,
                                 y_lim_s = 0,
                                 y_lim_n = 10,
                                 zonec = "zone_n")
  
  median_lrange_s <- my_plotting(ds %>% filter(lat_median_s > 0),
                                 xc = "lat_median_s", 
                                 yc = "lat_range_sd_s",
                                 gfc = "growthform",
                                 binc = "bin",
                                 gf_sel = growthform,
                                 ttitle = "Southern hemisphere",
                                 xxlab = 'Latitudinal median',
                                 yylab = 'Latitudinal range',
                                 plot_type = plot.type,
                                 binning = bin.ning,
                                 y_lim_s = 0,
                                 y_lim_n = 10,
                                 zonec = "zone_s")
  
  median_lrange_g <- my_plotting(ds,
                                 xc = "lat_median_g", 
                                 yc = "lat_range_sd_g",
                                 gfc = "growthform",
                                 binc = "bin",
                                 gf_sel = growthform,
                                 ttitle = "Global",
                                 xxlab = 'Latitudinal median',
                                 yylab = 'Latitudinal range',
                                 plot_type = plot.type,
                                 binning = bin.ning,
                                 y_lim_s = 0,
                                 y_lim_n = 10,
                                 zonec = "zone_g")
  
  #------------- Environmental breadth
  
  median_ebreadth_n <- my_plotting(ds %>% filter(hemisphere == 1),
                                   xc = all_of("e_breadth"), 
                                   yc = all_of("lat_median_n"),
                                   gfc = "growthform",
                                   binc = "bin",
                                   gf_sel = growthform,
                                   ttitle = "Northern hemisphere",
                                   xxlab = 'Environmental breadth',
                                   yylab = 'Latitudinal median',
                                   plot_type = plot.type,
                                   binning = bin.ning, 
                                   y_lim_s = 0,
                                   y_lim_n = 50)
  
  median_ebreadth_s <- my_plotting(ds %>% filter(hemisphere == -1),
                                   xc = "e_breadth", 
                                   yc = "lat_median_s",
                                   gfc = "growthform",
                                   binc = "bin",
                                   gf_sel = growthform,
                                   ttitle = "Southern hemisphere",
                                   xxlab = 'Environmental breadth',
                                   yylab = 'Latitudinal median',
                                   plot_type = plot.type,
                                   binning = bin.ning, 
                                   y_lim_s = 0,
                                   y_lim_n = 50)
  
  median_ebreadth_g <- my_plotting(ds,
                                   xc = "e_breadth", 
                                   yc = "lat_median_g",
                                   gfc = "growthform",
                                   binc = "bin",
                                   gf_sel = growthform,
                                   ttitle = "Global",
                                   xxlab = 'Environmental breadth',
                                   yylab = 'Latitudinal median',
                                   plot_type = plot.type,
                                   binning = bin.ning, 
                                   y_lim_s = -50,
                                   y_lim_n = 50)
  
  #------------- Environmental breadth vs. latitudinal range
  
  ebreadth_range_n <- my_plotting(ds %>% filter(hemisphere == 1),
                                  xc = "e_breadth", 
                                  yc = "lat_range_sd_n",
                                  gfc = "growthform",
                                  binc = "bin",
                                  gf_sel = growthform,
                                  ttitle = "Northern hemisphere",
                                  xxlab = 'Environmental breadth',
                                  yylab = 'Latitudinal range',
                                  plot_type = plot.type,
                                  binning = bin.ning, 
                                  y_lim_s = 0,
                                  y_lim_n = 10)
  
  ebreadth_range_s <- my_plotting(ds %>% filter(hemisphere == -1),
                                  xc = "e_breadth", 
                                  yc = "lat_range_sd_s",
                                  gfc = "growthform",
                                  binc = "bin",
                                  gf_sel = growthform,
                                  ttitle = "Southern hemisphere",
                                  xxlab = 'Environmental breadth',
                                  yylab = 'Latitudinal range',
                                  plot_type = plot.type,
                                  binning = bin.ning, 
                                  y_lim_s = 0,
                                  y_lim_n = 10)
  
  ebreadth_range_g <- my_plotting(ds,
                                  xc = "e_breadth", 
                                  yc = "lat_range_sd_g",
                                  gfc = "growthform",
                                  binc = "bin",
                                  gf_sel = growthform,
                                  ttitle = "Global",
                                  xxlab = 'Environmental breadth',
                                  yylab = 'Latitudinal range',
                                  plot_type = plot.type,
                                  binning = bin.ning, 
                                  y_lim_s = 0,
                                  y_lim_n = 10)
  
  
  p <- cowplot::plot_grid(median_lrange_n + theme(legend.position = "none"),
                          median_lrange_s + theme(legend.position = "none"),
                          median_lrange_g,
                          median_ebreadth_n + theme(legend.position = "none"),
                          median_ebreadth_s + theme(legend.position = "none"), 
                          median_ebreadth_g,
                          ebreadth_range_n + theme(legend.position = "none"),
                          ebreadth_range_s + theme(legend.position = "none"), 
                          ebreadth_range_g, 
                          rel_widths = c(1, 1, 1.3,
                                         1, 1, 1.3, 
                                         1, 1, 1.3),
                          ncol = 3, align = "h",
                          labels = c("A", "B", "C",
                                     "D", "E", "F",
                                     "G", "H", "I"),
                          label_size = 25)
  
  return(p)
  
}

p <- nine_plots(ds = niche_data,
                growthform = "herb",
                plot.type = "p_mean",
                bin.ning = "eq_points")
p

ggsave("./../tmp/9-fig-tree-mean-eqpoints.jpg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

ggsave("./../tmp/9-fig-herb-mean-eqpoints.jpg",
       width = 4000, height = 3000, units = "px")

