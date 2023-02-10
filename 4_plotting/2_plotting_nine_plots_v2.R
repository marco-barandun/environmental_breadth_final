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
  mutate(e_breadth = (env_breadth*mess)^(1/4)); range(niche_data$e_breadth); ds <- niche_data
#  rename(l_median = lat_median) %>%   # Define which column to use as median latitude
#  mutate(l_range = (lat_n95q+90)-(lat_s05q+90)) %>%     # Define which column to use as latitudinal range #(lat_n95q+90)-(lat_s05q+90) lat_range_mad
#  mutate(e_breadth = env_breadth) %>%     # Define which column to use as environmental breadth
#  mutate(al_range = l_range/max(l_range)) %>%
#  mutate(al_median = abs(l_median)/max(abs(l_median))) %>%
#  mutate(ae_breadth = e_breadth/max(e_breadth)) %>%
#  mutate(bin = ntile(l_median, n=40))

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
                        y_lim_n) {
  
  df <- df %>% filter(.data[[gfc]] == gf_sel)
  mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))
  
  p <- ggplot(data = df, 
              aes(y = .data[[yc]])) +
    ggtitle(paste(ttitle, gf_sel)) +
    xlab(xxlab) +
    ylab(yylab) +
    theme(text = element_text(size = 25)) +
    theme_classic()

  if (plot_type == "bp") {p <- p + 
    geom_boxplot(data = df, aes(x = .data[[binc]], group = .data[[binc]]))}
  if (plot_type == "sp") {p <- p + 
    geom_point(data = df, aes(x = .data[[xc]], alpha = 0.5)) + 
    geom_smooth(data = df, aes(x = .data[[xc]]), method = "lm", color = "black", size = 2, show.legend = FALSE)}
  if (plot_type == "dp") {p <- p + 
    geom_density_2d_filled(data = df, aes(x = .data[[xc]]))}
  if (plot_type == "p_median") {
    if (binning == "eq_points") {p <- p + 
      #geom_point(data = df, 
      #           aes(x = .data[[xc]]),
      #           size = 0.5, 
      #           alpha = 0.1,
      #           #alpha = (((northern$al_range/(max(northern$al_range)))^2) + ((northern$al_median/(max(northern$al_median)))^2))^2
      #) +
      #geom_polygon(aes(x, y), fill = "white",
      #             data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
      #                                  contourLines(x=eval.points[[1]], y=eval.points[[2]],
      #                                               z=estimate, levels=cont["50%"])[[1]]))) +
      scale_fill_manual(values = mycolors) +
      geom_density_2d_filled(data = df, 
                             aes(x = .data[[xc]]),
                             bins = 20, show.legend = FALSE) +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["50%"])[[1]])),
                linetype = "dotted") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["20%"])[[1]])),
                linetype = "dashed") +
      #geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
      #                                          contourLines(x=eval.points[[1]], y=eval.points[[2]],
      #                                                       z=estimate, levels=cont["5%"])[[1]]))) +
      stat_summary_bin(data = df, 
                       aes(x = df[[xc]]),
                       breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05)),
                       fun = median, 
                       fun.min = function(x) median(x)-max(x), 
                       fun.max = function(x) median(x)+mad(x))
    }
    if (binning == "eq_distance") {p <- p + 
      geom_point(data = df, 
                 aes(x = .data[[xc]]),
                 size = 0.5, 
                 alpha = 0.1,
                 #alpha = (((northern$al_range/(max(northern$al_range)))^2) + ((northern$al_median/(max(northern$al_median)))^2))^2
      ) +
      geom_polygon(aes(x, y), fill = "white",
                   data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                        contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                     z=estimate, levels=cont["5%"])[[1]]))) +
      scale_fill_manual(values = mycolors) +
      geom_density_2d_filled(data = df, 
                             aes(x = .data[[xc]]),
                             bins = 20, show.legend = FALSE) +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["50%"])[[1]])),
                linetype = "dotted") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["20%"])[[1]])),
                linetype = "dashed") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["5%"])[[1]]))) +
      stat_summary_bin(data = df, 
                       aes(x = df[[xc]]),
                       fun = median,
                       fun.min = function(x) median(x)-max(x), 
                       fun.max = function(x) median(x)+mad(x))}
  }
  if (plot_type == "p_mean") {
    if (binning == "eq_points") {p <- p + 
      scale_fill_manual(values = mycolors) +
      geom_density_2d_filled(data = df, 
                             aes(x = .data[[xc]]),
                             bins = 20, show.legend = FALSE) +
      #geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
      #                                          contourLines(x=eval.points[[1]], y=eval.points[[2]],
      #                                                       z=estimate, levels=cont["20%"])[[1]])),
      #          linetype = "dashed") +
       stat_summary_bin(data = df, 
                       aes(x = df[[xc]]),
                       breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05)),
                       fun = mean, 
                       fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                       fun.max = function(x) mean(x)+sd(x)/sqrt(length(x))) +
      coord_cartesian(ylim=c(y_lim_s, y_lim_n))}
    if (binning == "eq_distance") {p <- p + 
      geom_point(data = df, 
                 aes(x = .data[[xc]]),
                 size = 0.5, 
                 alpha = 0.1,
                 #alpha = (((northern$al_range/(max(northern$al_range)))^2) + ((northern$al_median/(max(northern$al_median)))^2))^2
      ) +
      geom_polygon(aes(x, y), fill = "white",
                   data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                        contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                     z=estimate, levels=cont["5%"])[[1]]))) +
      scale_fill_manual(values = mycolors) +
      geom_density_2d_filled(data = df, 
                             aes(x = .data[[xc]]),
                             bins = 20, show.legend = FALSE) +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["50%"])[[1]])),
                linetype = "dotted") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["20%"])[[1]])),
                linetype = "dashed") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(df %>% select(.data[[xc]], .data[[yc]]), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["5%"])[[1]]))) +
      stat_summary_bin(data = df, 
                       aes(x = df[[xc]]),
                       fun = mean,
                       fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                       fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)))}
  }
  if (plot_type == "simple") {
    if (binning == "eq_points") {p <- p + 
      stat_summary_bin(data = df, 
                       aes(x = df[[xc]]),
                       breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05)),
                       fun = mean,
                       fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                       fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)))}
    if (binning == "eq_distance") {p <- p + 
      stat_summary_bin(data = df, 
                     aes(x = df[[xc]]),
                     fun = mean,
                     fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                     fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)))}
    }
  
  return(p)
}

# -------------------------------------------------------------------------------------------------------
##### Creating plot grid
      
nine_plots <- function(ds,
                       growthform = "tree",
                       plot.type = "p_mean",
                       bin.ning = "eq_points") {
  

  
  #------------- Latitudinal range
  

  median_lrange_n <- my_plotting(ds %>% filter(hemisphere == 1),
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
                                 y_lim_n = 10)

  median_lrange_s <- my_plotting(ds %>% filter(hemisphere == -1),
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
                                 y_lim_n = 10)
  
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
                                 y_lim_n = 10)
   
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

