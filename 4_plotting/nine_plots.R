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

base::setwd("/Users/marco/GitHub/environmental_breadth/"); getwd()
dir_archive <- "/Users/marco/Documents/env_breadth_archive/"

# -------------------------------------------------------------------------------------------------------
##### Import and filter data

niche_data <- read_csv("./res_temp_exports/raw_data_sd.csv")

niche_data <- niche_data %>%
  filter(auc.val.avg > 0.55) %>%
  filter(0.05 < or.10p.avg) %>%
  filter(or.10p.avg < 0.3) %>%
  #head(., n = 10000) %>%
  rename(l_median = lat_median) %>%   # Define which column to use as median latitude
  mutate(l_range = lat_range_mad) %>%     # Define which column to use as latitudinal range
  rename(e_breadth = env_breadth) %>%     # Define which column to use as environmental breadth
  mutate(al_range = l_range/max(l_range)) %>%
  mutate(al_median = abs(l_median)/max(abs(l_median))) %>%
  mutate(ae_breadth = e_breadth/max(e_breadth))

# -------------------------------------------------------------------------------------------------------
##### Defining function for single plot

my_plotting <- function(df, 
                        xc, 
                        yc,
                        gfc,
                        binc,
                        hc,
                        gf_sel = "tree",
                        hemisphere,
                        ttitle,
                        xxlab,
                        yylab,
                        plot_type,
                        binning) {
  
  df <- df %>% filter(.data[[gfc]] == gf_sel) %>% filter(.data[[hc]] == UQ(hemisphere))
  
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
                       breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05)),
                       fun = mean, 
                       fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                       fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)))}
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
  
  return(p)
}

# -------------------------------------------------------------------------------------------------------
##### Creating plot grid



      
nine_plots <- function(ds,                         # The niche breadth dataset
                       box_or_points = "box"       # Set to "points" for a scatterplot, to "box" for a boxplot
                       ) {
  
  northern <- ds %>% dplyr::filter(l_median > 0) %>% mutate(bin = cut_number(l_median, n=10)) %>% mutate(ebin = cut_number(e_breadth, n=10))
  set.seed(2886)
  northern_pt <- northern %>% slice_sample(n = 5000)
  southern <- ds %>% dplyr::filter(l_median < 0) %>% mutate(bin = cut_number(l_median, n=10)) %>% mutate(ebin = cut_number(e_breadth, n=10))
  set.seed(2886)
  southern_pt <- southern %>% slice_sample(n = 5000)
  global <- ds %>% mutate(bin = cut_number(l_median, n=20)) %>% mutate(ebin = cut_number(e_breadth, n=20))
  set.seed(2886)
  global_pt <- global %>% slice_sample(n = 5000)
  
  #------------- Latitudinal range
  
  mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))

  median_lrange_n <- ggplot(data = northern, aes(y = l_range), color = growthform) +
    ggtitle("Northern hemisphere") +
    xlab('Latitudinal median') +
    ylab('Latitudinal range') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {median_lrange_n <- median_lrange_n + 
    geom_boxplot(data = northern, aes(x = bin, group = interaction(bin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {median_lrange_n <- median_lrange_n + 
    geom_point(data = northern_pt, aes(x = l_median, color = growthform, alpha = 0.5)) + 
    geom_smooth(data = northern, aes(x = l_median, group = growthform), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = northern, aes(x = l_median, group = growthform, color = growthform), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {median_lrange_n <- median_lrange_n + 
    geom_density_2d_filled(data = global, aes(x = l_median))}
  if (box_or_points == "paper_median") {median_lrange_n <- median_lrange_n + 
    stat_summary_bin(data = northern, aes(x = l_median), fun = median, fun.min = function(x) median(x)-mad(x), fun.max = function(x) median(x)+mad(x))}
  if (box_or_points == "paper_mean") {
    if (binning == "eq_points") {median_lrange_n <- median_lrange_n + 
      geom_point(data = northern, 
                 aes(x = l_median),
                 size = 0.5, 
                 alpha = 0.1,
                 #alpha = (((northern$al_range/(max(northern$al_range)))^2) + ((northern$al_median/(max(northern$al_median)))^2))^2
                 ) +
      geom_polygon(aes(x, y), fill = "white",
                   data=data.frame(with(ks::kde(northern %>% select(l_median, l_range), compute.cont=TRUE),
                                        contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                     z=estimate, levels=cont["5%"])[[1]]))) +
      scale_fill_manual(values = mycolors) +
      geom_density_2d_filled(data = northern, 
                             aes(x = l_median),
                             bins = 20, show.legend = FALSE) +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(northern %>% select(l_median, l_range), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["50%"])[[1]])),
                linetype = "dotted") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(northern %>% select(l_median, l_range), compute.cont=TRUE),
                                                contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                             z=estimate, levels=cont["20%"])[[1]])),
                linetype = "dashed") +
      geom_path(aes(x, y), data=data.frame(with(ks::kde(northern %>% select(l_median, l_range), compute.cont=TRUE),
                                                                     contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                                                                  z=estimate, levels=cont["5%"])[[1]]))) +
      stat_summary_bin(data = northern, 
                       aes(x = l_median),
                       breaks = quantile(northern$l_median, probs = seq(0, 1, by = 0.05)),
                       fun = mean, 
                       fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                       fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)))}
    if (binning == "eq_distance") {median_lrange_n <- median_lrange_n + 
      stat_summary_bin(data = northern, 
                       aes(x = l_median),
                       fun = mean,
                       fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                       fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)))}
    }

  ((northern$al_range/(max(northern$al_range)))^2) + ((northern$al_median/(max(northern$al_median)))^2)
  
  
  set.seed(1001)
  d <- data.frame(x=rnorm(1000),y=rnorm(1000))
  
  kd <- ks::kde(northern %>% select(l_median, l_range), compute.cont=TRUE)
  contour_95 <- with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["5%"])[[1]])
  contour_95 <- data.frame(contour_95)
  
  ggplot(data=d, aes(x, y)) +
    geom_point() +
    geom_path(aes(x, y), data=contour_95) +
    theme_bw()
  
  
  
  
  
  
  
   
  median_lrange_s <- ggplot(data = southern, aes(y = l_range), color = growthform) +
    ggtitle("Southern hemisphere") +
    xlab('Latitudinal median') +
    ylab('Latitudinal range') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {median_lrange_s <- median_lrange_s +
    scale_x_discrete(limits = rev(levels(southern$bin))) +
    geom_boxplot(data = southern, aes(x = bin, group = interaction(bin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {median_lrange_s <- median_lrange_s + 
    scale_x_reverse() +
    geom_point(data = southern_pt, aes(x = l_median, color = growthform, alpha = 0.5)) + 
    geom_smooth(data = southern, aes(x = l_median, group = growthform), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = southern, aes(x = l_median, group = growthform, color = growthform), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {median_lrange_s <- median_lrange_s + 
    geom_density_2d_filled(data = global, aes(x = l_median))}
  
  median_lrange_g <- ggplot(data = global, aes(y = l_range), color = growthform) +
    ggtitle("Global") +
    xlab('Latitudinal median') +
    ylab('Latitudinal range') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {median_lrange_g <- median_lrange_g + 
    geom_boxplot(data = global, aes(x = bin, group = interaction(bin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {median_lrange_g <- median_lrange_g + 
    geom_point(data = global_pt, aes(x = l_median, color = growthform, alpha = 0.5)) + 
    geom_smooth(data = global, aes(x = l_median, group = growthform), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = global, aes(x = l_median, group = growthform, color = growthform), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {median_lrange_g <- median_lrange_g + 
    geom_density_2d_filled(data = global, aes(x = l_median))}
  
  #------------- Environmental breadth
  
  median_ebreadth_n <- ggplot(data = northern, aes(y = e_breadth), color = growthform) +
    ggtitle("Northern hemisphere") +
    xlab('Latitudinal median') +
    ylab('Environmental breadth') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {median_ebreadth_n <- median_ebreadth_n + 
    geom_boxplot(data = northern, aes(x = bin, group = interaction(bin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {median_ebreadth_n <- median_ebreadth_n + 
    geom_point(data = northern_pt, aes(x = l_median, color = growthform, alpha = 0.5, size = weight)) + 
    geom_smooth(data = northern, aes(x = l_median, group = growthform, weight = weight), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = northern, aes(x = l_median, group = growthform, color = growthform, weight = weight), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {median_ebreadth_n <- median_ebreadth_n + 
    geom_density_2d_filled(aes(x = l_median))}
  
  median_ebreadth_s <- ggplot(data = southern, aes(y = e_breadth), color = growthform) +
    ggtitle("Southern hemisphere") +
    xlab('Latitudinal median') +
    ylab('Environmental breadth') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {median_ebreadth_s <- median_ebreadth_s +
    scale_x_discrete(limits = rev(levels(southern$bin))) +
    geom_boxplot(data = southern, aes(x = bin, group = interaction(bin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {median_ebreadth_s <- median_ebreadth_s + 
    scale_x_reverse() +
    geom_point(data = southern_pt, aes(x = l_median, color = growthform, alpha = 0.5, size = weight)) + 
    geom_smooth(data = southern, aes(x = l_median, group = growthform, weight = weight), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = southern, aes(x = l_median, group = growthform, color = growthform, weight = weight), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {median_ebreadth_s <- median_ebreadth_s + 
    geom_density_2d_filled(aes(x = l_median))}
  
  median_ebreadth_g <- ggplot(data = global, aes(y = e_breadth), color = growthform) +
    ggtitle("Global") +
    xlab('Latitudinal median') +
    ylab('Environmental breadth') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {median_ebreadth_g <- median_ebreadth_g + 
    geom_boxplot(data = global, aes(x = bin, group = interaction(bin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {median_ebreadth_g <- median_ebreadth_g + 
    geom_point(data = global_pt, aes(x = l_median, color = growthform, alpha = 0.5, size = weight)) + 
    geom_smooth(data = global, aes(x = l_median, group = growthform, weight = weight), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = global, aes(x = l_median, group = growthform, color = growthform, weight = weight), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {median_ebreadth_g <- median_ebreadth_g + 
    geom_density_2d_filled(aes(x = l_median))}
  
  #------------- Environmental breadth vs. latitudinal range
  
  ebreadth_range_n <- ggplot(data = northern, aes(y = l_range), color = growthform) +
    ggtitle("Northern hemisphere") +
    xlab('Environmental breadth') +
    ylab('Latitudinal range') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {ebreadth_range_n <- ebreadth_range_n + 
    geom_boxplot(data = northern, aes(x = ebin, group = interaction(ebin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {ebreadth_range_n <- ebreadth_range_n + 
    geom_point(data = northern_pt, aes(x = e_breadth, color = growthform, alpha = 0.5, size = weight)) + 
    geom_smooth(data = global, aes(x = e_breadth, group = growthform, weight = weight), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = global, aes(x = e_breadth, group = growthform, color = growthform, weight = weight), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {ebreadth_range_n <- ebreadth_range_n + 
    geom_density_2d_filled(data = global, aes(x = e_breadth))}
  
  ebreadth_range_s <- ggplot(data = southern, aes(y = l_range), color = growthform) +
    ggtitle("Southern hemisphere") +
    xlab('Environmental breadth') +
    ylab('Latitudinal range') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {ebreadth_range_s <- ebreadth_range_s +
    geom_boxplot(data = southern, aes(x = ebin, group = interaction(ebin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {ebreadth_range_s <- ebreadth_range_s + 
    geom_point(data = southern_pt, aes(x = e_breadth, color = growthform, alpha = 0.5, size = weight)) + 
    geom_smooth(data = global, aes(x = e_breadth, group = growthform, weight = weight), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = global, aes(x = e_breadth, group = growthform, color = growthform, weight = weight), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {ebreadth_range_s <- ebreadth_range_s + 
    geom_density_2d_filled(data = global, aes(x = e_breadth))}
  
  ebreadth_range_g <- ggplot(data = global, aes(y = l_range), color = growthform) +
    ggtitle("Global") +
    xlab('Environmental breadth') +
    ylab('Latitudinal range') +
    theme(text = element_text(size = 25)) +
    theme_classic()
  if (box_or_points == "box") {ebreadth_range_g <- ebreadth_range_g + 
    geom_boxplot(data = global, aes(x = ebin, group = interaction(ebin, growthform), color = growthform)) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "points") {ebreadth_range_g <- ebreadth_range_g +
    geom_point(data = global_pt, aes(x = e_breadth, color = growthform, alpha = 0.5, size = weight)) +
    geom_smooth(data = global, aes(x = e_breadth, group = growthform, weight = weight), method = "lm", color = "black", size = 2, show.legend = FALSE) +
    geom_smooth(data = global, aes(x = e_breadth, group = growthform, color = growthform, weight = weight), method = "lm", size = 1.7, se = FALSE) +
    scale_color_discrete(name = "Growthforms")}
  if (box_or_points == "heat") {ebreadth_range_g <- ebreadth_range_g + 
    geom_density_2d_filled(data = global, aes(x = e_breadth))}
  
  
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
                box_or_points = "heat")
p

ggsave("./2_nine_fig/nine_plots_hp_mad.jpg",
       width = 3840*1.5, height = 2160*1.5, units = "px")



