library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")
library("viridis")

setwd("/Users/marco/GitHub/environmental_breadth_final/"); getwd()

# Before running this script, run 5_coefficients.R
allres_eblr <- as.data.frame(allres_eblr)
allres_lmeb <- as.data.frame(allres_lmeb)
allres_lmlr <- as.data.frame(allres_lmlr)

# -------------------------------------------------------------------------------------------------------
##### Import and filter data

niche_data <- read_csv("./3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4)); range(niche_data$e_breadth)
zones <- read_csv("./3_generated_data/zones_v3.csv")
niche_data <- left_join(niche_data, zones, by = "Species"); ds <- niche_data

# -------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------

##### Results plots

# Defining color palette
mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))

# -------------------------------------------------------------------------------------------------------
# Figure 2A: The latitudinal gradient of latitudinal range - Global, tree

F2A_data <- niche_data %>% filter(growthform == "tree"); range(F2A_data$lat_range_sd_g)
F2A_breaks <- quantile(F2A_data[["lat_median_g"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2A <- ggplot(data = F2A_data,
              aes(x = lat_median_g, y = lat_range_sd_g)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2A_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  
  ggtitle("Global, tree") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +

  theme_classic() +
  theme(text = element_text(size = 17)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 10))

# -------------------------------------------------------------------------------------------------------
# Figure 2B: The latitudinal gradient of latitudinal range - Global, herb
  
F2B_data <- niche_data %>% filter(growthform == "herb"); range(F2B_data$lat_range_sd_g)
F2B_breaks <- quantile(F2B_data[["lat_median_g"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2B <- ggplot(data = F2B_data,
              aes(x = lat_median_g, y = lat_range_sd_g)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2B_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  
  ggtitle("Global, herb") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  
  theme_classic() +
  theme(text = element_text(size = 17)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim=c(0, 10))

# -------------------------------------------------------------------------------------------------------
# Figure 2C: The latitudinal gradient of latitudinal range - Northern hemisphere, tree

F2C_data <- niche_data %>% filter(growthform == "tree"); range(F2C_data$lat_range_sd_g)
F2C_breaks <- quantile(F2C_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2C <- ggplot(data = F2C_data,
              aes(x = lat_median_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2C_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_n"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Northern hemisphere, tree") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 17, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 55, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = 17)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# -------------------------------------------------------------------------------------------------------
# Figure 2D: The latitudinal gradient of latitudinal range - Northern hemisphere, herb

F2D_data <- niche_data %>% filter(growthform == "herb"); range(F2D_data$lat_range_sd_n)
F2D_breaks <- quantile(F2D_data[["lat_median_n"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2D <- ggplot(data = F2D_data,
              aes(x = lat_median_n, y = lat_range_sd_n)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2D_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_n"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Northern hemisphere, herb") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 17, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "North") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 55, y = 7, size = 5, 
           label = paste("β =",
                        allres_lmlr %>%
                          filter(hemisphere == "North") %>%
                          filter(growthform == "herb") %>%
                          filter(zone == "else") %>%
                          .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = 17)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# -------------------------------------------------------------------------------------------------------
# Figure 2E: The latitudinal gradient of latitudinal range - Southern hemisphere, tree

F2E_data <- niche_data %>% filter(growthform == "tree"); range(F2E_data$lat_range_sd_s)
F2E_breaks <- quantile(F2E_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2E <- ggplot(data = F2E_data,
              aes(x = lat_median_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2E_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_s"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Southern hemisphere, tree") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 17, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 55, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "tree") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = 17)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# -------------------------------------------------------------------------------------------------------
# Figure 2F: The latitudinal gradient of latitudinal range - Southern hemisphere, herb

F2F_data <- niche_data %>% filter(growthform == "herb"); range(F2F_data$lat_range_sd_s)
F2F_breaks <- quantile(F2F_data[["lat_median_s"]], probs = seq(0, 1, by = 0.05), na.rm = TRUE)

F2F <- ggplot(data = F2F_data,
              aes(x = lat_median_s, y = lat_range_sd_s)) +
  
  geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  stat_summary_bin(breaks = F2F_breaks,
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  geom_smooth(
    aes(linetype = .data[["zone_s"]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  
  ggtitle("Southern hemisphere, herb") +
  xlab("Latitudinal median") +
  ylab("Latitudinal range (SD)") +
  annotate("text", x = 17, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "tropical") %>%
                           .$valse)) +
  annotate("text", x = 55, y = 7, size = 5, 
           label = paste("β =",
                         allres_lmlr %>%
                           filter(hemisphere == "South") %>%
                           filter(growthform == "herb") %>%
                           filter(zone == "else") %>%
                           .$valse)) +
  
  theme_classic() +
  theme(text = element_text(size = 17)) +
  scale_fill_manual(values = mycolors) +
  coord_cartesian(ylim = c(0, 7.5), xlim = c(0, 70))

# -------------------------------------------------------------------------------------------------------

F2 <- cowplot::plot_grid(F2A,
                        F2B,
                        F2C,
                        F2D,
                        F2E,
                        F2F,
                        rel_widths = c(1.3, 1, 1,
                                       1.3, 1, 1),
                        ncol = 3, byrow = FALSE,
                        labels = c("A", "C", "E",
                                   "B", "D", "F"),
                        label_size = 20)

ggsave("./tmp/new_figure_1.jpg",
       width = 4000, height = 2000, units = "px")

# -------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------



  coord_cartesian(ylim=c(y_lim_s, y_lim_n), xlim = c(0, x_lim)) +
  geom_smooth(
    aes(x = df[[xc]], linetype = df[[zonec]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  annotate("text", x= label_trop_x, y= label_trop_y, label= label_trop, size=2.5) +
  annotate("text", x= label_notrop_x, y= label_notrop_y, label= label_notrop, size=2.5)
  
  
  
  
ggplot(data = df, 
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
                   aes(x = df[[xc]]),
                   breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05), na.rm = TRUE),
                   fun = mean, 
                   fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                   fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                   color = "azure4") +
  coord_cartesian(ylim=c(y_lim_s, y_lim_n), xlim = c(0, x_lim)) +
  geom_smooth(
    aes(x = df[[xc]], linetype = df[[zonec]]),
    color = "black",
    method = "lm",
    na.rm = TRUE,
    show.legend = FALSE) +
  annotate("text", x= label_trop_x, y= label_trop_y, label= label_trop, size=2.5) +
  annotate("text", x= label_notrop_x, y= label_notrop_y, label= label_notrop, size=2.5)



lm_lr_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                         xc = "lat_median_n", 
                         yc = "lat_range_sd_n",
                         gfc = "growthform",
                         binc = "bin",
                         gf_sel = "tree",
                         ttitle = "Northern hemisphere",
                         xxlab = 'Latitudinal median',
                         yylab = 'Latitudinal range',
                         plot_type = plot.type,
                         binning = bin.ning,
                         y_lim_s = 0,
                         y_lim_n = 7.5,
                         x_lim = 70,
                         zonec = "zone_n",
                         label_trop = paste("Slope:\n",
                                            allres_lmlr_2 %>%
                                              filter(hemisphere == 1) %>%
                                              filter(growthform == "tree") %>%
                                              filter(zone == "tropical") %>%
                                              .$val),
                         label_notrop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "else") %>%
                                                .$val),
                         label_trop_x = 8,
                         label_trop_y = 7,
                         label_notrop_x = 35,
                         label_notrop_y = 7)



my_plotting <- function(df, 
                        xc, 
                        yc,
                        gfc,
                        binc,
                        gf_sel,
                        ttitle,
                        xxlab,
                        yylab,
                        plot_type,
                        binning,
                        y_lim_s,
                        y_lim_n,
                        x_lim,
                        zonec,
                        label_trop,
                        label_notrop,
                        label_trop_x,
                        label_trop_y,
                        label_notrop_x,
                        label_notrop_y) {
  
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
                     aes(x = df[[xc]]),
                     breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05), na.rm = TRUE),
                     fun = mean, 
                     fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                     fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                     color = "azure4") +
    coord_cartesian(ylim=c(y_lim_s, y_lim_n), xlim = c(0, x_lim)) +
    geom_smooth(
      aes(x = df[[xc]], linetype = df[[zonec]]),
      color = "black",
      method = "lm",
      na.rm = TRUE,
      show.legend = FALSE) +
    annotate("text", x= label_trop_x, y= label_trop_y, label= label_trop, size=2.5) +
    annotate("text", x= label_notrop_x, y= label_notrop_y, label= label_notrop, size=2.5)
  
  return(p)
}

my_plotting_g <- function(df, 
                          xc, 
                          yc,
                          gfc,
                          binc,
                          gf_sel,
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
    theme_classic() +
    scale_fill_manual(values = mycolors) +
    geom_density_2d_filled(data = df, 
                           aes(x = .data[[xc]]),
                           bins = 20, show.legend = FALSE) +
    stat_summary_bin(data = df, 
                     aes(x = df[[xc]]),
                     breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05), na.rm = TRUE),
                     fun = mean, 
                     fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                     fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                     color = "azure4") +
    coord_cartesian(ylim=c(y_lim_s, y_lim_n))
  
  
  return(p)
}

my_plotting_g_2 <- function(df, 
                            xc, 
                            yc,
                            gfc,
                            binc,
                            gf_sel,
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
    theme_classic() +
    scale_fill_manual(values = mycolors) +
    geom_density_2d_filled(data = df, 
                           aes(x = .data[[xc]]),
                           bins = 20, show.legend = FALSE) +
    stat_summary_bin(data = df, 
                     aes(x = df[[xc]]),
                     breaks = quantile(df[[xc]], probs = seq(0, 1, by = 0.05), na.rm = TRUE),
                     fun = mean, 
                     fun.min = function(x) mean(x)-sd(x)/sqrt(length(x)), 
                     fun.max = function(x) mean(x)+sd(x)/sqrt(length(x)),
                     color = "azure4") +
    geom_smooth(
      aes(x = df[[xc]]),
      color = "black",
      method = "lm",
      na.rm = TRUE,
      show.legend = FALSE) +
    coord_cartesian(ylim=c(y_lim_s, y_lim_n))
  
  
  return(p)
}


# -------------------------------------------------------------------------------------------------------
##### Creating plot grid

# Figure 1: Rapoport's rule

allres_lmlr_2 <- allres_lmlr %>% mutate(hemisphere = ifelse(hemisphere == "North", 1, -1))

rapoport <- function(ds,
                     #growthform = "tree",
                     plot.type = "p_mean",
                     bin.ning = "eq_points") {
  
  ### Trees
  
  lm_lr_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  lm_lr_t_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  lm_lr_t_g <- my_plotting_g(ds,
                             xc = "lat_median_g", 
                             yc = "lat_range_sd_g",
                             gfc = "growthform",
                             binc = "bin",
                             gf_sel = "tree",
                             ttitle = "Global",
                             xxlab = 'Latitudinal median',
                             yylab = 'Latitudinal range',
                             plot_type = plot.type,
                             binning = bin.ning,
                             y_lim_s = 0,
                             y_lim_n = 10)
  
  ### Herbs
  
  lm_lr_h_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  lm_lr_h_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 7.5,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope:\n",
                                              allres_lmlr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope:\n",
                                                allres_lmlr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 8,
                           label_trop_y = 7,
                           label_notrop_x = 35,
                           label_notrop_y = 7)
  
  
  lm_lr_h_g <- my_plotting_g(ds,
                             xc = "lat_median_g", 
                             yc = "lat_range_sd_g",
                             gfc = "growthform",
                             binc = "bin",
                             gf_sel = "herb",
                             ttitle = "Global",
                             xxlab = 'Latitudinal median',
                             yylab = 'Latitudinal range',
                             plot_type = plot.type,
                             binning = bin.ning,
                             y_lim_s = 0,
                             y_lim_n = 10)
  
  
  p <- cowplot::plot_grid(lm_lr_t_g,
                          lm_lr_h_g,
                          lm_lr_t_n,
                          lm_lr_h_n,
                          lm_lr_t_s,
                          lm_lr_h_s, 
                          rel_widths = c(1.3, 1, 1,
                                         1.3, 1, 1),
                          ncol = 3, byrow = FALSE,
                          labels = c("A", "C", "E",
                                     "B", "D", "F"),
                          label_size = 25)
  
  return(p)
}

figure1 <- rapoport(ds = niche_data,
                    #growthform = "herb",
                    plot.type = "p_mean",
                    bin.ning = "eq_points")

ggsave("./../tmp/figure1_v3.jpg",
       width = 4000, height = 2000, units = "px")

# -------------------------------------------------------------------------------------------------------
# Figure 2: Environmental breadth and latitudinal range

allres_eblr_2 <- allres_eblr %>% mutate(hemisphere = ifelse(hemisphere == "North", 1, -1))

range <- function(ds,
                  #growthform = "tree",
                  plot.type = "p_mean",
                  bin.ning = "eq_points") {
  
  ### Trees
  
  eb_lr_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning, 
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(zone == "tropical") %>%
                                                filter(growthform == "tree") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(zone == "else") %>%
                                                  filter(growthform == "tree") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  eb_lr_t_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  eb_lr_t_g <- my_plotting_g_2(ds,
                               xc = "e_breadth", 
                               yc = "lat_range_sd_g",
                               gfc = "growthform",
                               binc = "bin",
                               gf_sel = "tree",
                               ttitle = "Global",
                               xxlab = 'Environmental breadth',
                               yylab = 'Latitudinal range',
                               plot_type = plot.type,
                               binning = bin.ning,
                               y_lim_s = 0,
                               y_lim_n = 10)
  
  ### Herbs
  
  eb_lr_h_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_n",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  eb_lr_h_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "e_breadth", 
                           yc = "lat_range_sd_s",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Environmental breadth',
                           yylab = 'Latitudinal range',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 10,
                           x_lim = 1,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_eblr_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_eblr_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 0.10,
                           label_trop_y = 9,
                           label_notrop_x = 0.5,
                           label_notrop_y = 9)
  
  
  eb_lr_h_g <- my_plotting_g_2(ds,
                               xc = "e_breadth", 
                               yc = "lat_range_sd_g",
                               gfc = "growthform",
                               binc = "bin",
                               gf_sel = "herb",
                               ttitle = "Global",
                               xxlab = 'Environmental breadth',
                               yylab = 'Latitudinal range',
                               plot_type = plot.type,
                               binning = bin.ning,
                               y_lim_s = 0,
                               y_lim_n = 10)
  
  
  p <- cowplot::plot_grid(eb_lr_t_g,
                          eb_lr_h_g,
                          eb_lr_t_n,
                          eb_lr_h_n,
                          eb_lr_t_s,
                          eb_lr_h_s, 
                          rel_widths = c(1.3, 1, 1,
                                         1.3, 1, 1),
                          ncol = 3, byrow = FALSE,
                          labels = c("A", "C", "E",
                                     "B", "D", "F"),
                          label_size = 25)
  
  return(p)
}

# tropical = dashed
figure2 <- range(ds = niche_data,
                 #growthform = "herb",
                 plot.type = "p_mean",
                 bin.ning = "eq_points")

ggsave("./../tmp/figuree2_v3.jpg",
       width = 4000, height = 2000, units = "px")

# -------------------------------------------------------------------------------------------------------
# Figure 3: Environmental breadth and median latitude

allres_lmeb_2 <- allres_lmeb %>% mutate(hemisphere = ifelse(hemisphere == "North", 1, -1))

lmebreadth <- function(ds,
                       #growthform = "tree",
                       plot.type = "p_mean",
                       bin.ning = "eq_points") {
  
  ### Trees
  
  lm_eb_t_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning, 
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  lm_eb_t_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "tree",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "tree") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "tree") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  lm_eb_t_g <- my_plotting_g(ds,
                             xc = "lat_median_g", 
                             yc = "e_breadth",
                             gfc = "growthform",
                             binc = "bin",
                             gf_sel = "tree",
                             ttitle = "Global",
                             xxlab = 'Latitudinal median',
                             yylab = 'Environmental breadth',
                             plot_type = plot.type,
                             binning = bin.ning,
                             y_lim_s = 0,
                             y_lim_n = 1)
  
  ### Herbs
  
  lm_eb_h_n <- my_plotting(ds %>% filter(!is.na(lat_median_n)),
                           xc = "lat_median_n", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Northern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_n",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == 1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == 1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  lm_eb_h_s <- my_plotting(ds %>% filter(!is.na(lat_median_s)),
                           xc = "lat_median_s", 
                           yc = "e_breadth",
                           gfc = "growthform",
                           binc = "bin",
                           gf_sel = "herb",
                           ttitle = "Southern hemisphere",
                           xxlab = 'Latitudinal median',
                           yylab = 'Environmental breadth',
                           plot_type = plot.type,
                           binning = bin.ning,
                           y_lim_s = 0,
                           y_lim_n = 1,
                           x_lim = 70,
                           zonec = "zone_s",
                           label_trop = paste("Slope tropical:\n",
                                              allres_lmeb_2 %>%
                                                filter(hemisphere == -1) %>%
                                                filter(growthform == "herb") %>%
                                                filter(zone == "tropical") %>%
                                                .$val),
                           label_notrop = paste("Slope else:\n",
                                                allres_lmeb_2 %>%
                                                  filter(hemisphere == -1) %>%
                                                  filter(growthform == "herb") %>%
                                                  filter(zone == "else") %>%
                                                  .$val),
                           label_trop_x = 7,
                           label_trop_y = 0.9,
                           label_notrop_x = 35,
                           label_notrop_y = 0.9)
  
  
  lm_eb_h_g <- my_plotting_g(ds,
                             xc = "lat_median_g", 
                             yc = "e_breadth",
                             gfc = "growthform",
                             binc = "bin",
                             gf_sel = "herb",
                             ttitle = "Global",
                             xxlab = 'Latitudinal median',
                             yylab = 'Environmental breadth',
                             plot_type = plot.type,
                             binning = bin.ning,
                             y_lim_s = 0,
                             y_lim_n = 1)
  
  
  p <- cowplot::plot_grid(lm_eb_t_g,
                          lm_eb_h_g,
                          lm_eb_t_n,
                          lm_eb_h_n,
                          lm_eb_t_s,
                          lm_eb_h_s, 
                          rel_widths = c(1.3, 1, 1,
                                         1.3, 1, 1),
                          ncol = 3, byrow = FALSE,
                          labels = c("A", "C", "E",
                                     "B", "D", "F"),
                          label_size = 25)
  
  return(p)
}

# tropical = dashed
figure3 <- lmebreadth(ds = niche_data,
                      #growthform = "herb",
                      plot.type = "p_mean",
                      bin.ning = "eq_points")

ggsave("./../tmp/figure3_v3.jpg",
       width = 4000, height = 2000, units = "px")





