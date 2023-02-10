### Analysis of the tree species for publication

library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")

base::setwd("/Users/marco/GitHub/environmental_breadth/"); getwd()
dir_archive <- "/Users/marco/Documents/env_breadth_archive/"

# -------------------------------------------------------------------------------------------------------
##### Import and filter data

raw_niche_data <- read_csv("./raw_data_fully_full.csv")

niche_data <- raw_niche_data %>%
  filter(auc.val.avg > 0.55) %>%
  filter(0.05 < or.10p.avg) %>%
  filter(or.10p.avg < 0.3)

table(raw_niche_data$growthform)

# -------------------------------------------------------------------------------------------------------
##### Exploratory plots regarding the MESS values

ggplot(data = niche_data, aes(x = lat_median, y = log10(weight), color = "#00BFC4")) +
  geom_point(alpha = 0.5, color = "#00BFC4") +
  geom_smooth(color = "black") +
  xlab('Latitudinal median') +
  ylab('MESS weight') +
  theme_classic() +
  theme(text = element_text(size = 25))

ggsave(paste(dir_archive, "plots/LM_MESS.jpeg", sep = ""))


ggplot(data = niche_data, aes(x = lat_range, y = sqrt(env_breadth), color = "#00BFC4")) +
  geom_point(alpha = 0.5, color = "#00BFC4", aes(size = weight)) +
  geom_smooth(color = "black", aes(weight = weight)) +
  #xlab('Latitudinal median') +
  #ylab('MESS weight') +
  theme_classic() +
  theme(text = element_text(size = 25))



ll <- ggplot(data = niche_data %>% slice_sample(n = 30000), aes(color = growthform)) +
  #geom_smooth(aes(x = lat_range, y = lat_hlimit, linetype = "High latitude limit"), se=FALSE) +
  geom_point(aes(x = lat_range, y = lat_llimit), alpha = 0.5) +
  #geom_abline(intercept = 0, slope = 1, color = "black") +
  #geom_smooth(aes(x = lat_range, y = lat_hlimit, linetype = "High latitude limit"), show.legend = FALSE) +
  #geom_smooth(aes(x = lat_range, y = lat_llimit, linetype = "Low latitude limit"), se = FALSE) +
  #geom_smooth(aes(x = lat_range, y = lat_llimit, linetype = "Low latitude limit"), linetype = "dashed", show.legend = FALSE) +
  xlab('Latitudinal range') +
  ylab('Low latitudinal limit') +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 60)) +
  theme_classic() +
  theme(text = element_text(size = 25))

limits <- ggplot(data = niche_data, aes(color = growthform)) +
  geom_smooth(aes(x = lat_range, y = lat_hlimit, linetype = "High latitude limit"), se=FALSE) +
  #geom_point(aes(x = lat_range, y = lat_llimit)) +
  #geom_abline(intercept = 0, slope = 1, color = "black") +
  geom_smooth(aes(x = lat_range, y = lat_hlimit, linetype = "High latitude limit"), show.legend = FALSE) +
  geom_smooth(aes(x = lat_range, y = lat_llimit, linetype = "Low latitude limit"), se = FALSE) +
  geom_smooth(aes(x = lat_range, y = lat_llimit, linetype = "Low latitude limit"), linetype = "dashed", show.legend = FALSE) +
  xlab('Latitudinal range') +
  ylab('Latitudinal limit') +
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 60)) +
  theme_classic() +
  theme(text = element_text(size = 25))

ggarrange(hl,
          ll,
          limits,
          nrow = 1, ncol = 3,
          common.legend = TRUE, legend = "right",
          legend.grob = get_legend(limits))


ggsave("./Latitude_limit.jpeg")

(dens_lm <- ggplot(niche_data, aes(lat_median, color = growthform)) +
    geom_density(aes(y = after_stat(count))) +
    xlab('Median latitude') +
    ylab('Number of included species (sqrt)') +
    scale_y_sqrt(breaks = c(10, 100, 1000, 2500)) +
    #coord_cartesian(ylim = c(0, 1)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    ))

(dens_lm <- ggplot(niche_data, aes(auc.val.avg, color = growthform)) +
    geom_density() +
    #xlab('Median latitude') +
    #ylab('Number of included species (sqrt)') +
    #scale_y_sqrt(breaks = c(10, 100, 1000, 2500)) +
    #coord_cartesian(ylim = c(0, 1)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    ))

ggsave("./../20221115/env_weight.jpg")

# -------------------------------------------------------------------------------------------------------------------------------------
# Figure 1: compatison of latitudinal gradients of niche breadth and the relationship between niche breadth and latitudinal range

s_lat_env <- ggplot(niche_data, aes(lat_median_s, sqrt(env_breadth), color = growthform, group = growthform)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8) +
    #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +   
    xlab('Latitudinal median') +
    ylab('Niche breadth (B2), sqrt') +
    coord_cartesian(ylim=c(0, 1)) +
    #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    )

s_lr_env <- ggplot(niche_data, aes(lat_range_s, sqrt(env_breadth), color = growthform, group = growthform)) +
    geom_point(size = 1, alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8) +
    #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +   
    xlab('Latitudinal range') +
    ylab('Niche breadth (B2), sqrt') +
    #coord_cartesian(ylim=c(0,8.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    )

n_lat_env <- ggplot(niche_data, aes(lat_median_n, sqrt(env_breadth), color = growthform, group = growthform)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8) +
    #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +
    xlab('Latitudinal median') +
    ylab('Niche breadth (B2), sqrt') +
    coord_cartesian(ylim=c(0, 1)) +
    #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    )

n_lr_env <- ggplot(niche_data, aes(lat_range_n, sqrt(env_breadth), color = growthform, group = growthform)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8) +
    #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +
    xlab('Latitudinal range') +
    ylab('Niche breadth (B2), sqrt') +
    coord_cartesian(ylim=c(0, 1)) +
    #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    )

lat_env <- ggplot(niche_data, aes(lat_median, sqrt(env_breadth), color = growthform, group = growthform)) +
    geom_point(size = 1, alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8) +
    #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +
    xlab('Latitudinal median') +
    ylab('Niche breadth (B2), sqrt') +
    coord_cartesian(ylim=c(0, 1)) +
    #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
    )

lr_env <- ggplot(niche_data, aes(lat_range, sqrt(env_breadth), color = growthform, group = growthform)) +
    geom_point(size = 1, alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8) +
    #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +
    xlab('Latitudinal range') +
    ylab('Niche breadth (B2), sqrt') +
    coord_cartesian(ylim=c(0, 1)) +
    #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
    theme_classic() +
    labs(colour = "Group", linetype = "") +
    theme_classic() +
    guides(color=guide_legend(override.aes = list(fill = NA))) +
    theme(text = element_text(size = 25), 
          legend.key = element_rect(fill = "transparent", color = "transparent")
          )

ggarrange(s_lat_env + ggtitle("Southern Hemisphere"), 
          n_lat_env + ggtitle("Northern Hemisphere"),
          lat_env + ggtitle("both Hemispheres"),
          s_lr_env, 
          n_lr_env, 
          lr_env,
          nrow = 2, ncol = 3,
          common.legend = TRUE, legend = "right",
          legend.grob = get_legend(lr_env))

# -------------------------------------------------------------------------------------------------------------------------------------
# Figure 2: latitudinal gradient of latitudinal range

s_lat_lr <- ggplot(niche_data %>% filter(lat_range_s > 0), aes(lat_median_s, log10(lat_range_s), color = growthform, group = growthform)) +
    geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), size = 1, alpha = 0.5) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
    geom_smooth(method = "gam", alpha = .15, size = 1.8, se = FALSE) +
    #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1) +   
    xlab('Latitudinal median') +
    ylab('Latitudinal range') +
    #coord_cartesian(ylim=c(0,1.5)) +
    #coord_cartesian(ylim=c(0,8.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
          )

n_lat_lr <- ggplot(niche_data %>% filter(lat_range_n > 0), aes(lat_median_n, log10(lat_range_n), color = growthform, group = growthform)) +
    geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), size = 1, alpha = 0.5) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
    geom_smooth(method = "gam", alpha = .15, size = 1.8, se = FALSE) +
    #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1) +
    xlab('Latitudinal median') +
    ylab('Latitudinal range') +
    #coord_cartesian(ylim=c(0, 1.5)) +
    #coord_cartesian(ylim=c(0,8.5), xlim = c(0,50)) +
    theme_classic() +
    theme(text = element_text(size = 25)
          )

lat_lr <- ggplot(niche_data, aes(lat_median, lat_range, color = growthform, group = growthform)) +
    geom_point(size = 1, alpha = 0.1) +
    geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
    geom_smooth(method = "gam", alpha = .15, size = 1.8, se = FALSE) +
    #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1, aes(linetype = "Expectations")) +
    xlab('Latitudinal median') +
    ylab('Latitudinal range') +
    coord_cartesian(ylim=c(0,40)) +
    #coord_cartesian(ylim=c(0,8.5), xlim = c(0,50)) +
    labs(colour = "Group", linetype = "") +
    theme_classic() +
    theme(text = element_text(size = 25),
          legend.key = element_rect(fill = "transparent", color = "transparent")
          )

ggarrange(s_lat_lr + ggtitle("Southern Hemisphere"), 
          n_lat_lr + ggtitle("Northern Hemisphere"),
          lat_lr + ggtitle("both Hemispheres"),
          nrow= 1, ncol = 3,
          common.legend = TRUE, legend="right",
          legend.grob = get_legend(lat_lr))

# -------------------------------------------------------------------------------------------------------------------------------------

ggsave("./pretty_quite_good_overview_plot_v2.jpg", width = 6400, height = 3600, units = "px")



m <- lm(niche_data$env_breadth ~ niche_data$lat_median)
summary(m)




# -------------------------------------------------------------------------------------------------------------------------------------
# Figure 1v2: compatison of latitudinal gradients of niche breadth and the relationship between niche breadth and latitudinal range

s_lat_env2 <- ggplot(niche_data %>% filter(lat_range > 0), aes(lat_median_s, sqrt(env_breadth), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% filter(lat_range > 0) %>% slice_sample(n = 5000), alpha = 0.5) +
  geom_smooth(method = "gam", alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
  geom_smooth(method = "gam", alpha = .15, size = 1.8, se = FALSE) +
  #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +   
  xlab('Latitudinal median') +
  ylab('Niche breadth (B2), sqrt') +
  ggtitle("Southern Hemisphere") +
  coord_cartesian(ylim=c(0, 1), xlim=c(0, 60)) +
  theme_classic() +
  theme(text = element_text(size = 25),
        title = element_text(size = 25)
  )
s_lat_env2 <- ggExtra::ggMarginal(s_lat_env2 + theme(legend.position = "none"), type = "density", groupColour = TRUE)

n_lat_env2 <- ggplot(niche_data %>% filter(lat_range > 0), aes(lat_median_n, sqrt(env_breadth), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% filter(lat_range > 0) %>% slice_sample(n = 5000), alpha = 0.5) +
  geom_smooth(method = "gam", alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
  geom_smooth(method = "gam", alpha = .15, size = 1.8, se = FALSE) +
  #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +
  xlab('Latitudinal median') +
  ylab('Niche breadth (B2), sqrt') +
  ggtitle("Northern Hemisphere") +
  coord_cartesian(ylim=c(0, 1)) +
  #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
  theme_classic() +
  theme(text = element_text(size = 25),
        title = element_text(size = 25)
  )
n_lat_env2 <- ggExtra::ggMarginal(n_lat_env2 + theme(legend.position = "none"), type = "density", groupColour = TRUE)

lat_env2 <- ggplot(niche_data %>% filter(lat_range > 0), aes(lat_median, sqrt(env_breadth), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% filter(lat_range > 0) %>% slice_sample(n = 5000), alpha = 0.3) +
  geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
  geom_smooth(method = "gam", alpha = .15, size = 1.8) +
  #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +
  xlab('Latitudinal median') +
  ylab('Niche breadth (B2), sqrt') +
  ggtitle("both Hemispheres") +
  coord_cartesian(ylim=c(0, 1)) +
  #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
  theme_classic() +
  theme(text = element_text(size = 25),
        title = element_text(size = 25)
  )
lat_env2 <- ggExtra::ggMarginal(lat_env2 + theme(legend.position = "none"), type = "density", groupColour = TRUE)

s_lr_env2 <- ggplot(niche_data %>% filter(lat_range_s > 0), aes(sqrt(env_breadth), log10(lat_range_s), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), alpha = 0.3) +
  geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
  geom_smooth(method = "gam", alpha = .15, size = 1.8) +
  #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +   
  xlab('Niche breadth (B2), sqrt') +
  ylab('Latitudinal range, log10') +
  #coord_cartesian(ylim=c(0, 1)) +
  #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
  theme_classic() +
  theme(text = element_text(size = 25)
  )
s_lr_env2 <- ggExtra::ggMarginal(s_lr_env2 + theme(legend.position = "none"), type = "density", groupColour = TRUE)

n_lr_env2 <- ggplot(niche_data %>% filter(lat_range_n > 0), aes(sqrt(env_breadth), log10(lat_range_n), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), alpha = 0.3) +
  geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
  geom_smooth(method = "gam", alpha = .15, size = 1.8) +
  #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +   
  xlab('Niche breadth (B2), sqrt') +
  ylab('Latitudinal range, log10') +
  #coord_cartesian(ylim=c(0, 1)) +
  #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
  theme_classic() +
  theme(text = element_text(size = 25)
  )
n_lr_env2 <- ggExtra::ggMarginal(n_lr_env2 + theme(legend.position = "none"), type = "density", groupColour = TRUE)

lr_env2 <- ggplot(niche_data %>% filter(lat_range > 0), aes(sqrt(env_breadth), log10(lat_range), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% filter(lat_range > 0) %>% slice_sample(n = 5000), alpha = 0.3) +
  geom_smooth(method = "gam", alpha = .15, size = 2, color = "black") +
  geom_smooth(method = "gam", alpha = .15, size = 1.8) +
  #geom_function(fun = ~ (.x^2)/2500, colour = "black", size = 1) +   
  xlab('Niche breadth (B2), sqrt') +
  ylab('Latitudinal range, log10') +
  #coord_cartesian(ylim=c(0, 1)) +
  #coord_cartesian(ylim=c(0.25, 0.5), xlim = c(0,50)) +
  theme_classic() +
  theme(text = element_text(size = 25)
  )
lr_env2 <- ggExtra::ggMarginal(lr_env2 + theme(legend.position = "none"), type = "density", groupColour = TRUE)

grid.arrange(s_lat_env2, 
             n_lat_env2,
             lat_env2,
             s_lr_env2, 
             n_lr_env2, 
             lr_env2, ncol = 3)

ggarrange(s_lat_env2, 
          n_lat_env2,
          lat_env2,
          s_lr_env2, 
          n_lr_env2, 
          lr_env2,
          nrow = 2, ncol = 3,
          common.legend = TRUE, legend = "right",
          legend.grob = get_legend(lr_env2))

# -------------------------------------------------------------------------------------------------------------------------------------
# Figure 2v2: compatison of latitudinal gradients of niche breadth and the relationship between niche breadth and latitudinal range

breadth <- ggplot(niche_data_w, aes(x = log(lat_range), y = sqrt(env_breadth), color = growthform, group = growthform)) +
  geom_point(data = niche_data_w %>% slice_sample(n = 5000), alpha = 0.5, color = "#00BFC4") +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 4),
              alpha = .15, size = 2, color = "black", se = TRUE) + #, aes(weight = niche_data_w$weight)
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 4), 
              alpha = .15, size = 1.7, color = "#00BFC4") + # "#00BFC4" #aes(weight = niche_data_w$weight)
  xlab('Latitudinal range') +
  ylab('Niche breadth (B2), sqrt') +
  #coord_cartesian(xlim = c(-1.5,1.5)) +
  theme_classic() +
  theme(text = element_text(size = 25)
  )

limits <- ggplot(data = niche_data, aes(color = growthform)) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 4),
              aes(x = log(lat_range), y = lat_hlimit, linetype = "High latitude limit"), size = 2, se=FALSE) +
  #geom_point(aes(x = lat_range, y = lat_llimit)) +
  #geom_abline(intercept = 0, slope = 1, color = "black") +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 4),
              aes(x = log(lat_range), y = lat_hlimit, linetype = "High latitude limit"), size = 2, show.legend = FALSE) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 4),
              aes(x = log(lat_range), y = lat_llimit, linetype = "Low latitude limit"), size = 2, se = FALSE) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 4),
              aes(x = log(lat_range), y = lat_llimit, linetype = "Low latitude limit"), size = 2, linetype = "dashed", show.legend = FALSE) +
  xlab('Latitudinal range') +
  ylab('Latitudinal limit') +
  #coord_cartesian(xlim = c(-1.5,1.5)) +
  theme_classic() +
  theme(text = element_text(size = 25))

#median <- ggplot(niche_data, aes(color = growthform, group = growthform)) +
#  #geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5) +
#  #geom_smooth(aes(x = lat_median_s, y = log10(lat_range_s)), method = "gam", alpha = .15, size = 2, color = "black") +
#  geom_smooth(aes(x = lat_median_s, y = lat_range_s), method = "gam", alpha = .15, size = 1.8) +
#  #geom_smooth(aes(x = lat_median_n, y = log10(lat_range_n)), method = "gam", alpha = .15, size = 2, linetype = "dashed", color = "black") +
#  geom_smooth(aes(x = lat_median_n, y = lat_range_n), method = "gam", alpha = .15, size = 1.8, linetype = "dashed", ) +
#  xlab('Latitudinal median') +
#  ylab('Latitudinal range, log10') +
#  #coord_cartesian(ylim = c(-1.5,1.5)) +
#  theme_classic() +
#  theme(text = element_text(size = 25)
#  )

ggarrange(breadth,
          ggarrange(n_lat_lr + coord_cartesian(ylim=c(-1.5, 1.5)), 
                    s_lat_lr + coord_cartesian(ylim=c(-1.5, 1.5)), 
                    nrow = 2, ncol = 1, legend = "none"),
          limits,
          nrow = 1, ncol = 3,
          common.legend = TRUE, legend = "right",
          legend.grob = get_legend(limits))

ggsave("./breadth_limits_median_RANGE_v3.jpeg", width = 3840*1.5, height = 2160*1.5, units =  "px")
