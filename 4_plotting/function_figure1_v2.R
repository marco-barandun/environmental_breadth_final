### Analysis of the tree species for publication

library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")
library(scales)


# --------------------- Notes
# To find a good transformation plot the hist of the envs and look hist((var)^1/2) --> sqrt seems the best
# ---------------------

base::setwd("/Users/marco/GitHub/environmental_breadth/"); getwd()
raw_niche_data <- read_csv("./raw_niche_data.csv")

niche_data_unnorm <- raw_niche_data %>%
  filter(auc.val.avg > 0.55) %>%
  filter(0.05 < or.10p.avg) %>%
  filter(or.10p.avg < 0.3) %>%
  mutate(bin = cut_width(lat_median, width=5))

niche_data_h <- niche_data_unnorm %>%
  filter(growthform == "herb") %>%
  mutate(lat_range_s_norm = bestNormalize::bestNormalize(lat_range_s)$x.t) %>%
  mutate(lat_range_n_norm = bestNormalize::bestNormalize(lat_range_n)$x.t) %>%
  mutate(env_breadth_norm = bestNormalize::bestNormalize(env_breadth)$x.t) %>%
  mutate(lat_median_norm = bestNormalize::bestNormalize(lat_median)$x.t)%>%
  mutate(lat_range_norm = bestNormalize::bestNormalize(abs(lat_range))$x.t) %>%
  mutate(weight_norm = bestNormalize::bestNormalize(weight)$x.t)

niche_data_t <- niche_data_unnorm %>%
  filter(growthform == "tree") %>%
  mutate(lat_range_s_norm = bestNormalize::bestNormalize(lat_range_s)$x.t) %>%
  mutate(lat_range_n_norm = bestNormalize::bestNormalize(lat_range_n)$x.t) %>%
  mutate(env_breadth_norm = bestNormalize::bestNormalize(env_breadth)$x.t) %>%
  mutate(lat_median_norm = bestNormalize::bestNormalize(lat_median)$x.t)%>%
  mutate(lat_range_norm = bestNormalize::bestNormalize(abs(lat_range))$x.t) %>%
  mutate(weight_norm = bestNormalize::bestNormalize(weight)$x.t)

niche_data <- rbind(niche_data_t, niche_data_h)

write_csv(niche_data, "./raw_data_bestnormalized.csv")

niche_data %>%
  ggplot(aes(x=bin, y=env_breadth, fill = growthform)) +
  geom_boxplot() +
  theme_classic()

xlm_b <- niche_data %>% 
  filter(lat_median_norm %in% quantile(lat_median_norm, probs = seq(0, 1, 0.05))) %>% 
  distinct(round(lat_median_norm, digits = 1), .keep_all = TRUE) %>% 
  add_row(niche_data %>% filter(lat_median %in% c(-0.2760))) %>%
  mutate(lat_median = round(lat_median, digits = 0))

ylm_b <- niche_data %>% 
  filter(round(env_breadth_norm, digits = 3) %in% round(quantile(niche_data$env_breadth_norm, probs = seq(0, 1, 0.1)), digits = 3)) %>% 
  distinct(round(env_breadth_norm, digits = 1), .keep_all = TRUE) %>% 
  distinct(round(env_breadth, digits = 1), .keep_all = TRUE) %>% 
  #add_row(niche_data %>% filter(lat_median %in% c(-0.2760))) %>%
  mutate(env_breadth = round(env_breadth, digits = 2))

lm_b <- ggplot(data = niche_data, aes(x = lat_median_norm, y = env_breadth_norm, weight = weight, color = growthform)) +
  geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5, aes(size = weight)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, linewidth = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            data = niche_data %>% filter(growthform == "herb") %>% filter(lat_median > 23.5)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, linewidth = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            data = niche_data %>% filter(growthform == "tree") %>% filter(lat_median > 23.5)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, linewidth = 1.7, se = FALSE,
  #            data = niche_data %>% filter(lat_median > 23.5)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            data = niche_data %>% filter(sign(lat_median) == 1)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 1.7, se = FALSE,
  #            data = niche_data %>% filter(sign(lat_median) == 1)) +
  scale_x_continuous(breaks = xlm_b$lat_median_norm, label = xlm_b$lat_median) + 
  scale_y_continuous(breaks = ylm_b$env_breadth_norm, label = ylm_b$env_breadth) + 
  theme_classic() +
  theme(text = element_text(size = 25))

xb_lr <- niche_data %>% 
  filter(round(env_breadth_norm, digits = 3) %in% round(quantile(niche_data$env_breadth_norm, probs = seq(0, 1, 0.1)), digits = 3)) %>% 
  distinct(round(env_breadth_norm, digits = 1), .keep_all = TRUE) %>% 
  distinct(round(env_breadth, digits = 1), .keep_all = TRUE) %>% 
  #add_row(niche_data %>% filter(lat_median %in% c(-0.2760))) %>%
  mutate(env_breadth = round(env_breadth, digits = 1))

yb_lr <- niche_data %>% 
  filter(lat_range_norm %in% quantile(lat_range_norm, probs = seq(0, 1, 0.05))) %>% 
  distinct(round(lat_range_norm, digits = 1), .keep_all = TRUE) %>% 
  distinct(round(lat_range, digits = 0), .keep_all = TRUE) %>% 
  #add_row(niche_data %>% filter(lat_median %in% c(-0.2760))) %>%
  mutate(lat_range = round(lat_range, digits = 0))

b_lr <- niche_data %>%
  ggplot(aes(x = env_breadth_norm, y = lat_range_norm, weight = weight, color = growthform)) +
  geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5, aes(size = weight)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 1.7, se = FALSE) +
  scale_x_continuous(breaks = xb_lr$env_breadth_norm, label = xb_lr$env_breadth) + 
  scale_y_continuous(breaks = yb_lr$lat_range_norm, label = yb_lr$lat_range) + 
  theme_classic() +
  theme(text = element_text(size = 25))

xlm_lr <- niche_data %>% 
  filter(lat_median_norm %in% quantile(lat_median_norm, probs = seq(0, 1, 0.05))) %>% 
  distinct(round(lat_median_norm, digits = 1), .keep_all = TRUE) %>% 
  add_row(niche_data %>% filter(lat_median %in% c(-0.2760))) %>%
  mutate(lat_median = round(lat_median, digits = 0))

ylm_lr <- niche_data %>% 
  filter(lat_range_norm %in% quantile(lat_range_norm, probs = seq(0, 1, 0.05))) %>% 
  distinct(round(lat_range_norm, digits = 1), .keep_all = TRUE) %>% 
  distinct(round(lat_range, digits = 0), .keep_all = TRUE) %>% 
  #add_row(niche_data %>% filter(lat_median %in% c(-0.2760))) %>%
  mutate(lat_range = round(lat_range, digits = 0))

lm_lr <- ggplot(data = niche_data, aes(x = lat_median_norm, y = lat_range_norm, color = growthform)) +
  geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
              alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
              data = niche_data %>% filter(growthform == "tree") %>% filter(lat_median > 0)) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
              alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
              data = niche_data %>% filter(growthform == "herb") %>% filter(lat_median > 0)) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
              alpha = .15, size = 1.7, se = FALSE,
              data = niche_data %>% filter(lat_median > 0)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            data = niche_data %>% filter(growthform == "tree") %>% filter(lat_median < 0)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            data = niche_data %>% filter(growthform == "herb") %>% filter(lat_median < 0)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 1.7, se = FALSE,
  #            data = niche_data %>% filter(lat_median < 0)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            data = niche_data %>% filter(sign(lat_median) == 1)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 1.7, se = FALSE,
  #            data = niche_data %>% filter(sign(lat_median) == 1)) +
  scale_x_continuous(breaks = xlm_lr$lat_median_norm, label = xlm_lr$lat_median) + 
  scale_y_continuous(breaks = ylm_lr$lat_range_norm, label = ylm_lr$lat_range) + 
  theme_classic() +
  theme(text = element_text(size = 25))



(p <- cowplot::plot_grid(lm_b + theme(legend.position = "none"), 
                   b_lr + theme(legend.position = "none"), 
                   rel_widths = c(1, 1, 1.5),
                   lm_lr,
                   ncol = 3, align = "h",
                   labels = c("A", "B", "C"),
                   label_size = 36))

ggsave("/Users/marco/GitHub/environmental_breadth/res_temp_exports/new_normalized_v2.jpeg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

# -----------------------------------------

niche_data_2 <- niche_data %>%
  mutate(lat_hl95q_abs = pmax(lat_95q_s, lat_95q_n, na.rm = TRUE)) %>%
  mutate(lat_ll05q_abs = ifelse(lat_95q_n > lat_95q_s,
                                ifelse(lat_s05q > 0, lat_s05q, 0),
                                ifelse(lat_s05q < 0, abs(lat_s05q), 0))) %>%
  mutate(lat_ll05q_abs = ifelse(is.na(lat_ll05q_abs), pmin(lat_05q_s, lat_05q_n, na.rm = TRUE), lat_ll05q_abs))

set.seed(443)
trees <- niche_data %>% #filter(growthform == "tree") %>% 
  slice_sample(n = 5000)


ds <- niche_data %>% #filter(growthform == "herb") %>% 
  filter(hemisphere == -1) %>% filter(lat_median > -23.5)
summary(lm(ds$lat_range_norm~ds$lat_median_norm))



ggplot(data = trees, aes(x = lat_median, color = growthform)) +
  geom_point(data = trees, alpha = 0.5, aes(y = lat_hl95q_abs, color = "lat_hl95q_abs")) +
  geom_point(data = trees, alpha = 0.5, aes(y = lat_ll05q_abs, color = "lat_ll05q_abs")) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            aes(y = lat_hl95q_abs)) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
              alpha = .15, size = 1.7, se = FALSE,
              aes(y = lat_hl95q_abs)) +
  #geom_smooth(method = "gam",
  #            formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
  #            alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
  #            aes(y = lat_ll05q_abs), linetype = "dashed") +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = 3),
              alpha = .15, size = 1.7, se = FALSE,
              aes(y = lat_ll05q_abs), linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 25))

# ----------------------------------------- weird stuff going on down here
ggsave("/Users/marco/GitHub/environmental_breadth/res_temp_exports/prova.jpeg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

nbor <- niche_data %>% filter(lat_median > 23.5)
trop <- niche_data %>% filter(lat_median < 23.5) %>% filter(lat_median > -23.5)
sbor <- niche_data %>% filter(lat_median < -23.5)

plot(hnds$lat_median_norm, hnds$env_breadth_norm, cex = hnds$weight)

cor.test(hnds$lat_median_norm, hnds$env_breadth_norm, method=c("pearson", "kendall", "spearman"))

cov.wt(hnds %>% select(lat_median_norm, env_breadth_norm), wt = hnds$weight, cor = TRUE)

ggpubr::ggscatter(nbor, x = "lat_median_norm", y = "env_breadth_norm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


summary(lm(nbor$env_breadth ~ nbor$lat_median, weights = nbor$weight))



wCorr::weightedCorr(nbor$lat_range_norm,
                    nbor$lat_s05q,
                    #weights = sbor$weight, 
                    method = "Pearson")


figure_1 <- function(niche_data, 
                     xlat_param, 
                     smoothing = 3,
                     weighted = FALSE) {
  
  if (xlat_param == "lat_median") {
    
    breadth_s <- ggplot(niche_data %>% filter(lat_range_s > 0), aes(x = lat_median_s, y = env_breadth_norm, color = growthform, group = growthform)) +
      xlab('Latitudinal median') +
      ylab('Niche breadth (B2), sqrt') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    if (weighted == FALSE) {
      breadth_s <- breadth_s + 
        geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), alpha = 0.5) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE, color = "#00BFC4")
    }
    if (weighted == TRUE) {
      breadth_s <- breadth_s + 
        geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), alpha = 0.5, aes(size = weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_s > 0) %>% .$weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_s > 0) %>% .$weight))
    }
    
    breadth_n <- ggplot(niche_data %>% filter(lat_range_n > 0), aes(x = lat_median_n, y = env_breadth_norm, color = growthform, group = growthform)) +
      xlab('Latitudinal median') +
      ylab('Niche breadth (B2), sqrt') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    if (weighted == FALSE) {
      breadth_n <- breadth_n + 
        geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), alpha = 0.5) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE)
    }
    if (weighted == TRUE) {
      breadth_n <- breadth_n + 
        geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), alpha = 0.5, aes(size = weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_n > 0) %>% .$weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_n > 0) %>% .$weight))
    }
    
    s_lat_lr <- ggplot(niche_data %>% filter(lat_range_s > 0), aes(x = lat_median_s, y = lat_range_s_norm, color = growthform, group = growthform)) +
      geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), size = 1, alpha = 0.5) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 1.8, se = FALSE) +
      #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1) +   
      xlab('Latitudinal median') +
      ylab('Latitudinal range') +
      coord_cartesian(ylim = c(-2.5,2.5)) +
      theme_classic() +
      theme(text = element_text(size = 25)
      )
    
    n_lat_lr <- ggplot(niche_data %>% filter(lat_range_n > 0), aes(lat_median_n, lat_range_n_norm, color = growthform, group = growthform)) +
      geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), size = 1, alpha = 0.5) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 1.8, se = FALSE) +
      #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1) +
      xlab('Latitudinal median') +
      ylab('Latitudinal range') +
      coord_cartesian(ylim = c(-2.5,2.5)) +
      theme_classic() +
      theme(text = element_text(size = 25)
      )
    
    limits_s <- ggplot(data = niche_data %>% filter(lat_range_s > 0), aes(color = growthform)) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median_s, y = lat_95q_s, linetype = "High latitude limit"), size = 2, se=TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median_s, y = lat_95q_s, linetype = "High latitude limit"), size = 2, se=FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median_s, y = lat_05q_s, linetype = "Low latitude limit"), size = 2, se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median_s, y = lat_05q_s, linetype = "Low latitude limit"), size = 2, se = FALSE) +
      xlab('Latitudinal median') +
      ylab('Latitudinal limit') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    
    limits_n <- ggplot(data = niche_data %>% filter(lat_range_n > 0), aes(color = c.olor)) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median, y = lat_95q_n, linetype = "High latitude limit"), size = 2, se=TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median, y = lat_95q_n, linetype = "High latitude limit"), size = 2, se=FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median, y = lat_05q_n, linetype = "Low latitude limit"), size = 2, se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_median, y = lat_05q_n, linetype = "Low latitude limit"), size = 2, se = FALSE) +
      xlab('Latitudinal median') +
      ylab('Latitudinal limit') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    
  }
  
  if (xlat_param == "lat_range") {
    
    breadth_s <- ggplot(niche_data %>% filter(lat_range_s > 0), aes(x = env_breadth_norm, y = lat_range_norm, color = growthform, group = growthform)) +
      #xlab('Niche breadth (B2)') +
      #ylab('Latitudinal range') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    if (weighted == FALSE) {
      breadth_s <- breadth_s + 
        geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE)
    }
    if (weighted == TRUE) {
      breadth_s <- breadth_s + 
        geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), alpha = 0.5, aes(size = weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_s > 0) %>% .$weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_s > 0) %>% .$weight))
    }
    
    breadth_n <- ggplot(niche_data %>% filter(lat_range_n > 0), aes(x = env_breadth_norm, y = lat_range_norm, color = growthform, group = growthform)) +
      #xlab('Latitudinal range') +
      #ylab('Niche breadth (B2)') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    if (weighted == FALSE) {
      breadth_n <- breadth_n + 
        geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE)
    }
    if (weighted == TRUE) {
      breadth_n <- breadth_n + 
        geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), alpha = 0.5, aes(size = weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_n > 0) %>% .$weight)) +
        geom_smooth(method = "gam",
                    formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                    alpha = .15, size = 1.7, se = FALSE,
                    aes(weight = niche_data %>% filter(lat_range_n > 0) %>% .$weight))
    }
    
    s_lat_lr <- ggplot(niche_data %>% filter(lat_range_s > 0), aes(x = lat_median_norm, y = lat_range_norm, color = growthform, group = growthform)) +
      geom_point(data = niche_data %>% filter(lat_range_s > 0) %>% slice_sample(n = 5000), size = 1, alpha = 0.5) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 1.8, se = FALSE) +
      #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1) +   
      #xlab('Latitudinal range') +
      #ylab('Latitudinal median') +
      #coord_cartesian(xlim = c(-2.5,2.5)) +
      theme_classic() +
      theme(text = element_text(size = 25)
      )
    
    n_lat_lr <- ggplot(niche_data %>% filter(lat_range_n > 0), aes(x = lat_median_norm, y = lat_range_norm, color = growthform, group = growthform)) +
      geom_point(data = niche_data %>% filter(lat_range_n > 0) %>% slice_sample(n = 5000), size = 1, alpha = 0.5) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  alpha = .15, size = 1.8, se = FALSE) +
      #geom_function(fun = ~ (.x^2)/50, colour = "black", size = 1) +
      #xlab('Latitudinal range') +
      #ylab('Latitudinal median') +
      #coord_cartesian(xlim = c(-2.5,2.5)) +
      theme_classic() +
      theme(text = element_text(size = 25)
      )
    
    limits_s <- ggplot(data = niche_data %>% filter(lat_range_s > 0), aes(color = growthform)) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_s_norm, y = lat_95q_s, linetype = "High latitude limit"), size = 2, se=TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_s_norm, y = lat_95q_s, linetype = "High latitude limit"), size = 2, se=FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_s_norm, y = lat_05q_s, linetype = "Low latitude limit"), size = 2, se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_s_norm, y = lat_05q_s, linetype = "Low latitude limit"), size = 2, se = FALSE) +
      xlab('Latitudinal range') +
      ylab('Latitudinal limit') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    
    limits_n <- ggplot(data = niche_data %>% filter(lat_range_n > 0), aes(color = growthform)) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_n_norm, y = lat_95q_n, linetype = "High latitude limit"), size = 2, se=TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_n_norm, y = lat_95q_n, linetype = "High latitude limit"), size = 2, se=FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_n_norm, y = lat_05q_n, linetype = "Low latitude limit"), size = 2, se = TRUE, show.legend = FALSE) +
      geom_smooth(method = "gam", 
                  formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                  aes(x = lat_range_n_norm, y = lat_05q_n, linetype = "Low latitude limit"), size = 2, se = FALSE) +
      xlab('Latitudinal range') +
      ylab('Latitudinal limit') +
      #coord_cartesian(xlim = c(-1.5,1.5)) +
      theme_classic() +
      theme(text = element_text(size = 25))
    
  }
  
  ggarrange(ggarrange(breadth_n, 
                      breadth_s, 
                      nrow = 2, ncol = 1, legend = "none"),
            ggarrange(n_lat_lr, 
                      s_lat_lr, 
                      nrow = 2, ncol = 1, legend = "none"),
            ggarrange(limits_n, 
                      limits_s, 
                      nrow = 2, ncol = 1, legend = "none"),
            nrow = 1, ncol = 3,
            common.legend = TRUE, legend = "right",
            legend.grob = get_legend(limits_n))
}

figure_1(niche_data = niche_data,
         xlat_param = "lat_range",
         smoothing = 4,
         weighted = TRUE)

ggsave("/Users/marco/GitHub/environmental_breadth/res_temp_exports/lat_range_normalised_v2.jpeg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

# --------------------- 
# Supplementary Materials
# ---------------------

(lat_auc <- ggplot(niche_data, aes(x = lat_range, y = auc.val.avg, color = growthform)) +
   geom_point() +
   geom_smooth(method = "lm")
   xlab('Average AUC value') +
   ylab('Number of included species') +
   theme_classic() +
   theme(text = element_text(size = 25),
         legend.position="none") +
   scale_fill_discrete(labels=c('Herbaceous', 'Tree')) +
   guides(fill = guide_legend("Growthform"))
)

### Figure S1A & B
(auc_plot <- ggplot(niche_data, aes(auc.val.avg, fill = growthform)) +
   geom_histogram(alpha = 0.5) +
   xlab('Average AUC value') +
   ylab('Number of included species') +
   theme_classic() +
   theme(text = element_text(size = 25),
         legend.position="none") +
   scale_fill_discrete(labels=c('Herbaceous', 'Tree')) +
   guides(fill = guide_legend("Growthform"))
   )

(or_plot <- ggplot(niche_data, aes(or.10p.avg, fill = growthform)) +
    geom_histogram(alpha = 0.5) +
    xlab('Average 10% omission rate') +
    ylab('Number of included species') +
    theme_classic() +
    theme(text = element_text(size = 25)) +
    scale_fill_discrete(labels=c('Herbaceous', 'Tree')) +
    guides(fill = guide_legend("Growthform"))
)

auc_or_plots <- cowplot::plot_grid(auc_plot, 
                   or_plot + theme(legend.position="none"),
                   labels = c('A', 'B'), label_size = 25)

legend_auc_or <- get_legend(
  # create some space to the left of the legend
  or_plot + theme(legend.box.margin = margin(0, 0, 0, 12)))

cowplot::plot_grid(auc_or_plots, legend_auc_or, rel_widths = c(3, .4))


ggsave("/Users/marco/Desktop/Final_plots/S1_AUC_OR.jpeg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

### -----------------------------------------------------------------------------------------------------------------------------------------------------------
### Figure S2A & B
herbs <- read_csv("/Users/marco/GitHub/niche_breadth_stuff/analysis_for_publication/config/ALLherbs_medianLat.csv") %>% mutate(growthform = "herb")
df_trees <- read_csv("/Users/marco/GitHub/niche_breadth_stuff/occurrences/ALL_Species_cleaned_occtest_ocean.csv") %>%
  rename(Species = taxonID) %>%
  rename(latitude = Latitude)
trees <- data.frame(sp = unique(df_trees$Species)) %>%
  mutate(lat_median = tapply(df_trees$latitude, df_trees$Species, function(x) round(median(x, na.rm = TRUE), digits = 3))) %>%
  mutate(lat_median_absolute = tapply(df_trees$latitude, df_trees$Species, function(x) round(median(abs(x), na.rm = TRUE), digits = 3))) %>%
  select(sp, lat_median, lat_median_absolute) %>%
  mutate(growthform = "tree") %>%
  mutate(sp = gsub(" ", "_", sp))

occs_not <- rbind(herbs, trees) %>%
  filter(!sp %in% niche_data$Species) %>%
  mutate(inc = "NO")

occs <- rbind(herbs, trees) %>%
  filter(sp %in% niche_data$Species) %>%
  mutate(inc = "YES") %>%
  bind_rows(., occs_not)

nr_lat_median_trees <- ggplot(occs %>% filter(growthform == "tree"), aes(lat_median, fill = inc)) +
    geom_histogram() +
    scale_fill_manual(values=c("grey60", "grey10")) +
    xlab('Median latitude') +
    ylab('Number of included species') +
    ylim(0, 20000) +
    scale_x_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75), limits = c(-80, 80)) +
    theme_classic() +
    theme(text = element_text(size = 25),
          legend.position="none")

nr_lat_median_herbs <- ggplot(occs %>% filter(growthform == "herb"), aes(lat_median, fill = inc)) +
    geom_histogram() +
    xlab('Median latitude') +
    ylab('Number of included species') +
    ylim(0, 20000) +
    scale_x_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75), limits = c(-80, 80)) +
    theme_classic() +
    theme(text = element_text(size = 25)) +
    scale_fill_manual(values=c("grey60", "grey10"), labels=c('Excluded', 'Included')) +
    guides(fill = guide_legend("Group"))

nr_lat_median_trees_herbs <- cowplot::plot_grid(nr_lat_median_trees, 
                                   nr_lat_median_herbs + theme(legend.position="none"),
                                   labels = c('A', 'B'), label_size = 25)

legend_tree_herbs <- get_legend(
  # create some space to the left of the legend
  nr_lat_median_herbs + theme(legend.box.margin = margin(0, 0, 0, 12)))

cowplot::plot_grid(nr_lat_median_trees_herbs, legend_tree_herbs, rel_widths = c(3, .4))

ggsave("/Users/marco/Desktop/Final_plots/S2_IncExc_TreesHerbs.jpeg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

# not all trees are present
# ---------------------
# End
# ---------------------








ggplot(data = niche_data, aes(color = growthform)) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
              aes(x = lat_range, y = lat_hlimit_n, linetype = "High latitude limit"), size = 2, se=TRUE, show.legend = FALSE) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
              aes(x = lat_range, y = lat_hlimit_n, linetype = "High latitude limit"), size = 2, se=FALSE) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
              aes(x = lat_range, y = lat_llimit_n, linetype = "Low latitude limit"), size = 2, se = TRUE, show.legend = FALSE) +
  geom_smooth(method = "gam", 
              formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
              aes(x = lat_range, y = lat_llimit_n, linetype = "Low latitude limit"), size = 2, se = FALSE) +
  xlab('Latitudinal median') +
  ylab('Latitudinal limit') +
  #coord_cartesian(xlim = c(-1.5,1.5)) +
  theme_classic() +
  theme(text = element_text(size = 25))

ggplot(niche_data, aes(x = log(lat_range_s), y = sqrt(env_breadth), color = growthform, group = growthform)) +
  geom_point(data = niche_data %>% slice_sample(n = 5000), alpha = 0.5) +
  xlab('Latitudinal range, log') +
  ylab('Niche breadth (B2), sqrt') +
  #coord_cartesian(xlim = c(-1.5,1.5)) +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  geom_smooth(method = "gam",
                                   formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                                   alpha = .15, size = 2, color = "black", se = TRUE, show.legend = FALSE) +
    geom_smooth(method = "gam",
                formula = y ~ s(x, bs = "cs", fx = TRUE, k = smoothing),
                alpha = .15, size = 1.7, se = FALSE)

