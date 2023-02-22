library(tidyverse)

setwd("/Users/marco/GitHub/environmental_breadth_final/3_generated_data")

niche_data <- read_csv("./../3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4))

A <- ggplot(data = niche_data, aes(x = n_inc_obs, group = growthform, fill = growthform)) +
  geom_density(stat = "count", show.legend = FALSE) +
  xlab("Number of included observations") +
  ylab("Number of species") +
  theme(text = element_text(size = 25)) +
  theme_classic() +
  coord_cartesian(xlim=c(0, 500)) + 
  scale_fill_grey(start = 0.35, 
                  end = .9,
                  name = "Growthform")

B <- ggplot(data = niche_data, aes(x = lat_median_g, group = growthform, fill = growthform)) +
  geom_density(alpha = 0.7, show.legend = FALSE) +
  xlab("Global median latitude of species") +
  ylab("Density of species") +
  theme(text = element_text(size = 25)) +
  theme_classic() +
  scale_fill_grey(start = 0, 
                  end = .9,
                  name = "Growthform")

C <- ggplot(data = niche_data, aes(x = lat_range_sd_g, group = growthform, fill = growthform)) +
  geom_density(alpha = 0.7) +
  xlab("Global latitudinal range of species (SD)") +
  ylab("Density of species") +
  theme(text = element_text(size = 25)) +
  theme_classic() +
  scale_fill_grey(start = 0, 
                  end = .9,
                  name = "Growthform") +
  coord_cartesian(xlim=c(0, 25))


cowplot::plot_grid(A,
                   B,
                   C,
                   rel_widths = c(1, 1, 1),
                   ncol = 3, align = "h",
                   labels = c("A", "B", "C"),
                   label_size = 25)

ggsave("./../tmp/f1.jpg",
       width = 4000, height = 1500, units = "px")



mycolors <- c("#00000000", colorRampPalette(c("#FFFFD6FF", "yellow", "orange", "red"), alpha=TRUE)(20))

A2 <- ggplot(data = niche_data %>% filter(growthform == "herb"), 
       aes(x = lat_range_sd_s, 
           y = lat_range_sd_g)) +
  ggtitle("Southern hemisphere herb") +
  geom_point(alpha = 0.3, size = 0.3) +
  scale_fill_manual(values = mycolors) +
 # geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  theme_classic() +
  xlab("Latitudinal range in the southern hemisphere (SD)") +
  ylab("Global latitudinal range (SD)")

B2 <- ggplot(data = niche_data %>% filter(growthform == "herb"), 
       aes(x = lat_range_sd_n, 
           y = lat_range_sd_g)) +
  ggtitle("Northern hemisphere herb") +
  geom_point(alpha = 0.3, size = 0.3) +
  scale_fill_manual(values = mycolors) +
 # geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  theme_classic() +
  xlab("Latitudinal range in the southern hemisphere (SD)") +
  ylab("Global latitudinal range (SD)")

C2 <- ggplot(data = niche_data %>% filter(growthform == "tree"), 
       aes(x = lat_range_sd_s, 
           y = lat_range_sd_g)) +
  ggtitle("Southern hemisphere tree") +
  geom_point(alpha = 0.3, size = 0.3) +
  scale_fill_manual(values = mycolors) +
  #geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  theme_classic() +
  xlab("Latitudinal range in the southern hemisphere (SD)") +
  ylab("Global latitudinal range (SD)")

D2 <- ggplot(data = niche_data %>% filter(growthform == "tree"), 
       aes(x = lat_range_sd_n, 
           y = lat_range_sd_g)) +
  ggtitle("Northern hemisphere tree") +
  geom_point(alpha = 0.3, size = 0.3) +
  scale_fill_manual(values = mycolors) +
  #geom_density_2d_filled(bins = 20, show.legend = FALSE) +
  theme_classic() +
  xlab("Latitudinal range in the southern hemisphere (SD)") +
  ylab("Global latitudinal range (SD)")

cowplot::plot_grid(A2, B2,
                   C2, D2,
                   rel_widths = c(1, 1,
                                  1, 1),
                   ncol = 2, align = "h",
                   labels = c("A", "B", 
                              "C", "D"),
                   label_size = 25)

ggsave("./../tmp/sf?.jpg",
       width = 3000, height = 2000, units = "px")

