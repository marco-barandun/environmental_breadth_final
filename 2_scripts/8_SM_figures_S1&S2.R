library(tidyverse)

setwd("/Users/marco/GitHub/environmental_breadth_final/3_generated_data")

niche_data <- read_csv("./../3_generated_data/niche_data_final_summarized_v4.csv") %>%
  mutate(e_breadth = (env_breadth*mess)^(1/4))

### Figure S1A & B
herbs <- read_csv("/Users/marco/GitHub/environmental_breadth_final/3_generated_data/ALLherbs_medianLat.csv") %>% mutate(growthform = "herb")
df_trees <- read_csv("/Users/marco/GitHub/environmental_breadth_final/1_original_data/ALL_Species_cleaned_occtest_ocean.csv") %>%
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
  xlab('Global median latitude') +
  ylab('Number of species') +
  ylim(0, 20000) +
  scale_x_continuous(breaks=c(-75, -50, -25, 0, 25, 50, 75), limits = c(-80, 80)) +
  theme_classic() +
  theme(text = element_text(size = 25),
        legend.position="none")

nr_lat_median_herbs <- ggplot(occs %>% filter(growthform == "herb"), aes(lat_median, fill = inc)) +
  geom_histogram() +
  xlab('Global median latitude') +
  ylab('Number of species') +
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

#ggsave("./../tmp/S1_sp_lat.jpeg",
#       width = 3840*1.5, height = 2160*1.5, units = "px")

### Figure S2A & B
(auc_plot <- ggplot(niche_data, aes(auc.val.avg, fill = growthform)) +
    geom_histogram() +
    xlab('Average AUC value') +
    ylab('Number of included species') +
    theme_classic() +
    theme(text = element_text(size = 25),
          legend.position="none") +
    scale_fill_grey(start = 0.35, 
                    end = .9,
                    name = "Growthform")
)

(or_plot <- ggplot(niche_data, aes(or.10p.avg, fill = growthform)) +
    geom_histogram() +
    xlab('Average 10% omission rate') +
    ylab('Number of included species') +
    theme_classic() +
    theme(text = element_text(size = 25)) +
    scale_fill_grey(start = 0.35, 
                    end = .9,
                    name = "Growthform")
)

auc_or_plots <- cowplot::plot_grid(auc_plot, 
                                   or_plot + theme(legend.position="none"),
                                   labels = c('A', 'B'), label_size = 25)

legend_auc_or <- get_legend(
  # create some space to the left of the legend
  or_plot + theme(legend.box.margin = margin(0, 0, 0, 12)))

(cowplot::plot_grid(auc_or_plots, legend_auc_or, rel_widths = c(3, .4)))


ggsave("./../tmp/S1_AUC_OR.jpeg",
       width = 3840*1.5, height = 2160*1.5, units = "px")

