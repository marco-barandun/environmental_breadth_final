### Analysis of the tree species for publication

library("tidyverse")
library("data.table")
library("dplyr")
library("ggpubr")
library("viridis")
library("plotly")
library("gridExtra")
library("data.table")

# --------------------- Notes
# To find a good transformation plot the hist of the envs and look hist((var)^1/2) --> sqrt seems the best
# ---------------------

base::setwd("."); getwd()
dir_archive <- "/Users/marco/Documents/env_breadth_archive/"

##### Reading in the first line of all the species *_maxent_results.csv file (the line with the niche breadth estimate)
##### using the data.table package with fread as performance is much much faster than other solutions
##### https://statisticsglobe.com/merge-csv-files-in-r
raw_niche_data_trees <- list.files(path = paste(dir_archive, "res_maxent_result_tables_trees/", sep = ""), 
                                   pattern = "*_maxent_results.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  mutate(growthform = "tree")

raw_niche_data_herbs <- list.files(path = paste(dir_archive, "res_maxent_result_tables_herbs/", sep = ""), 
                                   pattern = "*_maxent_results.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  mutate(growthform = "herb")

# -------------------------------------------------------------------------------------------------------
##### Reading in the MESS weight for the niche breadth and add it to the raw niche data
mess_trees <- list.files(path = paste(dir_archive, "res_representativeness_trees/", sep = ""), 
                                 pattern = "*_representativeness.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  rename(Species = species) %>%
  rename(mess = value) %>%
  data.table::as.data.table(.)

raw_niche_data_trees_mess <- raw_niche_data_trees %>%
  data.table::as.data.table(.) %>%
  .[mess_trees, nomatch=0, on = "Species"]
  
mess_herbs <- list.files(path = paste(dir_archive, "res_representativeness_herbs/", sep = ""), 
                                 pattern = "*_representativeness.csv", full.names = TRUE) %>%
  map_df(~fread(., nrows = 1)) %>%
  rename(Species = species) %>%
  rename(mess = value)

raw_niche_data_herbs_mess <- raw_niche_data_herbs %>%
  data.table::as.data.table(.) %>%
  .[mess_herbs, nomatch=0, on = "Species"]

# -------------------------------------------------------------------------------------------------------
##### Generate the colums for the additional variables which will be computed below
raw_niche_data_mess <- rbind(raw_niche_data_trees_mess, raw_niche_data_herbs_mess)

write_csv(raw_niche_data_mess, "./../3_generated_data/raw_niche_data_mess.csv")
rm(list=setdiff(ls(), c("dir_archive", "raw_niche_data_mess")))

niche_data <- raw_niche_data_mess %>%
  mutate(lat_range_mad_s = as.numeric(NA)) %>%
  mutate(lat_range_mad_n = as.numeric(NA)) %>%
  mutate(lat_range_mad_g = as.numeric(NA)) %>%
  mutate(lat_range_sd_s = as.numeric(NA)) %>%
  mutate(lat_range_sd_n = as.numeric(NA)) %>%
  mutate(lat_range_sd_g = as.numeric(NA)) %>%
  mutate(lat_median_s = as.numeric(NA)) %>%
  mutate(lat_median_n = as.numeric(NA)) %>%
  mutate(lat_median_g = as.numeric(NA)) %>%
  mutate(hemisphere = as.numeric(NA)) %>%
  mutate(lat_n95q = as.numeric(NA)) %>%
  mutate(lat_s05q = as.numeric(NA))

# -------------------------------------------------------------------------------------------------------
##### Reading in the species occurrences
occurrences_trees <- read_csv(paste("./../1_original_data/tree_occurrences_Andrea_clean_withElevation.csv", sep = "")) %>%
  filter(Species %in% raw_niche_data_mess$Species) %>%
  mutate(hemisphere = sign(latitude)) %>%
  filter(hemisphere != 0) %>%
  dplyr::select(-...1) %>%
  mutate(growthform = "tree")

occurrences_herbs <- read_csv(paste("./../1_original_data/herb_occs_elevation-105450sp.csv", sep = "")) %>%
  filter(Species %in% raw_niche_data_mess$Species) %>%
  mutate(hemisphere = sign(latitude)) %>%
  filter(hemisphere != 0) %>%
  mutate(growthform = "herb") %>%
  rename(Source = source) %>%
  select(occID, Species, longitude, latitude, Source, elevation, hemisphere, growthform)

# -------------------------------------------------------------------------------------------------------
##### Separating the occurrences based on growthform and hemisphere
southern_herbs <- occurrences_herbs %>%
  filter(hemisphere == -1) %>%
  filter(Species %in% raw_niche_data_mess$Species)
northern_herbs <- occurrences_herbs %>%
  filter(hemisphere == 1) %>%
  filter(Species %in% raw_niche_data_mess$Species)

southern_trees <- occurrences_trees %>%
  filter(hemisphere == -1) %>%
  filter(Species %in% raw_niche_data_mess$Species)
northern_trees <- occurrences_trees %>%
  filter(hemisphere == 1) %>%
  filter(Species %in% raw_niche_data_mess$Species)

# -------------------------------------------------------------------------------------------------------
##### Calculating latitudinal ranges per species per hemisphere; I know, it is not elegant, but strangely this is the only way it works
# Southern trees
niche_data[!is.na(match(niche_data$Species, unique(southern_trees$Species))),]$lat_range_mad_s <- tapply(southern_trees$latitude, southern_trees$Species, function(x) abs(round(mad(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(southern_trees$Species))),]$lat_range_sd_s <- tapply(southern_trees$latitude, southern_trees$Species, function(x) abs(round(sd(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(southern_trees$Species))),]$lat_median_s <- tapply(southern_trees$latitude, southern_trees$Species, function(x) abs(round(median(x, na.rm = TRUE), digits = 3)))
# Northern trees
niche_data[!is.na(match(niche_data$Species, unique(northern_trees$Species))),]$lat_range_mad_n <- tapply(northern_trees$latitude, northern_trees$Species, function(x) abs(round(mad(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(northern_trees$Species))),]$lat_range_sd_n <- tapply(northern_trees$latitude, northern_trees$Species, function(x) abs(round(sd(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(northern_trees$Species))),]$lat_median_n <- tapply(northern_trees$latitude, northern_trees$Species, function(x) abs(round(median(x, na.rm = TRUE), digits = 3)))
# Global trees
niche_data[!is.na(match(niche_data$Species, unique(occurrences_trees$Species))),]$lat_range_mad_g <- tapply(occurrences_trees$latitude, occurrences_trees$Species, function(x) abs(round(mad(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_trees$Species))),]$lat_range_sd_g <- tapply(occurrences_trees$latitude, occurrences_trees$Species, function(x) abs(round(sd(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_trees$Species))),]$lat_median_g <- tapply(occurrences_trees$latitude, occurrences_trees$Species, function(x) round(median(x, na.rm = TRUE), digits = 3))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_trees$Species))),]$hemisphere <- tapply(occurrences_trees$latitude, occurrences_trees$Species, function(x) sign(median(x, na.rm = TRUE)))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_trees$Species))),]$lat_n95q <- tapply(occurrences_trees$latitude, occurrences_trees$Species, function(x) round(quantile(x, probs = .95, na.rm = TRUE), digits = 3))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_trees$Species))),]$lat_s05q <- tapply(occurrences_trees$latitude, occurrences_trees$Species, function(x) round(quantile(x, probs = .05, na.rm = TRUE), digits = 3))

# Southern herbs
niche_data[!is.na(match(niche_data$Species, unique(southern_herbs$Species))),]$lat_range_mad_s <- tapply(southern_herbs$latitude, southern_herbs$Species, function(x) abs(round(mad(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(southern_herbs$Species))),]$lat_range_sd_s <- tapply(southern_herbs$latitude, southern_herbs$Species, function(x) abs(round(sd(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(southern_herbs$Species))),]$lat_median_s <- tapply(southern_herbs$latitude, southern_herbs$Species, function(x) abs(round(median(x, na.rm = TRUE), digits = 3)))
# Northern herbs
niche_data[!is.na(match(niche_data$Species, unique(northern_herbs$Species))),]$lat_range_mad_n <- tapply(northern_herbs$latitude, northern_herbs$Species, function(x) abs(round(mad(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(northern_herbs$Species))),]$lat_range_sd_n <- tapply(northern_herbs$latitude, northern_herbs$Species, function(x) abs(round(sd(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(northern_herbs$Species))),]$lat_median_n <- tapply(northern_herbs$latitude, northern_herbs$Species, function(x) round(median(abs(x), na.rm = TRUE), digits = 3))
# Global herbs
niche_data[!is.na(match(niche_data$Species, unique(occurrences_herbs$Species))),]$lat_range_mad_g <- tapply(occurrences_herbs$latitude, occurrences_herbs$Species, function(x) abs(round(mad(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_herbs$Species))),]$lat_range_sd_g <- tapply(occurrences_herbs$latitude, occurrences_herbs$Species, function(x) abs(round(sd(x, na.rm = TRUE), digits = 3)))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_herbs$Species))),]$lat_median_g <- tapply(occurrences_herbs$latitude, occurrences_herbs$Species, function(x) round(median(x, na.rm = TRUE), digits = 3))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_herbs$Species))),]$hemisphere <- tapply(occurrences_herbs$latitude, occurrences_herbs$Species, function(x) sign(median(x, na.rm = TRUE)))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_herbs$Species))),]$lat_n95q <- tapply(occurrences_herbs$latitude, occurrences_herbs$Species, function(x) round(quantile(x, probs = .95, na.rm = TRUE), digits = 3))
niche_data[!is.na(match(niche_data$Species, unique(occurrences_herbs$Species))),]$lat_s05q <- tapply(occurrences_herbs$latitude, occurrences_herbs$Species, function(x) round(quantile(x, probs = .05, na.rm = TRUE), digits = 3))

# Adding global parameters
niche_data <- niche_data %>%
  mutate(lat_range_mad_max = pmax(lat_range_mad_s, lat_range_mad_n, na.rm = TRUE)) %>%
  mutate(lat_range_sd_max = pmax(lat_range_sd_s, lat_range_sd_n, na.rm = TRUE)) %>%
  mutate(lat_median_ave = rowMeans(niche_data[,c("lat_median_s", "lat_median_n")], na.rm = TRUE)*niche_data$hemisphere)

# -------------------------------------------------------------------------------------------------------
# Check if the number of NAs is reasonable. Most columns should not have any NAs, elevation might have some
as.data.frame(niche_data) %>% summarise_all(~ sum(is.na(.)))

### Filter
niche_data <- niche_data %>%
  filter(auc.val.avg > 0.55) %>%
  filter(0.05 < or.10p.avg) %>%
  filter(or.10p.avg < 0.3)

# Export
write_csv(niche_data, "./../3_generated_data/niche_data_final.csv")
rm(list=setdiff(ls(), c("dir_archive", "niche_data")))
