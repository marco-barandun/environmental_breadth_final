library(tidyverse)
library(boot)
library(readr)

setwd("/Users/marco/GitHub/environmental_breadth_final/")

# Read and preprocess the data
dt <- read_csv("./3_generated_data/niche_data_final_summarized_v5_withMAD_min5_sign.csv") %>%
  mutate(e_breadth = (env_breadth * mess) ^ (1 / 4))
colnames(dt)

# Function for filtering data based on hemisphere, zone, and growth form
filter_data <- function(data, type, hemisphere, zone, form) {
  if (type == "Global") {
    if (hemisphere == "North") {
      filtered_data <- data %>% 
        filter(lat_median_g > 0) %>%
        mutate(zone = ifelse(abs(lat_median_g) > 0, "tropical", NA)) %>%
        mutate(zone = ifelse(abs(lat_median_g) > 10, "else", zone)) %>%
        filter(zone == !!zone, growthform == !!form) %>% 
        select(lat_range_mad_g, e_breadth, lat_median_g)
    } else if (hemisphere == "South") {
      filtered_data <- data %>% 
        filter(lat_median_g < 0) %>%
        mutate(zone = ifelse(lat_median_g < 0, "tropical", NA)) %>%
        mutate(zone = ifelse(lat_median_g < -10, "else", zone)) %>%
        mutate(lat_median_g = abs(lat_median_g)) %>%
        filter(zone == !!zone, growthform == !!form) %>% 
        select(lat_range_mad_g, e_breadth, lat_median_g)
    }
  } else if (type == "Hemisphere Specific") {
    if (hemisphere == "North") {
      filtered_data <- data %>% 
        filter(lat_median_n > 0) %>%
        mutate(zone = ifelse(abs(lat_median_n) > 0, "tropical", NA)) %>%
        mutate(zone = ifelse(abs(lat_median_n) > 10, "else", zone)) %>%
        filter(zone == !!zone, growthform == !!form) %>% 
        select(lat_range_mad_n, e_breadth, lat_median_n)
    } else if (hemisphere == "South") {
      filtered_data <- data %>% 
        filter(lat_median_s > 0) %>%
        mutate(zone = ifelse(abs(lat_median_s) >= 0, "tropical", NA)) %>%
        mutate(zone = ifelse(abs(lat_median_s) > 10, "else", zone)) %>%
        filter(zone == !!zone, growthform == !!form) %>% 
        select(lat_range_mad_s, e_breadth, lat_median_s)
    }
  }
  return(filtered_data)
}

# Bootstrapping function
boot_function <- function(data, indices, formula) {
  boot_sample <- data[indices, ]
  model <- lm(formula, data = boot_sample)
  coef(model)[2]
}

# Generate combinations
combs <- expand.grid(type = c("Global", "Hemisphere Specific"),
                     zone = c("tropical", "else"),
                     hemi = c("North", "South"),
                     form = c("tree", "herb"))

allres_lmlr <- tibble()
allres_eblr <- tibble()
allres_lmeb <- tibble()

# Loop through combinations for all analyses
for(i in 1:nrow(combs)){
  # Filter data based on current combination
  tmpdat <- filter_data(dt, combs$type[i], combs$hemi[i], combs$zone[i], combs$form[i])
  
  if(nrow(tmpdat) > 0){
    # Analysis 1: Latitudinal median vs. latitudinal range
    formula_lmlr <- as.formula(paste(colnames(tmpdat)[1], "~", colnames(tmpdat)[3]))
    lmlr <- lm(formula_lmlr, data = tmpdat)
    boot_results_lmlr <- boot(tmpdat, boot_function, R = 1000, formula = formula_lmlr)
    ci_lmlr <- boot.ci(boot_results_lmlr, type = "perc", conf = 0.90)
    boot_ci_lmlr <- ci_lmlr$percent[4:5]
    
    # Analysis 2: Environmental breadth vs. latitudinal range
    formula_eblr <- as.formula(paste(colnames(tmpdat)[1], "~", colnames(tmpdat)[2]))
    eblr <- lm(formula_eblr, data = tmpdat)
    boot_results_eblr <- boot(tmpdat, boot_function, R = 1000, formula = formula_eblr)
    ci_eblr <- boot.ci(boot_results_eblr, type = "perc", conf = 0.90)
    boot_ci_eblr <- ci_eblr$percent[4:5]
    
    # Analysis 3: Latitudinal median vs. environmental breadth
    formula_lmeb <- as.formula(paste(colnames(tmpdat)[2], "~", colnames(tmpdat)[3]))
    lmeb <- lm(formula_lmeb, data = tmpdat)
    boot_results_lmeb <- boot(tmpdat, boot_function, R = 1000, formula = formula_lmeb)
    ci_lmeb <- boot.ci(boot_results_lmeb, type = "perc", conf = 0.90)
    boot_ci_lmeb <- ci_lmeb$percent[4:5]
    
    # Creating the tables for each analysis
    tmpres_lmlr <- tibble(
      model = "lat median vs lat range", 
      type = combs$type[i],
      zone = combs$zone[i], 
      hemisphere = combs$hemi[i],
      growthform = combs$form[i],
      valci = paste0(round(mean(boot_results_lmlr$t), 3), " (", round(boot_ci_lmlr[1], 3), ", ", round(boot_ci_lmlr[2], 3), ")"),
      valse = paste0(round(coef(lmlr)[2], 3), " ± ", round(sqrt(diag(vcov(lmlr)))[2], 3)),
      lower_bci = boot_ci_lmlr[1],
      upper_bci = boot_ci_lmlr[2],
      mean_b = mean(boot_results_lmlr$t)
    )
    
    tmpres_eblr <- tibble(
      model = "env.breadth vs lat range", 
      type = combs$type[i],
      zone = combs$zone[i],
      hemisphere = combs$hemi[i],
      growthform = combs$form[i],
      valci = paste0(round(mean(boot_results_eblr$t), 3), " (", round(boot_ci_eblr[1], 3), ", ", round(boot_ci_eblr[2], 3), ")"),
      valse = paste0(round(coef(eblr)[2], 3), " ± ", round(sqrt(diag(vcov(eblr)))[2], 3)),
      lower_bci = boot_ci_eblr[1],
      upper_bci = boot_ci_eblr[2],
      mean_b = mean(boot_results_eblr$t)
    )
    
    tmpres_lmeb <- tibble(
      model = "lat median vs env.breadth", 
      type = combs$type[i],
      zone = combs$zone[i],
      hemisphere = combs$hemi[i],
      growthform = combs$form[i],
      valci = paste0(round(mean(boot_results_lmeb$t), 3), " (", round(boot_ci_lmeb[1], 3), ", ", round(boot_ci_lmeb[2], 3), ")"),
      valse = paste0(round(coef(lmeb)[2], 3), " ± ", round(sqrt(diag(vcov(lmeb)))[2], 3)),
      lower_bci = boot_ci_lmeb[1],
      upper_bci = boot_ci_lmeb[2],
      mean_b = mean(boot_results_lmeb$t)
    )
    
    allres_lmlr <- allres_lmlr %>% bind_rows(tmpres_lmlr)
    allres_eblr <- allres_eblr %>% bind_rows(tmpres_eblr)
    allres_lmeb <- allres_lmeb %>% bind_rows(tmpres_lmeb)
    
  }
}

#write_csv(allres_lmlr, "./3_generated_data/coefficients/allres_lmlr.csv")
#write_csv(allres_eblr, "./3_generated_data/coefficients/allres_eblr.csv")
#write_csv(allres_lmeb, "./3_generated_data/coefficients/allres_lmeb.csv")

# Output the results
allres_lmlr %>% spread(zone, valse) %>% arrange(model, growthform, hemisphere)
allres_eblr %>% spread(zone, valse) %>% arrange(model, growthform, hemisphere)
allres_lmeb %>% spread(zone, valse) %>% arrange(model, growthform, hemisphere)

# Define the plotting function
plot_analysis_results <- function(data, title) {
  data %>%
    arrange(type, growthform, hemisphere, zone) %>%
    mutate(combined_factor = paste(growthform, hemisphere, zone, sep = " - "),
           combined_factor = factor(combined_factor, levels = c("tree - North - else", 
                                                                "tree - North - tropical", 
                                                                "tree - South - tropical", 
                                                                "tree - South - else",
                                                                "herb - North - else", 
                                                                "herb - North - tropical", 
                                                                "herb - South - tropical", 
                                                                "herb - South - else"))) %>%
    ggplot(aes(x = mean_b, y = combined_factor)) +
    geom_point() +
    geom_errorbarh(aes(xmin = lower_bci, xmax = upper_bci), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_hline(yintercept = which(levels(data$combined_factor) == "tree - else - South") - 0.5, linetype = "solid", color = "black") +
    facet_grid(growthform ~ ., scales = "free", space = "free") +
    labs(title = title,
         x = "Bootstrapped Slope",
         y = "") +
    theme_classic() +
    theme(text = element_text(size = 17))

  ggsave(paste0("./figures/_sf_", title, ".jpg"),
         width = 4000, height = 3000, units = "px")
}

# Use the function for each analysis
plot_analysis_results(allres_lmlr %>% filter(type == "Global"), "Latitudinal Median vs. Latitudinal Range")
plot_analysis_results(allres_eblr %>% filter(type == "Global"), "Environmental Breadth vs. Latitudinal Range")
plot_analysis_results(allres_lmeb %>% filter(type == "Global"), "Latitudinal Median vs. Environmental Breadth")





