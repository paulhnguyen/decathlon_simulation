# This file assumes you have a results directory, containing the results from from running the coverage study.
results_dir = "../results/coverage_results/"
data_dir = "data/"

library(tidyverse)
library(knitr)
library(ggplot2)
source("study/decathlon_funs.R")
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
# load true coefficients
sim_beta_coef_list <- readRDS(paste0(data_dir, "beta_list_sim_manual.RData"))

### load results ###

# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(results_dir, list.files(results_dir), sep = "")
results <- read_csv(files[1]) 
for (i in 2:length(files)) {
  results <- results %>%
    rbind(read_csv(files[i]))
}


# add true coefficients to results
for (i in 1:nrow(results)) {
  target_id <- results$target[i]
  predictor_id <- results$predictor[i]
  if (predictor_id == "age2") {
    predictor_id = "I(age^2)"
  } else if (predictor_id == "age3"){
    predictor_id = "I(age^3)"
  }
  beta_coef <- sim_beta_coef_list[[target_id]][predictor_id]
  results$true_coef[i] <- beta_coef
}
results <- results %>%
  mutate(cov_check = (true_coef < ub) & (true_coef > lb))


comp_prop <- results %>%
  filter(comp == "compositional",
         type != "spline") %>%
  group_by(target, predictor, type, comp) %>%
  summarize(prop = sum(cov_check) / n()) %>%
  drop_na()



comp_prop_wide <- comp_prop %>%
  ungroup() %>%
  select(-type, -comp) %>%
  pivot_wider(names_from = target,
              values_from = prop) %>%
  arrange(factor(predictor, levels = c('age', 'age2', 'age3', dec_events[1:9]))) %>%
  select(all_of(c('predictor', dec_events)))
simple_prop_wide <- simple_prop %>%
  ungroup() %>%
  select(-type, -comp) %>%
  pivot_wider(names_from = target,
              values_from = prop) %>%
  arrange(factor(predictor, levels = c('age', 'age2', 'age3', dec_events[1:9]))) %>%
  select(all_of(c('predictor', dec_events)))
  

# producing table in paper
comp_prop_wide %>%
  kable(caption = "Proportion of 95% uncertainty intervals containing the true parameter associated with corresponding predictor. NA's have been replaced with '_'",
        escape = F,
        label = "athlete_table",
        digits = 2,
        format = "latex",
        booktabs = T) # Output format = latex 

