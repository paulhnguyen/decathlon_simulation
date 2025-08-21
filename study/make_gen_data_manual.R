# Aug 21, 2025
# code to make the simulated data set and save true event coefficients in the simulated parameter recovery. will produce and save 1 dataset with explicitly determined beta coefficients for use in the study_gen_data_2.R file

set.seed(2)
library(tidyverse)
library(readxl)
library(tidyselect)
library(patchwork)
library(forcats)
library(mvtnorm)
library(lme4)

study = "decathlon_simulation"
data_dir = "decathlon_simulation/data/"
script_dir = "decathlon_simulation/study/"
stan_dir = "decathlon_simulation/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")

online_data_filter <- read_csv(paste0(data_dir,"online_data_filter.csv")) 
event_sums <- get_event_sums_df(online_data_filter)
mean_age <- mean(online_data_filter$age)
sd_age <- sd(online_data_filter$age)
dec_data_standard <- standardize_decathlon_data(online_data_filter, event_sums = event_sums) %>%
  mutate(age = (age-mean_age) / sd_age)


beta_coef_list <- vector("list", 10)
names(beta_coef_list) <- dec_events
mean_mat <- data.frame(matrix(nrow = length(dec_data_standard$age),
                              ncol = 11))
colnames(mean_mat) <- c(dec_events, "age")
mean_mat$age <- dec_data_standard$age
sim_df_list <- vector("list", 200)

for(p in 1:200){
  print(p)
  sim_df <- data.frame(age = dec_data_standard$age,
                       athlete_id = dec_data_standard$athlete_id)
  set.seed(p)
  for (i in 1:10) {
    event = dec_events[i]
    if (i >= 2) {
      prev_events <- dec_events[1:(i-1)]
      form <- as.formula(paste0(event, "~  (1|athlete_id) + age + I(age^2) +I(age^3) + ", paste0(prev_events, collapse = " + ")))
    } else{
      form <- as.formula(paste0(event, "~  (1|athlete_id) + age + I(age^2) +I(age^3)"))
    }
    
    lm_mod <- lmer(form, data = dec_data_standard)
    beta_coef_list[[i]] <- fixef(lm_mod)
    names(beta_coef_list[[i]]) <- names(coef((lm_mod))$athlete_id)
    sim_df[,event] <- simulate(lm_mod, 
                               newdata = sim_df)
  }
  sim_df[,'age'] <- (sim_df[,'age'] *sd_age) + mean_age
  sim_df_list[[p]] <- sim_df
}



# saveRDS(sim_df_list, file = "decathlon_simulation/data/sim_data_list_manual.RData")
# saveRDS(beta_coef_list, file = "decathlon_simulation/data/beta_list_sim_manual.RData")
