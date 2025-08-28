# this document runs and saves the sims that are used for generating simulations for - all athletes, specifically for EATON and WILLIAMS. Date: August 28, 2025. Some code is copied from make_age_curves_graphs_paper.R. this document is only for the simulation of data. for analyses, see make_age_curves_graphs_paper, find_maxima_ind, and look_at_posterior_correlation.
set.seed(2)
library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(mvtnorm)
library(knitr)


study = "new_stan_sim"
data_dir = "new_stan_sim/data/"
script_dir = "new_stan_sim/study/"
stan_dir = "new_stan_sim/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))

online_data_filter <- read_csv(paste0(data_dir,"online_data_filter.csv"))
event_sums <- get_event_sums_df(online_data_filter)
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
event_sums <- get_event_sums_df(online_data_filter)
dec_data_standard <- standardize_decathlon_data(online_data_filter,
                                                event_sums)




# Eaton and Williams ------------------------------------------------------
# used in the maximal age calculation and age curves for specific athletes. 
train_df <- dec_data_standard %>%
  filter((subj_id!=239) |(age < 25),
         (subj_id!=759) | (age < 25))
test_df <- online_data_filter %>%
  filter((subj_id==239) & (age >= 25) |
           (subj_id==759) & (age >= 25))


full_test_df <- online_data_filter %>%
  filter((subj_id == 239)|(subj_id == 759)) %>%
  mutate(status = case_when(((subj_id==239) & (age < 25) )|((subj_id==759) & (age < 25)) ~ "train",
                            ((subj_id==239) & (age >= 25) )|((subj_id==759) & (age >= 25)) ~ "test"))


age_vec <- c(seq(19, 30, by = .1),
             seq(19, 30, by = .1))
athlete_id <- c(rep(91, length(seq(19, 30, by = .1))),
                rep(310, length(seq(19, 30, by = .1))))
is_new_athlete <- rep(0, length(age_vec))

baseline_sim <- get_baseline_cubic_sim(age_vec = age_vec,
                                       athlete_id = athlete_id,
                                       is_new_athlete = is_new_athlete,
                                       decathlon_data = train_df,
                                       stan_dir = stan_dir,
                                       iter = 2000,
                                       return_all = T)


simple_sim <- get_simple_cubic_sim(age_vec = age_vec,
                                   athlete_id = athlete_id,
                                   is_new_athlete = is_new_athlete,
                                   decathlon_data = train_df,
                                   event_sums = event_sums,
                                   stan_dir = stan_dir,
                                   iter = 2000,
                                   return_all = T)

comp_sim <- get_comp_cubic_sim(age_vec = age_vec,
                               athlete_id = athlete_id,
                               is_new_athlete = is_new_athlete,
                               decathlon_data = train_df,
                               event_sums = event_sums,
                               stan_dir = stan_dir,
                               iter = 2000,
                               return_all = T)
saveRDS(object = baseline_sim, file = "results/baseline_sim_for_select_athletes.RData")
saveRDS(object = simple_sim, file = "results/simple_sim_for_select_athletes.RData")
saveRDS(object = comp_sim, file = "results/comp_sim_for_select_athletes.RData")


# "General" Athlete -------------------------------------------------------
# used in looking at the posterior predictive correlation

# use same age vec and athlete ids as data.
age_vec <- online_data_filter$age
athlete_id <- online_data_filter$athlete_id
is_new_athlete <- rep(0, length(age_vec))



simple_sim_gen <- get_simple_cubic_sim(age_vec = age_vec,
                                   athlete_id = athlete_id,
                                   is_new_athlete = is_new_athlete,
                                   decathlon_data = dec_data_standard,
                                   event_sums = event_sums,
                                   stan_dir = stan_dir,
                                   iter = 1000,
                                   return_all = F)

comp_sim_gen <- get_comp_cubic_sim(age_vec = age_vec,
                               athlete_id = athlete_id,
                               is_new_athlete = is_new_athlete,
                               decathlon_data = dec_data_standard,
                               event_sums = event_sums,
                               stan_dir = stan_dir,
                               iter = 1000,
                               return_all = F)

saveRDS(object = simple_sim_gen, file = "results/simple_sim_for_all_athletes.RData")
saveRDS(object = comp_sim_gen, file = "results/comp_sim_for_all_athletes.RData")


