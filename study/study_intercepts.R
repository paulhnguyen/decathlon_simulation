# September 25, 20205
# Code to run models to get decathlon simulation, with a focus on the random intercepts for each athlete. 
# dependencies: tidyverse, readxl, rstan, rlist, tidyselect, patchwork, forcats
set.seed(2)
library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(mvtnorm)


# setwd("~/school/wisconsin/research_repo/decathlon")
study = "new_stan_sim"
data_dir = "new_stan_sim/data/"
script_dir = "new_stan_sim/study/"
stan_dir = "new_stan_sim/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))

dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")

online_data_filter <- read_csv(paste0(data_dir,"online_data_filter.csv")) %>%
  filter(year(dob) > 1950) %>%
  group_by(name, dob) %>%
  mutate(athlete_id = cur_group_id()) %>%
  unique() %>%
  filter(discus > 2) %>%
  unique()
event_sums <- get_event_sums_df(online_data_filter)
dec_data_standard <- standardize_decathlon_data(online_data_filter,
                                                event_sums)

# we do not need the simulated events, can get those using betas later. focus on athlete intercepts.
athlete_ids <- unique(dec_data_standard$athlete_id)
athlete_ages <- rep(20, length(athlete_ids))
is_new_athlete <- rep(0, length(athlete_ids))

sim <- get_comp_cubic_sim_athlete_intercepts(age_vec = athlete_ages,
                                            athlete_id = athlete_ids,
                                            is_new_athlete = is_new_athlete,
                                            decathlon_data = dec_data_standard,
                                            stan_dir = stan_dir,
                                            iter = 2000)
saveRDS(object = sim, file = "intercepts_sim.RData")

