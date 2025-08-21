# April 15, 2025
# Code to run models to get decathlon bakeoff between models
# dependencies: tidyverse, readxl, rstan, rlist, tidyselect, patchwork, forcats
set.seed(2)

library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(splines)
# setwd("~/school/wisconsin/research_repo/decathlon")
study = "new_stan_sim"
data_dir = "new_stan_sim/data/"
script_dir = "new_stan_sim/study/"
stan_dir = "new_stan_sim/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))
source(paste0(script_dir, "settings_bakeoff.R"))
load(paste0(data_dir, "test_split_list_athlete.RData"))
load(paste0(data_dir, "test_split_list_future.RData"))
load(paste0(data_dir, "test_split_list_general.RData"))
### simulation settings ###
args = commandArgs(TRUE)
job_id = as.numeric(args[1]) + 1

type = as.character(settings$type[job_id])
comp = as.character(settings$comp[job_id])
prior = as.character(settings$prior[job_id])
pred_type = as.character(settings$pred_type[job_id])
iter = as.integer(settings$iter[job_id])
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
print(c(type, comp, prior, pred_type, iter))

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

if (pred_type == "general") {
  test_index = test_split_list[[iter]]
} else if (pred_type == "athlete") {
  test_index = test_split_list_athlete[[iter]]
} else if (pred_type == "future") {
  test_index = test_split_list_future[[iter]]
}

train_df <- dec_data_standard[-test_index,]
test_df <- dec_data_standard[test_index,]

athlete_id <- test_df$athlete_id
is_new_athlete <- as.integer(!(test_df$athlete_id %in% train_df$athlete_id))
print(sum(is_new_athlete))
age_vec <- test_df$age


if (type == "cubic") {
  if (prior == "none"){
    if (comp == "baseline"){
      sim <- get_baseline_cubic_sim(age_vec = age_vec,
                                    athlete_id = athlete_id,
                                    is_new_athlete = is_new_athlete,
                                    decathlon_data = train_df,
                                    stan_dir = stan_dir,
                                    iter = 2000,
                                    return_all = F)
    } else if (comp == "simple"){
      sim <- get_simple_cubic_sim(age_vec = age_vec,
                                  athlete_id = athlete_id,
                                  is_new_athlete = is_new_athlete,
                                  decathlon_data = train_df,
                                  event_sums = event_sums,
                                  stan_dir = stan_dir,
                                  iter = 2000,
                                  return_all = F)
    } else if (comp == "compositional"){
      sim <- get_comp_cubic_sim(age_vec = age_vec,
                                athlete_id = athlete_id,
                                is_new_athlete = is_new_athlete,
                                decathlon_data = train_df,
                                event_sums = event_sums,
                                stan_dir = stan_dir,
                                iter = 2000,
                                return_all = F)
    }
  } else if (prior == "joint"){
    if (comp == "simple"){
      sim <- get_simple_joint_cubic_sim(age_vec = age_vec,
                                        athlete_id = athlete_id,
                                        is_new_athlete = is_new_athlete,
                                        decathlon_data = train_df,
                                        event_sums = event_sums,
                                        stan_dir = stan_dir,
                                        ind_data_dir = data_dir,
                                        iter = 2000,
                                        return_all = F)
    } else if (comp == "compositional"){
      sim <- get_comp_joint_cubic_sim(age_vec = age_vec,
                                      athlete_id = athlete_id,
                                      is_new_athlete = is_new_athlete,
                                      decathlon_data = train_df,
                                      event_sums = event_sums,
                                      stan_dir = stan_dir,
                                      ind_data_dir = data_dir,
                                      iter = 2000,
                                      return_all = F)
    }
  } else if (prior == "moment"){
    if (comp == "simple"){
      sim <- get_simple_moment_cubic_sim(age_vec = age_vec,
                                         athlete_id = athlete_id,
                                         is_new_athlete = is_new_athlete,
                                         decathlon_data = train_df,
                                         event_sums = event_sums,
                                         stan_dir = stan_dir,
                                         ind_data_dir = data_dir,
                                         iter = 2000,
                                         return_all = F)
    } else if (comp == "compositional"){
      sim <- get_comp_moment_cubic_sim(age_vec = age_vec,
                                       athlete_id = athlete_id,
                                       is_new_athlete = is_new_athlete,
                                       decathlon_data = train_df,
                                       event_sums = event_sums,
                                       stan_dir = stan_dir,
                                       ind_data_dir = data_dir,
                                       iter = 2000,
                                       return_all = F)
    }
  }
  
} else if (type == "spline"){
  if (prior == "none"){
    if (comp == "baseline"){
      sim <- get_baseline_spline_sim(age_vec = age_vec,
                                    athlete_id = athlete_id,
                                    is_new_athlete = is_new_athlete,
                                    decathlon_data = train_df,
                                    stan_dir = stan_dir,
                                    iter = 2000,
                                    return_all = F)
    } else if (comp == "simple"){
      sim <- get_simple_spline_sim(age_vec = age_vec,
                                  athlete_id = athlete_id,
                                  is_new_athlete = is_new_athlete,
                                  decathlon_data = train_df,
                                  event_sums = event_sums,
                                  stan_dir = stan_dir,
                                  iter = 2000,
                                  return_all = F)
    } else if (comp == "compositional"){
      sim <- get_comp_spline_sim(age_vec = age_vec,
                                athlete_id = athlete_id,
                                is_new_athlete = is_new_athlete,
                                decathlon_data = train_df,
                                event_sums = event_sums,
                                stan_dir = stan_dir,
                                iter = 2000,
                                return_all = F)
    }
  }  else if (prior == "moment"){
    if (comp == "simple"){
      sim <- get_simple_moment_spline_sim(age_vec = age_vec,
                                         athlete_id = athlete_id,
                                         is_new_athlete = is_new_athlete,
                                         decathlon_data = train_df,
                                         event_sums = event_sums,
                                         stan_dir = stan_dir,
                                         ind_data_dir = data_dir,
                                         iter = 2000,
                                         return_all = F)
    } else if (comp == "compositional"){
      sim <- get_comp_moment_spline_sim(age_vec = age_vec,
                                       athlete_id = athlete_id,
                                       is_new_athlete = is_new_athlete,
                                       decathlon_data = train_df,
                                       event_sums = event_sums,
                                       stan_dir = stan_dir,
                                       ind_data_dir = data_dir,
                                       iter = 2000,
                                       return_all = F)
    }
  }
}
if (comp == "baseline") {
  pred_df <- data.frame(points = sim$post_pred)
  pred_df$athlete_id <- test_df$athlete_id
  
  mse_table <- data.frame(event = c("points"),
                          mse = NA,
                          denom = NA,
                          smse = NA,
                          iter = iter)
  mse_table$mse = mean((pred_df[["points"]] - test_df[["points"]])^2,
                          na.rm = T)
  mse_table$denom = mean(((mean(train_df[["points"]]) - test_df[["points"]])^2),
                         na.rm = T)
  mse_table$smse = mse_table$mse / mse_table$denom
  mse_table$type = type
  mse_table$iter = iter
  mse_table$prior = prior
  mse_table$comp = comp
  
} else{
  pred_df <- sim$sim_events %>%
    group_by(athlete, age) %>%
    summarize(hundred_m = mean(hundred_m),
              long_jump = mean(long_jump),
              shot_put = mean(shot_put),
              high_jump = mean(high_jump),
              four_hundred_m = mean(four_hundred_m),
              hurdles = mean(hurdles),
              discus = mean(discus),
              pole_vault = mean(pole_vault),
              javelin = mean(javelin),
              fifteen_hundred_m = mean(fifteen_hundred_m),
              points = mean(calc_point, na.rm = T)
    )
  pred_df$athlete_id <- test_df$athlete_id
  
  mse_table <- data.frame(event = c(dec_events, "points"),
                          mse = rep(NA, 11),
                          denom = rep(NA, 11),
                          smse = rep(NA, 11),
                          iter = iter)
  
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    test_df[event] <- (test_df[event] *event_sd ) + event_mean
    train_df[event] <- (train_df[event] *event_sd ) + event_mean
    
  }
  for (i in 1:11) {
    event = mse_table$event[i]
    mse_table$mse[i] = mean((pred_df[[event]] - test_df[[event]])^2,
                            na.rm = T)
    mse_table$denom[i] = mean((mean(train_df[[event]]) - test_df[[event]])^2,
                              na.rm = T)
    mse_table$smse[i] = mse_table$mse[i] / mse_table$denom[i] 
    mse_table$type = type
    mse_table$iter = iter
    mse_table$prior = prior
    mse_table$comp = comp
  }
}


name = paste0(pred_type, "_", comp, "_", prior, "_", 
              type, "_", iter, ".RData")
# saveRDS(object = sim, file = name)
write_csv(mse_table, paste0("mse_table_", pred_type, "_", 
                            comp, "_", prior, "_", type, "_",
                            iter, ".csv"))

