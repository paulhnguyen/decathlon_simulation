# Aug 21, 2025
# Code to run models to get decathlon simulations, with explicitly set beta coefficients from make_gen_data_manual.R. Goal: identify coverage of event coefficients, without a focus on prediction.

set.seed(2)
library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(forcats)


study = "decathlon_simulation"
data_dir = "decathlon_simulation/data/"
script_dir = "decathlon_simulation/study/"
stan_dir = "decathlon_simulation/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))
source(paste0(script_dir, "settings_coverage.R"))
sim_data_list <- readRDS(paste0(data_dir, "sim_data_list_manual.RData"))

### simulation settings ###
args = commandArgs(TRUE)
job_id = as.numeric(args[1]) + 1

type = as.character(settings$type[job_id])
comp = as.character(settings$comp[job_id])
iter = as.integer(settings$iter[job_id])


dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
sim_data <- sim_data_list[[iter]]
event_sums <- get_event_sums_df(sim_data)


# dec_data_standard <- standardize_decathlon_data(sim_data,  event_sums)
dec_data_standard <- sim_data
# no pred
age_vec <- c(20, 20)
print(c(type, comp, iter))
athlete_id <- rep(0, length(age_vec))
is_new_athlete <- rep(1, length(age_vec))

if (type == "cubic") {
   if (comp == "simple"){
      sim <- get_simple_cubic_sim(age_vec = age_vec,
                                  athlete_id = athlete_id,
                                  is_new_athlete = is_new_athlete,
                                  decathlon_data = dec_data_standard,
                                  event_sums = event_sums,
                                  stan_dir = stan_dir,
                                  iter = 2000,
                                  return_all = T)
    } else if (comp == "compositional"){
      sim <- get_comp_cubic_sim(age_vec = age_vec,
                                athlete_id = athlete_id,
                                is_new_athlete = is_new_athlete,
                                decathlon_data = dec_data_standard,
                                event_sums = event_sums,
                                stan_dir = stan_dir,
                                iter = 2000,
                                return_all = T)
    }
} else if (type == "spline"){
    if (comp == "simple"){
      sim <- get_simple_spline_sim(age_vec = age_vec,
                                   athlete_id = athlete_id,
                                   is_new_athlete = is_new_athlete,
                                   decathlon_data = dec_data_standard,
                                   event_sums = event_sums,
                                   stan_dir = stan_dir,
                                   iter = 2000,
                                   return_all = T)
    } else if (comp == "compositional"){
      sim <- get_comp_spline_sim(age_vec = age_vec,
                                athlete_id = athlete_id,
                                is_new_athlete = is_new_athlete,
                                decathlon_data = dec_data_standard,
                                event_sums = event_sums,
                                stan_dir = stan_dir,
                                iter = 2000,
                                return_all = T)
    }
}

# now, calculate coverage
bounds_df <- data.frame()
for (i in 1:length(sim$sims_list)) {
  event = dec_events[i]
  target_sim <- sim$sims_list[[i]]
    if (type == "cubic"){
        q_beta1 <- quantile(target_sim$beta1, probs = c(.025, .975) ) 
        q_beta2 <- quantile(target_sim$beta2, probs = c(.025, .975) ) 
        q_beta3 <- quantile(target_sim$beta3, probs = c(.025, .975) ) 
        q_beta_age <- data.frame(target = event,
                                 predictor = c("age", "age2", "age3"),
                                 lb = c(q_beta1[1], q_beta2[1], q_beta3[1]),
                                 ub = c(q_beta1[2], q_beta2[2], q_beta3[2]))
    } else { #spline
      q_beta_age_mat <- apply(X = target_sim$beta_age,
                              MARGIN = 2,
                              FUN = quantile, probs = c(0.025, .975))
      q_beta_age <- data.frame(target = event,
                               predictor = paste0("age", 1:dim(q_beta_age_mat)[2]),
                               lb = q_beta_age_mat[1,],
                               ub = q_beta_age_mat[2,])
    }
  if (comp == "simple"){
    q_beta_y <- NA
  } else{ # compositional
    if (event == "hundred_m") {
      q_beta_y <- NA
    } else{
      q_beta_y <- apply(X = target_sim$betaY,
                        MARGIN = 2,
                        FUN = quantile, probs = c(0.025, .975))
      colnames(q_beta_y) <- dec_events[1:(i-1)]
      q_beta_y_lb <- data.frame(t(q_beta_y))[,1]
      q_beta_y_ub <- data.frame(t(q_beta_y))[,2]
      q_beta_y <- data.frame(target = event,
                             predictor = dec_events[1:(i-1)],
                             lb = q_beta_y_lb,
                             ub = q_beta_y_ub)%>%
        mutate(type = type,
               comp = comp,
               iter = iter)
    }
  }
  q_beta_age <- q_beta_age %>%
    mutate(type = type,
           comp = comp,
           iter = iter)
  bounds_df <- rbind(bounds_df, q_beta_age,
                     q_beta_y) %>%
    mutate(type = type,
           comp = comp,
           iter = iter) %>%
    drop_na()
} 
  


name = paste0("coverage_df", "_", comp, "_", type, "_", iter,
              ".csv")
write_csv(bounds_df, name)



