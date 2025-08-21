# edit: april 25, 2025: added new arguments to simulations. athlete_id (id's for predicted athletes) and is_new_athlete (indicator for if new athlete)

library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(splines)

# set up functions --------------------------------------------------------
wrangle_data <- function(data){
  df = data %>%
    drop_na(Birthdate, Performance) %>%
    mutate(Birthdate = mdy(Birthdate),
           Performance = mdy(Performance)) 
  colnames(df) <- c("all_time_decathlon_rank",
                      "all_time_decathlete",
                      "points",
                      "first",
                      "middle",
                      "last",
                      "country",
                      "birthdate",
                      "site",
                      "performance_date",
                      "hundred_m",
                      "long_jump",
                      "shot_put", 
                      "high_jump", 
                      "four_hundred_m",
                      "hurdles",
                      "discus",
                      "pole_vault",
                      "javelin",
                      "fifteen_hundred_m") 
  df <- df %>%
    mutate(age = as.numeric(performance_date - birthdate) / 365.25) %>%
    mutate(name = paste(first, last)) %>%
    select(name, hundred_m, long_jump, shot_put, high_jump, four_hundred_m, hurdles, discus, pole_vault, javelin, fifteen_hundred_m, age, points) 
  
  return(df)
}

## indexes atheletes
add_real_points2 <- function(data){
  points <- data %>%
    mutate(index = 1:nrow(data)) %>%
    mutate(long_jump = long_jump * 100,
           high_jump = high_jump * 100,
           pole_vault = pole_vault * 100) %>%
    pivot_longer(cols =  c("hundred_m", "long_jump", "shot_put", "high_jump",
                           "four_hundred_m", "hurdles", "discus",
                           "pole_vault", "javelin", "fifteen_hundred_m") ,
                 names_to = "event", values_to = "P") %>%
    mutate(event_type = case_when(event %in% c("hundred_m",
                                               "four_hundred_m", 
                                               "hurdles", 
                                               "fifteen_hundred_m") ~ "track",
                                  .default = "field"),
           A = case_when(event == "hundred_m" ~ 25.4347,
                         event == "long_jump" ~ 0.14354,
                         event == "shot_put" ~ 51.39,
                         event == "high_jump" ~ 0.8465,
                         event == "four_hundred_m" ~ 1.53775,
                         event == "hurdles" ~ 5.74352,
                         event == "discus" ~ 12.91,
                         event == "pole_vault" ~ 0.2797,
                         event == "javelin" ~ 10.14,
                         event == "fifteen_hundred_m" ~ 0.03768),
           B = case_when(event == "hundred_m" ~ 18,
                         event == "long_jump" ~ 220,
                         event == "shot_put" ~ 1.5,
                         event == "high_jump" ~ 75,
                         event == "four_hundred_m" ~ 82,
                         event == "hurdles" ~ 28.5,
                         event == "discus" ~ 4,
                         event == "pole_vault" ~ 100,
                         event == "javelin" ~ 7,
                         event == "fifteen_hundred_m" ~ 480),
           C = case_when(event == "hundred_m" ~ 1.81,
                         event == "long_jump" ~ 1.4,
                         event == "shot_put" ~ 1.05,
                         event == "high_jump" ~ 1.42,
                         event == "four_hundred_m" ~ 1.81,
                         event == "hurdles" ~ 1.92,
                         event == "discus" ~ 1.1,
                         event == "pole_vault" ~ 1.35,
                         event == "javelin" ~ 1.08,
                         event == "fifteen_hundred_m" ~ 1.85),
           calc_points = case_when(event_type == "track" ~ floor(A * ((B - P)^C)),
                                   event_type == "field" ~ floor(A * ((P - B)^C)))) %>%
    group_by(index) %>%
    summarize(calc_point = sum(calc_points))
  data <- cbind(data, points)
  return(data)
}

# get events' means and standard deviation into a dataframe form
get_event_sums_df <- function(decathlon_data){
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  event_sums <- decathlon_data[,dec_events] %>%
    pivot_longer(cols = 1:10,
                 names_to = "event",
                 values_to = "score") %>%
    group_by(event) %>%
    summarize(mean_score = mean(score),
              sd_score = sd(score))
  return(event_sums)
}


#given decathlon data and event summary dataframe, standardize decathlon event data
standardize_decathlon_data <- function(decathlon_data, event_sums){
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_data_standard <- decathlon_data 
  for (event in dec_events) {
    event_stats <- event_sums %>%
      filter(event == !!event)
    event_mean = event_stats$mean_score
    event_sd = event_stats$sd_score
    dec_data_standard[event] <- (dec_data_standard[event] - event_mean)/event_sd
  }
  return(dec_data_standard)
}

# read in individual event data
get_ind_data <- function(event, data_dir, event_sums){
  df <- read_excel(paste(data_dir,
                         paste(event, "_prog.xlsx", sep = ""),
                         sep = "")) %>%
    mutate(name = paste(First, Last),
           DOB = ymd(DOB)) %>%
    select(-c(First, Last)) %>%
    relocate(name, .after = Rank) 
  df <- df %>%
    pivot_longer(cols = 4:ncol(df), names_to = "year", values_to = "score") %>%
    drop_na()  %>%
    mutate(year = as.numeric(year),
           age = year - year(DOB)) %>%
    group_by(name, DOB) %>%
    mutate(athlete_id = cur_group_id(),
           event = !!event,
           mean = event_sums %>%
             filter(event == !!event) %>%
             select(mean_score) %>%
             pull(),
           sd = event_sums %>%
             filter(event == !!event) %>%
             select(sd_score) %>%
             pull(),
           score = (score - mean)/sd)
  return(df)
}

# get points from each event
convert_scores_to_points <- function(data){
  points <- data %>%
    ungroup() %>%
    mutate(index = row_number()) %>%
    mutate(long_jump = long_jump * 100,
           high_jump = high_jump * 100,
           pole_vault = pole_vault * 100) %>%
    pivot_longer(cols =  c("hundred_m", "long_jump", "shot_put", "high_jump",
                           "four_hundred_m", "hurdles", "discus",
                           "pole_vault", "javelin", "fifteen_hundred_m") ,
                 names_to = "event", values_to = "P") %>%
    mutate(event_type = case_when(event %in% c("hundred_m",
                                               "four_hundred_m", 
                                               "hurdles", 
                                               "fifteen_hundred_m") ~ "track",
                                  .default = "field"),
           A = case_when(event == "hundred_m" ~ 25.4347,
                         event == "long_jump" ~ 0.14354,
                         event == "shot_put" ~ 51.39,
                         event == "high_jump" ~ 0.8465,
                         event == "four_hundred_m" ~ 1.53775,
                         event == "hurdles" ~ 5.74352,
                         event == "discus" ~ 12.91,
                         event == "pole_vault" ~ 0.2797,
                         event == "javelin" ~ 10.14,
                         event == "fifteen_hundred_m" ~ 0.03768),
           B = case_when(event == "hundred_m" ~ 18,
                         event == "long_jump" ~ 220,
                         event == "shot_put" ~ 1.5,
                         event == "high_jump" ~ 75,
                         event == "four_hundred_m" ~ 82,
                         event == "hurdles" ~ 28.5,
                         event == "discus" ~ 4,
                         event == "pole_vault" ~ 100,
                         event == "javelin" ~ 7,
                         event == "fifteen_hundred_m" ~ 480),
           C = case_when(event == "hundred_m" ~ 1.81,
                         event == "long_jump" ~ 1.4,
                         event == "shot_put" ~ 1.05,
                         event == "high_jump" ~ 1.42,
                         event == "four_hundred_m" ~ 1.81,
                         event == "hurdles" ~ 1.92,
                         event == "discus" ~ 1.1,
                         event == "pole_vault" ~ 1.35,
                         event == "javelin" ~ 1.08,
                         event == "fifteen_hundred_m" ~ 1.85),
           calc_points = case_when(event_type == "track" ~ floor(A * ((B - P)^C)),
                                   event_type == "field" ~ floor(A * ((P - B)^C)))) %>%
    select(dob, name, subj_id, age, points, athlete_id, index, event, calc_points) %>%
      pivot_wider(names_from = event,
                  values_from = calc_points)
  return(points)
}
# models ------------------------------------------------------------------

# baseline cubic
get_baseline_cubic_sim <- function(age_vec, 
                                   athlete_id, is_new_athlete,
                                   decathlon_data, stan_dir, 
                                   iter = 2000,
                                   return_all = F){
  age_mean = mean(decathlon_data$age)
  age_sd = sd(decathlon_data$age)
  points_mean = mean(decathlon_data$points)
  points_sd = sd(decathlon_data$points)
  # standardize ages
  N_decathlon <- nrow(decathlon_data)
  age_pred <- (age_vec - age_mean) / age_sd
  N_pred <- length(age_pred)
  
  athlete_decathlon <- decathlon_data$athlete_id
  Y_decathlon <- (decathlon_data$points - points_mean)/points_sd
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  
  data_list <- list(
    N_decathlon  = N_decathlon,
    age_pred = age_pred,
    N_pred = N_pred,
    A_decathlon = A_decathlon,
    athlete_decathlon = athlete_decathlon,
    Y_decathlon = Y_decathlon,
    age_decathlon = age_decathlon,
    athlete_pred = athlete_pred,
    is_new_athlete = is_new_athlete
  )
  # baseline cubic
  mod <- stan(paste0(stan_dir, "baseline_cubic.stan"),
              data = data_list,
              chains=4,
              iter = iter)
  
  
  post_sim <- (extract(mod)$Y_pred* points_sd) + points_mean
  post_pred <- apply(post_sim, 2, mean)
  mu <- mean(extract(mod)$mu_alpha)
  beta1 <- mean(extract(mod)$beta1)
  beta2 <- mean(extract(mod)$beta2)
  beta3 <- mean(extract(mod)$beta3)
  sigma_decathlon <- mean(extract(mod)$sigma_decathlon)
  params <- list(mu = mu,
                 beta1 = beta1,
                 beta2 = beta2,
                 beta3 = beta3)
  if (return_all == T) {
    results <- list(
      params = params,
      post_sim = post_sim,
      post_pred = post_pred)
  } else{
    results  <- list(
      # params = params,
      post_sim = post_sim,
      post_pred = post_pred)
  }
  
  return(results)
}

# turn simple model simulations to a dataframe with event results and points
sim_to_df <- function(sim, age_vec, iter){
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin", "fifteen_hundred_m")
  sims_vec <- paste0(dec_events, "_sims")
  test_df <- data.frame()
  event= dec_events[1]
  prev_events <- data.frame(sim[[1]][['post_sim']])
  prev_events <- prev_events %>%
    pivot_longer(cols = 1:length(age_vec), names_to = "age",
                 values_to = event) %>%
    mutate(athlete = as.integer(sub(".", "", age)),
           age = rep(age_vec, iter/2*4),
           age = as.numeric(age),
           row_num = row_number()) %>%
    relocate(athlete, row_num)
  for (i in 2:length(dec_events)) {
    event = dec_events[i]
    print(event)
    df <- data.frame(sim[[i]][['post_sim']])
    df <- df %>%
      pivot_longer(cols = 1:length(age_vec), names_to = "age",
                   values_to = event) %>%
      mutate(athlete = as.integer(sub(".", "", age)),
             age = rep(age_vec, iter/2*4),
             age = as.numeric(age),
             row_num = row_number()) %>%
      relocate(athlete, row_num)
    prev_events <- left_join(prev_events, df)
    
  }
  
  sim_events <- prev_events
  
  
  sim_events <- sim_events %>%
    add_real_points2()
  return(sim_events)
}

get_simple_cubic_sim <- function(age_vec,
                                 athlete_id,
                                 is_new_athlete,
                                 decathlon_data, event_sums,
                                 stan_dir,
                                 iter = 2000,
                                 return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin", "fifteen_hundred_m")
  N_decathlon <- nrow(decathlon_data)
  N_pred <- length(age_vec)
  athlete_decathlon <- decathlon_data$athlete_id
  #standardize age
  age_mean <- mean(decathlon_data$age)
  age_sd <- sd(decathlon_data$age)
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  age_pred <- (age_vec - age_mean) / age_sd
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  for (event in dec_events) {
    print(event)
    Y_decathlon <- decathlon_data[[event]]
    data_list <- list(
      N_decathlon  = N_decathlon,
      age_pred = age_pred,
      N_pred = N_pred,
      A_decathlon = A_decathlon,
      athlete_decathlon = athlete_decathlon,
      Y_decathlon = Y_decathlon,
      age_decathlon = age_decathlon,
      athlete_pred = athlete_pred,
      is_new_athlete = is_new_athlete
    )
    mod <- stan(paste0(stan_dir, "simple_cubic.stan"),
                data = data_list, chains=4,
                iter = iter)
    event_mean = event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd = event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    post_sim <- (extract(mod)$Y_pred * event_sd) + event_mean
    post_pred <- apply(post_sim, 2, mean)
    mu <- (extract(mod)$mu_alpha)
    beta1 <- (extract(mod)$beta1)
    beta2 <- (extract(mod)$beta2)
    beta3 <- (extract(mod)$beta3)
    sigma <- (extract(mod)$sigma_decathlon)
    sim <- list(mu = mu,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
  }
  sim_events <- sim_to_df(sims_list, age_vec, iter)
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
 
  return(results)
}


get_simple_joint_cubic_sim <- function(age_vec, 
                                       athlete_id,
                                       is_new_athlete,
                                       decathlon_data, event_sums,
                                       stan_dir, ind_data_dir,
                                       iter = iter,
                                       return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin", "fifteen_hundred_m")
  
  #standardize age
  age_mean <- mean(decathlon_data$age)
  age_sd <- sd(decathlon_data$age)
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  age_pred <- (age_vec - age_mean) / age_sd
  
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  for (event in dec_events) {
    print(event)
    y_mean = event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    y_sd = event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    ind_data <- get_ind_data(event, paste(ind_data_dir,
                                          sep = ""),
                             event_sums = event_sums) 
    Y_decathlon <- decathlon_data[[event]] 
    Y_individual <- ind_data$score 
    N_decathlon <- nrow(decathlon_data)
    N_individual <- nrow(ind_data)
    N_pred <- length(age_pred)
    A_individual <- length(unique(ind_data$athlete_id))
    athlete_individual <- ind_data$athlete_id
    age_individual <- (ind_data$age - age_mean) / age_sd
    athlete_pred <- athlete_id # athlete id's
    is_new_athlete <- is_new_athlete 
  
    data_list <- list(
      N_decathlon  = N_decathlon,
      N_individual = N_individual,
      age_pred = age_pred,
      N_pred = N_pred,
      A_decathlon = A_decathlon,
      A_individual = A_individual,
      athlete_decathlon = athlete_decathlon,
      athlete_individual = athlete_individual,
      Y_decathlon = Y_decathlon,
      Y_individual = Y_individual,
      age_decathlon = age_decathlon,
      age_individual = age_individual,
      athlete_pred = athlete_pred,
      is_new_athlete = is_new_athlete
    )
    mod <- stan(paste0(stan_dir, "simple_joint_cubic.stan"),
                data = data_list, chains=4,
                iter = iter)
    post_sim <- (extract(mod)$Y_pred * y_sd) +y_mean
    post_pred <- apply(post_sim, 2, mean)
    mu <- (extract(mod)$mu_alpha_decathlon)
    beta1 <- (extract(mod)$beta1)
    beta2 <- (extract(mod)$beta2)
    beta3 <- (extract(mod)$beta3)
    sigma <- (extract(mod)$sigma_decathlon)
    sim <- list(mu = mu,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
  }
  sim_events <- sim_to_df(sims_list, age_vec, iter)
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}


get_simple_moment_cubic_sim <- function(age_vec, 
                                        athlete_id,
                                        is_new_athlete,
                                        decathlon_data, event_sums,
                                        stan_dir, ind_data_dir,
                                        iter = 2000,
                                        return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  age_pred <- (age_vec - age_mean) / age_sd
  
  
  ind_data_list <- list()
  for (event in dec_events){
    name = paste0(event, "_ind_data")
    ind_data <- get_ind_data(event, paste(ind_data_dir,
                                          sep = ""),
                             event_sums = event_sums)
    assign(name, ind_data)
    ind_data_list[[name]] = ind_data
  }
  for (i in 1:length(dec_events)) {
    print(dec_events[i])
    N_individual <- nrow(ind_data_list[[i]])
    Y_individual <- unlist(ind_data_list[[i]][["score"]])
    age_individual <- (unlist(ind_data_list[[i]][["age"]]) - age_mean) / age_sd
    ind_athlete_ids <- ind_data_list[[i]]$'athlete_id'
    #simple cubic on individual data. no predictions.
    individual_cubic_list <- list(N_decathlon = N_individual,
                                  N_pred = 2,
                                  A_decathlon = length(unique(ind_athlete_ids)),
                                  athlete_decathlon = ind_athlete_ids,
                                  Y_decathlon = Y_individual,
                                  age_decathlon = age_individual,
                                  age_pred = rep(1,2),  # Dummy value,
                                  is_new_athlete = rep(1,2), 
                                  athlete_pred = rep(1,2))  
    ind_mod <- stan(paste0(stan_dir, "simple_cubic.stan"),
                    data = individual_cubic_list, chains=4)           
    mu <- (extract(ind_mod)$mu_alpha)
    beta1 <- (extract(ind_mod)$beta1)
    beta2 <- (extract(ind_mod)$beta2)
    beta3 <- (extract(ind_mod)$beta3)
    beta_means <- c(mean(beta1), mean(beta2), mean(beta3))
    beta_sds <- c(sd(beta1), sd(beta2), sd(beta3))
    mu_mean <- mean(mu)
    mu_sd <- sd(mu)
    
    N_decathlon <- nrow(decathlon_data)
    N_pred <- length(age_pred)
    athlete_decathlon <- decathlon_data$athlete_id
    age_decathlon <- (decathlon_data$age - age_mean) / age_sd
    Y_decathlon <- decathlon_data[[dec_events[i]]]
    
    athlete_pred <- athlete_id # athlete id's
    is_new_athlete <- is_new_athlete 
    A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
    moment_matched_cubic_list <- list(N_decathlon = N_decathlon,
                          N_pred = N_pred,
                          A_decathlon = A_decathlon,
                          athlete_decathlon = athlete_decathlon,
                          Y_decathlon = Y_decathlon,
                          age_decathlon = age_decathlon,
                          age_pred = age_pred, 
                          is_new_athlete = is_new_athlete, 
                          athlete_pred = athlete_id,
                          beta_means = beta_means,
                          beta_sds = beta_sds,
                          mu_mean = mu_mean,
                          mu_sd = mu_sd)  
    moment_matched_mod <- stan(paste0(stan_dir, "simple_moment_matched_cubic.stan"),
                               data = moment_matched_cubic_list,
                               chains=4,
                               iter = iter)  
    event = dec_events[i]
    event_mean = event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd = event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    post_sim <- (extract(moment_matched_mod)$Y_pred * event_sd) + event_mean
    post_pred <- apply(post_sim, 2, mean)
    mu <- (extract(moment_matched_mod)$mu_alpha)
    beta1 <- (extract(moment_matched_mod)$beta1)
    beta2 <- (extract(moment_matched_mod)$beta2)
    beta3 <- (extract(moment_matched_mod)$beta3)
    sigma <- (extract(moment_matched_mod)$sigma_decathlon)
    sim <- list(mu = mu,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
  }
  
  sim_events <- sim_to_df(sims_list, age_vec, iter)
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}


get_comp_cubic_sim <- function(age_vec, 
                               athlete_id,
                               is_new_athlete,
                               decathlon_data, event_sums,
                               stan_dir, iter = 2000,
                               return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  
  N_decathlon <- nrow(decathlon_data)
  age_pred <- (age_vec- age_mean) / age_sd
  N_pred <- length(age_pred)
  
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  
  event = dec_events[1]
  Y_decathlon <- decathlon_data[[event]]
  data_list <- list(
    N_decathlon  = N_decathlon,
    age_pred = age_pred,
    N_pred = N_pred,
    A_decathlon = A_decathlon,
    athlete_decathlon = athlete_decathlon,
    Y_decathlon = Y_decathlon,
    age_decathlon = age_decathlon,
    athlete_pred = athlete_pred,
    is_new_athlete = is_new_athlete
  )
  mod <- stan(paste0(stan_dir, "simple_cubic.stan"),
              data = data_list, chains=4,
              iter = iter)
  post_sim <- (extract(mod)$Y_pred )
  post_pred <- apply(post_sim, 2, mean)
  mu <- (extract(mod)$mu_alpha)
  alpha_effect <- extract(mod)$alpha_effect
  beta1 <- (extract(mod)$beta1)
  beta2 <- (extract(mod)$beta2)
  beta3 <- (extract(mod)$beta3)
  sigma <- (extract(mod)$sigma_decathlon)
  sim <- list(mu = mu,
              beta1 = beta1,
              beta2 = beta2,
              beta3 = beta3,
              sigma = sigma,
              post_sim = post_sim,
              post_pred = post_pred)
  sims_list[[paste0(event, "_sims")]] <- sim
  prev_events <- data.frame(post_sim)
  prev_events <- prev_events %>%
    pivot_longer(cols = 1:length(age_vec), names_to = "age",
                 values_to = event) %>%
    mutate(athlete = as.integer(sub(".", "", age)),
           age = rep(age_vec, iter/2*4),
           age = as.numeric(age),
           row_num = row_number()) %>%
    relocate(athlete, row_num)
  for (i in 2:length(dec_events)) {
    event = dec_events[i]
    print(event)
    N_prev_events <- i-1
    Y_decathlon <- (decathlon_data[[dec_events[i]]])
    Y_prev_events <- decathlon_data[dec_events[1:N_prev_events]]
    data_list_compositional_cubic <- list(
      N_decathlon  = N_decathlon,
      N_pred = N_pred,
      N_prev_events = dim(Y_prev_events)[2],
      A_decathlon = A_decathlon,
      athlete_decathlon = athlete_decathlon,
      Y_decathlon = Y_decathlon,
      Y_prev_events = Y_prev_events,
      age_decathlon = age_decathlon,
      is_new_athlete = is_new_athlete,
      athlete_pred = athlete_pred
    )
    
    model <- stan(paste0(stan_dir, "compositional_cubic.stan"),
                  data = data_list_compositional_cubic, chains=4,
                  iter = iter)
    
    
    # extract parameters
    mu <- extract(model)$mu_alpha
    alpha_effect <- extract(model)$alpha_effect
    beta1 <- extract(model)$beta1
    beta2 <- extract(model)$beta2
    beta3 <- extract(model)$beta3
    betaY <- extract(model)$betaY
    sigma <- extract(model)$sigma_decathlon
    sim <- list(mu = mu,
                alpha_effect = alpha_effect,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                betaY = betaY,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
    # make predictions
    dec_event = dec_events[i]
    df <- as.data.frame(matrix(nrow = length(mu),
                               ncol = length(age_pred)))
    
    for (k in 1:length(age_pred)) {
      pred_age = age_pred[k]
      mean_vec = alpha_effect[,k] + 
        (beta1 * pred_age) +
        (beta2 * pred_age^2) +
        (beta3 * pred_age^3) 
      if (i == 2){
        Y_effect <-
          betaY * prev_events %>%
          filter(athlete == k) %>%
          select(dec_events[i-1])  %>%
          pull()
      } else{
        Y_effect <- 0
        for (j in 1:dim(betaY)[2]) {
          Y_effect <- Y_effect +  
            (betaY[,j] * prev_events %>%
               filter(athlete == k) %>%
               select(dec_events[j]) %>%
               pull()) 
        }
      }
      mean_vec <- mean_vec + drop(Y_effect)
      age_sim <- rnorm(length(mean_vec), mean_vec, sigma)
      df[,k] <- age_sim
      colnames(df)[k] <- k
    }
    df <- df %>%
      pivot_longer(cols = 1:length(age_pred),
                   names_to = "athlete",
                   values_to = dec_event)  %>%
      mutate(athlete = as.integer(athlete),
             row_num = row_number())
    prev_events <- left_join(prev_events, df)
    
  }
  
  sim_events <- prev_events
  
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_events[event] <- (prev_events[event] *event_sd ) + event_mean
    
  }
  
  sim_events <- sim_events %>%
    add_real_points2()
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}


get_comp_joint_cubic_sim <- function(age_vec, 
                                     athlete_id,
                                     is_new_athlete,
                                     decathlon_data,
                                     event_sums, stan_dir,
                                     ind_data_dir,iter = 2000,
                                     return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  event = dec_events[1]
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  ind_data <- get_ind_data(event, ind_data_dir, event_sums)
  age_pred <- (age_vec- age_mean) / age_sd
  
  Y_decathlon <- decathlon_data[[event]]
  N_decathlon <- nrow(decathlon_data)
  N_pred <- length(age_pred)
  
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  
  #individual data
  Y_individual <- ind_data$score
  age_individual <- (ind_data$age - age_mean) / age_sd
  N_individual <- nrow(ind_data)
  A_individual <- length(unique(ind_data$athlete_id))
  athlete_individual <- ind_data$athlete_id
  
  #future pred
  athlete_pred <- athlete_id
  is_new_athlete <- is_new_athlete
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  
  data_list <- list(
    N_decathlon  = N_decathlon,
    age_pred = age_pred,
    N_pred = N_pred,
    A_decathlon = A_decathlon,
    athlete_decathlon = athlete_decathlon,
    Y_decathlon = Y_decathlon,
    age_decathlon = age_decathlon,
    Y_individual = Y_individual,
    N_individual = N_individual,
    age_individual = age_individual,
    A_individual = A_individual,
    athlete_individual = athlete_individual,
    athlete_pred = athlete_pred,
    is_new_athlete = is_new_athlete
  )
  mod <- stan(paste0(stan_dir, "simple_joint_cubic.stan"),
              data = data_list, chains=4,
              iter = iter)
  post_sim <- (extract(mod)$Y_pred )
  post_pred <- apply(post_sim, 2, mean)
  mu <- (extract(mod)$mu_alpha_decathlon)
  alpha_effect <- extract(mod)$alpha_effect
  beta1 <- (extract(mod)$beta1)
  beta2 <- (extract(mod)$beta2)
  beta3 <- (extract(mod)$beta3)
  sigma <- (extract(mod)$sigma_decathlon)
  sim <- list(mu = mu,
              alpha_effect = alpha_effect,
              beta1 = beta1,
              beta2 = beta2,
              beta3 = beta3,
              sigma = sigma,
              post_sim = post_sim,
              post_pred = post_pred)
  sims_list[[paste0(event, "_sims")]] <- sim
  prev_events <- data.frame(post_sim)
  prev_events <- prev_events %>%
    pivot_longer(cols = 1:length(age_vec), names_to = "age",
                 values_to = event) %>%
    mutate(athlete = as.integer(sub(".", "", age)),
           age = rep(age_vec, iter/2*4),
           age = as.numeric(age),
           row_num = row_number()) %>%
    relocate(athlete, row_num)
  for (i in 2:length(dec_events)) {
    event = dec_events[i]
    print(event)
    ind_data <- get_ind_data(event, ind_data_dir, event_sums)
    Y_individual <- ind_data$score
    age_individual <- (ind_data$age - age_mean) / age_sd
    N_individual <- nrow(ind_data)
    A_individual <- length(unique(ind_data$athlete_id))
    athlete_individual <- ind_data$athlete_id
    
    N_prev_events <- i-1
    Y_decathlon <- (decathlon_data[[dec_events[i]]])
    Y_prev_events <- decathlon_data[dec_events[1:N_prev_events]]
    data_list_compositional_joint_cubic <- list(
      N_decathlon  = N_decathlon,
      age_pred = age_pred,
      N_pred = N_pred,
      N_prev_events = dim(Y_prev_events)[2],
      A_decathlon = A_decathlon,
      athlete_decathlon = athlete_decathlon,
      Y_decathlon = Y_decathlon,
      Y_prev_events = Y_prev_events,
      age_decathlon = age_decathlon,
      Y_individual = Y_individual,
      age_individual = age_individual,
      N_individual = N_individual,
      A_individual = A_individual,
      athlete_individual = athlete_individual,
      athlete_pred = athlete_pred,
      is_new_athlete = is_new_athlete
    )
    
    model <- stan(paste0(stan_dir, "compositional_joint_cubic.stan"),
                  data = data_list_compositional_joint_cubic, chains=4,
                  iter = iter)
    
    
    # extract parameters
    mu <- extract(model)$mu_alpha_decathlon
    alpha_effect <- extract(model)$alpha_effect
    beta1 <- extract(model)$beta1
    beta2 <- extract(model)$beta2
    beta3 <- extract(model)$beta3
    betaY <- extract(model)$betaY
    sigma <- extract(model)$sigma_decathlon
    sim <- list(mu = mu,
                alpha_effect = alpha_effect,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                betaY = betaY,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
    # make predictions
    dec_event = dec_events[i]
    df <- as.data.frame(matrix(nrow = nrow(post_sim),
                               ncol = length(age_pred)))
    
    
    for (k in 1:length(age_pred)) {
      pred_age = age_pred[k]
      mean_vec = alpha_effect[,k] + 
        (beta1 * pred_age) +
        (beta2 * pred_age^2) +
        (beta3 * pred_age^3) 
      if (i == 2){
        Y_effect <-
          betaY * prev_events %>%
          filter(athlete == k) %>%
          select(dec_events[i-1])  %>%
          pull()
      } else{
        Y_effect <- 0
        for (j in 1:dim(betaY)[2]) {
          Y_effect <- Y_effect +  ## edit here
            (betaY[,j] * prev_events %>%
               filter(athlete == k) %>%
               select(dec_events[j]) %>%
               pull()) 
        }
      }
      mean_vec <- mean_vec + drop(Y_effect)
      age_sim <- rnorm(length(mean_vec), mean_vec, sigma)
      df[,k] <- age_sim
      colnames(df)[k] <- k
    }
    df <- df %>%
      pivot_longer(cols = 1:length(age_pred),
                   names_to = "athlete",
                   values_to = dec_event)  %>%
      mutate(athlete = as.integer(athlete),
             row_num = row_number())
    prev_events <- left_join(prev_events, df)
    
  }
  
  sim_events <- prev_events
  
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_events[event] <- (sim_events[event] *event_sd ) + event_mean
    
  }
  
  sim_events <- sim_events %>%
    add_real_points2()
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}


get_comp_moment_cubic_sim <- function(age_vec, 
                                      athlete_id,
                                      is_new_athlete,
                                      decathlon_data, event_sums,
                                      stan_dir, ind_data_dir,
                                      iter = 2000,
                                      return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  
  
  ind_data_list <- list()
  for (event in dec_events){
    name = paste0(event, "_ind_data")
    ind_data <- get_ind_data(event, paste(ind_data_dir,
                                          sep = ""),
                             event_sums = event_sums)
    assign(name, ind_data)
    ind_data_list[[name]] = ind_data
  }
  
  N_individual <- nrow(ind_data_list[[1]])
  Y_individual <- unlist(ind_data_list[[1]][["score"]])
  age_individual <- (unlist(ind_data_list[[1]][["age"]]) - age_mean) / age_sd
  ind_athlete_ids <- ind_data_list[[1]]$'athlete_id'
  #simple cubic on individual data. no predictions.
  individual_cubic_list <- list(N_decathlon = N_individual,
                                N_pred = 2,
                                A_decathlon = length(unique(ind_athlete_ids)),
                                athlete_decathlon = ind_athlete_ids,
                                Y_decathlon = Y_individual,
                                age_decathlon = age_individual,
                                age_pred = rep(1,2),  # Dummy value,
                                is_new_athlete = rep(1,2), 
                                athlete_pred = rep(1,2))  
  ind_mod <- stan(paste0(stan_dir, "simple_cubic.stan"),
                  data = individual_cubic_list, chains=4)           
  mu <- (extract(ind_mod)$mu_alpha)
  beta1 <- (extract(ind_mod)$beta1)
  beta2 <- (extract(ind_mod)$beta2)
  beta3 <- (extract(ind_mod)$beta3)
  beta_means <- c(mean(beta1), mean(beta2), mean(beta3))
  mu_mean <- mean(mu)
  
  
  
  N_decathlon <- nrow(decathlon_data)
  age_pred <- (age_vec- age_mean) / age_sd
  N_pred <- length(age_pred)
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  athlete_pred <- athlete_id
  is_new_athlete <- is_new_athlete
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  event = dec_events[1]
  Y_decathlon <- decathlon_data[[event]]
  data_list <- list(
    N_decathlon  = N_decathlon,
    age_pred = age_pred,
    N_pred = N_pred,
    A_decathlon = A_decathlon,
    athlete_decathlon = athlete_decathlon,
    Y_decathlon = Y_decathlon,
    age_decathlon = age_decathlon,
    athlete_pred = athlete_pred,
    is_new_athlete = is_new_athlete,
    beta_means = beta_means,
    mu_mean = mu_mean
  )
  
  
  
  mod <- stan(paste0(stan_dir, "simple_moment_matched_cubic.stan"),
              data = data_list, chains=4,
              iter = iter)
  post_sim <- (extract(mod)$Y_pred )
  post_pred <- apply(post_sim, 2, mean)
  mu <- (extract(mod)$mu_alpha)
  alpha_effect <- extract(mod)$alpha_effect
  beta1 <- (extract(mod)$beta1)
  beta2 <- (extract(mod)$beta2)
  beta3 <- (extract(mod)$beta3)
  sigma <- (extract(mod)$sigma_decathlon)
  sim <- list(mu = mu,
              alpha_effect = alpha_effect,
              beta1 = beta1,
              beta2 = beta2,
              beta3 = beta3,
              sigma = sigma,
              post_sim = post_sim,
              post_pred = post_pred)
  sims_list[[paste0(event, "_sims")]] <- sim
  prev_events <- data.frame(post_sim)
  prev_events <- prev_events %>%
    pivot_longer(cols = 1:length(age_vec), names_to = "age",
                 values_to = event) %>%
    mutate(athlete = as.integer(sub(".", "", age)),
           age = rep(age_vec, iter/2*4),
           age = as.numeric(age),
           row_num = row_number()) %>%
    relocate(athlete, row_num)
  for (i in 2:10) {
    event = dec_events[i]
    print(event)
    N_individual <- nrow(ind_data_list[[i]])
    Y_individual <- unlist(ind_data_list[[i]][["score"]])
    age_individual <- (unlist(ind_data_list[[i]][["age"]]) - age_mean) / age_sd
    ind_athlete_ids <- ind_data_list[[i]]$'athlete_id'
    #simple cubic on individual data. no predictions.
    individual_cubic_list <- list(N_decathlon = N_individual,
                                  N_pred = 2,
                                  A_decathlon = length(unique(ind_athlete_ids)),
                                  athlete_decathlon = ind_athlete_ids,
                                  Y_decathlon = Y_individual,
                                  age_decathlon = age_individual,
                                  age_pred = rep(1,2),  # Dummy value,
                                  is_new_athlete = rep(1,2), 
                                  athlete_pred = rep(1,2))  
    ind_mod <- stan(paste0(stan_dir, "simple_cubic.stan"),
                    data = individual_cubic_list, chains=4)           
    mu <- (extract(ind_mod)$mu_alpha)
    beta1 <- (extract(ind_mod)$beta1)
    beta2 <- (extract(ind_mod)$beta2)
    beta3 <- (extract(ind_mod)$beta3)
    beta_means <- c(mean(beta1), mean(beta2), mean(beta3))
    mu_mean <- mean(mu)
    
   
    Y_decathlon <- decathlon_data[[dec_events[i]]]
    
    N_prev_events <- i-1
    Y_prev_events <- decathlon_data[dec_events[1:N_prev_events]]
    
    moment_matched_cubic_list <- list(N_decathlon = N_decathlon,
                                      N_pred = N_pred,
                                      A_decathlon = A_decathlon,
                                      athlete_decathlon = athlete_decathlon,
                                      Y_decathlon = Y_decathlon,
                                      age_decathlon = age_decathlon,
                                      age_pred = age_pred,  
                                      N_prev_events = N_prev_events,
                                      Y_prev_events = Y_prev_events,
                                      is_new_athlete = is_new_athlete, 
                                      athlete_pred = athlete_pred,
                                      beta_means = beta_means,
                                      mu_mean = mu_mean)  
    
    
    moment_matched_mod <- stan(paste0(stan_dir, "compositional_moment_matched_cubic.stan"),
                               data = moment_matched_cubic_list, chains=4,
                               iter= iter)
    
    # extract parameters
    mu <- extract(moment_matched_mod)$mu_alpha
    alpha_effect <-extract(moment_matched_mod)$alpha_effect
    beta1 <- extract(moment_matched_mod)$beta1
    beta2 <- extract(moment_matched_mod)$beta2
    beta3 <- extract(moment_matched_mod)$beta3
    betaY <- extract(moment_matched_mod)$betaY
    sigma <- extract(moment_matched_mod)$sigma_decathlon
    sim <- list(mu = mu,
                alpha_effect = alpha_effect,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                betaY = betaY)
    sims_list[[paste0(event, "_sims")]] <- sim
    # make predictions
    dec_event = dec_events[i]
    df <- as.data.frame(matrix(nrow =  length(mu),
                               ncol = length(age_pred)))
    
    
    for (k in 1:length(age_pred)) {
      pred_age = age_pred[k]
      mean_vec = alpha_effect[,k] + 
        (beta1 * pred_age) +
        (beta2 * pred_age^2) +
        (beta3 * pred_age^3) 
      if (i == 2){
        Y_effect <-
          betaY * prev_events %>%
          filter(athlete == k) %>%
          select(dec_events[i-1])  %>%
          pull()
      } else{
        Y_effect <- 0
        for (j in 1:dim(betaY)[2]) {
          Y_effect <- Y_effect +  ## edit here
            (betaY[,j] * prev_events %>%
               filter(athlete == k) %>%
               select(dec_events[j]) %>%
               pull()) 
        }
      }
      mean_vec <- mean_vec + drop(Y_effect)
      age_sim <- rnorm(length(mean_vec), mean_vec, sigma)
      df[,k] <- age_sim
      colnames(df)[k] <- k
    }
    df <- df %>%
      pivot_longer(cols = 1:length(age_pred),
                   names_to = "athlete",
                   values_to = dec_event)  %>%
      mutate(athlete = as.integer(athlete),
             row_num = row_number())
    prev_events <- left_join(prev_events, df)
    
  }
  
  sim_events <- prev_events
  
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_events[event] <- (sim_events[event] *event_sd ) + event_mean
    
  }
  
  sim_events <- sim_events %>%
    add_real_points2()
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}



# spline models now -------------------------------------------------------

# baseline cubic
get_baseline_spline_sim <- function(age_vec, 
                                   athlete_id, is_new_athlete,
                                   decathlon_data, stan_dir, 
                                   iter = 2000,
                                   return_all = F){
  age_mean = mean(decathlon_data$age)
  age_sd = sd(decathlon_data$age)
  points_mean = mean(decathlon_data$points)
  points_sd = sd(decathlon_data$points)
  # standardize ages
  N_decathlon <- nrow(decathlon_data)
  age_pred <- (age_vec - age_mean) / age_sd
  N_pred <- length(age_pred)
  
  athlete_decathlon <- decathlon_data$athlete_id
  Y_decathlon <- (decathlon_data$points - points_mean)/points_sd
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  
  knots <- quantile(age_decathlon, probs = seq(0.1, 0.9, by = 0.1))
  # For decathlon data
  age_basis_decathlon <- bs(age_decathlon, degree = 3, knots = knots, intercept = TRUE)
  # For predictions
  age_basis_pred <- predict(age_basis_decathlon, newx = age_pred)
  
  data_list <- list(
    N_decathlon  = N_decathlon,
    N_pred = N_pred,
    A_decathlon = A_decathlon,
    athlete_decathlon = athlete_decathlon,
    Y_decathlon = Y_decathlon,
    age_basis_decathlon = age_basis_decathlon,
    age_basis_pred = age_basis_pred,
    athlete_pred = athlete_pred,
    is_new_athlete = is_new_athlete
  )
  # baseline cubic
  mod <- stan(paste0(stan_dir, "simple_spline.stan"),
              data = data_list,
              chains=4,
              iter = iter)
  
  
  post_sim <- (extract(mod)$Y_pred* points_sd) + points_mean
  post_pred <- apply(post_sim, 2, mean)
  mu <- mean(extract(mod)$mu_alpha)
  beta_age <- apply(extract(mod)$beta_age,
                    MARGIN = 2,
                    mean)
  sigma_decathlon <- mean(extract(mod)$sigma_decathlon)
  params <- list(mu = mu,
                 beta_age = beta_age,
                 sigma = sigma_decathlon)
  if (return_all == T) {
    results <- list(
      params = params,
      post_sim = post_sim,
      post_pred = post_pred)
  } else{
    results  <- list(
      # params = params,
      post_sim = post_sim,
      post_pred = post_pred)
  }
  return(results)
}

get_simple_spline_sim <- function(age_vec,
                                 athlete_id,
                                 is_new_athlete,
                                 decathlon_data, event_sums,
                                 stan_dir,
                                 iter = 2000,
                                 return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin", "fifteen_hundred_m")
  N_decathlon <- nrow(decathlon_data)
  N_pred <- length(age_vec)
  athlete_decathlon <- decathlon_data$athlete_id
  #standardize age
  age_mean <- mean(decathlon_data$age)
  age_sd <- sd(decathlon_data$age)
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  age_pred <- (age_vec - age_mean) / age_sd
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  knots <- quantile(age_decathlon, probs = seq(0.1, 0.9, by = 0.1))
  # For decathlon data
  age_basis_decathlon <- bs(age_decathlon, degree = 3, knots = knots, intercept = TRUE)
  # For predictions
  age_basis_pred <- predict(age_basis_decathlon, newx = age_pred)
  for (event in dec_events) {
    print(event)
    Y_decathlon <- decathlon_data[[event]]
    data_list <- list(
      N_decathlon  = N_decathlon,
      age_basis_pred = age_basis_pred,
      N_pred = N_pred,
      A_decathlon = A_decathlon,
      athlete_decathlon = athlete_decathlon,
      Y_decathlon = Y_decathlon,
      age_basis_decathlon = age_basis_decathlon,
      athlete_pred = athlete_pred,
      is_new_athlete = is_new_athlete
    )
    mod <- stan(paste0(stan_dir, "simple_spline.stan"),
                data = data_list,
                chains=4,
                iter = iter)
    event_mean = event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd = event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    post_sim <- (extract(mod)$Y_pred * event_sd) + event_mean
    post_pred <- apply(post_sim, 2, mean)
    # mu <- mean(extract(mod)$mu_alpha)
    # beta_age <- apply(extract(mod)$beta_age,
    #                   MARGIN = 2,
    #                   mean)
    # sigma_decathlon <- mean(extract(mod)$sigma_decathlon)
    mu <- extract(mod)$mu_alpha
    beta_age <- extract(mod)$beta_age
    sigma_decathlon <- extract(mod)$sigma_decathlon
    sim <- list(mu = mu,
                beta_age = beta_age,
                sigma_decathlon = sigma_decathlon,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
  }
  sim_events <- sim_to_df(sims_list, age_vec, iter)
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}



get_simple_moment_spline_sim <- function(age_vec, 
                                        athlete_id,
                                        is_new_athlete,
                                        decathlon_data, event_sums,
                                        stan_dir, ind_data_dir,
                                        iter = 2000,
                                        return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  age_pred <- (age_vec - age_mean) / age_sd
  
  N_decathlon <- nrow(decathlon_data)
  N_pred <- length(age_pred)
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  
  
  
  
  ind_data_list <- list()
  for (event in dec_events){
    name = paste0(event, "_ind_data")
    ind_data <- get_ind_data(event, paste(ind_data_dir,
                                          sep = ""),
                             event_sums = event_sums)
    assign(name, ind_data)
    ind_data_list[[name]] = ind_data
  }
  for (i in 1:length(dec_events)) {
    print(dec_events[i])
    N_individual <- nrow(ind_data_list[[i]])
    Y_individual <- unlist(ind_data_list[[i]][["score"]])
    age_individual <- (unlist(ind_data_list[[i]][["age"]]) - age_mean) / age_sd
    ind_athlete_ids <- ind_data_list[[i]]$'athlete_id'
    pooled_ages <- c(age_individual, age_decathlon)
    knots <- quantile(pooled_ages, probs = seq(0.1, 0.9, by = 0.1))
    # For individual data
    age_basis_individual <- bs(age_individual, degree = 3, knots = knots, intercept = TRUE)
    age_basis_decathlon <- bs(age_decathlon, degree = 3, knots = knots, intercept = TRUE)
    # For predictions
    age_basis_pred <- predict(age_basis_individual, newx = age_pred)
    #simple cubic on individual data. no predictions.
    individual_spline_list <- list(N_decathlon = N_individual,
                                  N_pred = N_pred,
                                  A_decathlon = length(unique(ind_athlete_ids)),
                                  athlete_decathlon = ind_athlete_ids,
                                  Y_decathlon = Y_individual,
                                  age_basis_decathlon = age_basis_individual,
                                  age_basis_pred = age_basis_pred,  
                                  is_new_athlete = rep(1,N_pred), 
                                  athlete_pred = rep(1,N_pred))  
    ind_mod <- stan(paste0(stan_dir, "simple_spline.stan"),
                    data = individual_spline_list, chains=4)           
    mu <- (extract(ind_mod)$mu_alpha)
    beta_age <- apply(extract(ind_mod)$beta_age,
                      MARGIN = 2,
                      mean)
    sigma_decathlon <- mean(extract(ind_mod)$sigma_decathlon)
    beta_means <- apply(extract(ind_mod)$beta_age,
                        MARGIN = 2,
                        mean)
    beta_sds <- apply(extract(ind_mod)$beta_age,
                      MARGIN = 2,
                      sd)
    mu_mean <- mean(mu)
    mu_sd <- sd(mu)
    
    Y_decathlon <- decathlon_data[[dec_events[i]]]
    
    athlete_pred <- athlete_id # athlete id's
    is_new_athlete <- is_new_athlete 
    A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
    moment_matched_spline_list <- list(N_decathlon = N_decathlon,
                                      N_pred = N_pred,
                                      A_decathlon = A_decathlon,
                                      athlete_decathlon = athlete_decathlon,
                                      Y_decathlon = Y_decathlon,
                                      age_basis_decathlon = age_basis_decathlon,
                                      age_basis_pred = age_basis_pred, 
                                      is_new_athlete = is_new_athlete, 
                                      athlete_pred = athlete_id,
                                      beta_means = beta_means,
                                      beta_sds = beta_sds,
                                      mu_mean = mu_mean,
                                      mu_sd = mu_sd)  
    moment_matched_mod <- stan(paste0(stan_dir, "simple_moment_matched_spline.stan"),
                               data = moment_matched_spline_list,
                               chains=4,
                               iter = iter)  
    event = dec_events[i]
    event_mean = event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd = event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    post_sim <- (extract(moment_matched_mod)$Y_pred * event_sd) + event_mean
    post_pred <- apply(post_sim, 2, mean)
    mu <- mean(extract(moment_matched_mod)$mu_alpha)
    beta_age <- apply(extract(moment_matched_mod)$beta_age,
                      MARGIN = 2,
                      mean)
    sigma_decathlon <- mean(extract(moment_matched_mod)$sigma_decathlon)
    sim <- list(mu = mu,
               beta_age = beta_age,
               sigma_decathlon = sigma_decathlon,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
  }
  
  sim_events <- sim_to_df(sims_list, age_vec, iter)
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}

get_comp_spline_sim <- function(age_vec, 
                               athlete_id,
                               is_new_athlete,
                               decathlon_data, event_sums,
                               stan_dir, iter = 2000,
                               return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  
  N_decathlon <- nrow(decathlon_data)
  age_pred <- (age_vec- age_mean) / age_sd
  N_pred <- length(age_pred)
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  knots <- quantile(dec_age_vec, probs = seq(0.1, 0.9, by = 0.1))

  age_basis_decathlon <- bs(age_decathlon, degree = 3, knots = knots, intercept = TRUE)
  age_basis_pred <- predict(age_basis_decathlon, newx = age_pred)
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  
  event = dec_events[1]
  Y_decathlon <- decathlon_data[[event]]
  data_list <- list(
    N_decathlon  = N_decathlon,
    age_basis_pred = age_basis_pred,
    N_pred = N_pred,
    A_decathlon = A_decathlon,
    athlete_decathlon = athlete_decathlon,
    Y_decathlon = Y_decathlon,
    age_basis_decathlon = age_basis_decathlon,
    athlete_pred = athlete_pred,
    is_new_athlete = is_new_athlete
  )
  mod <- stan(paste0(stan_dir, "simple_spline.stan"),
              data = data_list, chains=4,
              iter = iter)
  post_sim <- (extract(mod)$Y_pred )
  post_pred <- apply(post_sim, 2, mean)
  mu <- (extract(mod)$mu_alpha)
  alpha_effect <- extract(mod)$alpha_effect
  beta_age <- extract(mod)$beta_age
  sigma <- (extract(mod)$sigma_decathlon)
  sim <- list(mu = mu,
             beta_age = beta_age,
              sigma = sigma,
              post_sim = post_sim,
              post_pred = post_pred)
  sims_list[[paste0(event, "_sims")]] <- sim
  prev_events <- data.frame(post_sim)
  prev_events <- prev_events %>%
    pivot_longer(cols = 1:length(age_vec), names_to = "age",
                 values_to = event) %>%
    mutate(athlete = as.integer(sub(".", "", age)),
           age = rep(age_vec, iter/2*4),
           age = as.numeric(age),
           row_num = row_number()) %>%
    relocate(athlete, row_num)
  for (i in 2:length(dec_events)) {
    event = dec_events[i]
    print(event)
    N_prev_events <- i-1
    Y_decathlon <- (decathlon_data[[dec_events[i]]])
    Y_prev_events <- decathlon_data[dec_events[1:N_prev_events]]
    data_list_compositional_spline <- list(
      N_decathlon  = N_decathlon,
      N_pred = N_pred,
      N_prev_events = dim(Y_prev_events)[2],
      A_decathlon = A_decathlon,
      athlete_decathlon = athlete_decathlon,
      Y_decathlon = Y_decathlon,
      Y_prev_events = Y_prev_events,
      age_basis_decathlon = age_basis_decathlon,
      is_new_athlete = is_new_athlete,
      athlete_pred = athlete_pred
    )
    
    model <- stan(paste0(stan_dir, "compositional_spline.stan"),
                  data = data_list_compositional_spline, chains=4,
                  iter = iter)
    
    
    # extract parameters
    mu <- extract(model)$mu_alpha
    alpha_effect <- extract(model)$alpha_effect
    beta_age <- extract(model)$beta_age
    betaY <- extract(model)$betaY
    sigma <- extract(model)$sigma_decathlon
    sim <- list(mu = mu,
                alpha_effect = alpha_effect,
                beta_age = beta_age,
                sigma = sigma,
                betaY = betaY,
                post_sim = post_sim,
                post_pred = post_pred)
    sims_list[[paste0(event, "_sims")]] <- sim
    # make predictions
    dec_event = dec_events[i]
    df <- as.data.frame(matrix(nrow = length(mu),
                               ncol = length(age_pred)))
    
    for (k in 1:length(age_pred)) {
      pred_age = age_pred[k]
      mean_vec = alpha_effect[,k] + 
        (beta_age %*% age_basis_pred[k,])
      if (i == 2){
        Y_effect <-
          betaY * prev_events %>%
          filter(athlete == k) %>%
          select(dec_events[i-1])  %>%
          pull()
      } else{
        Y_effect <- 0
        for (j in 1:dim(betaY)[2]) {
          Y_effect <- Y_effect +  
            (betaY[,j] * prev_events %>%
               filter(athlete == k) %>%
               select(dec_events[j]) %>%
               pull()) 
        }
      }
      mean_vec <- mean_vec + drop(Y_effect)
      age_sim <- rnorm(length(mean_vec), mean_vec, sigma)
      df[,k] <- age_sim
      colnames(df)[k] <- k
    }
    df <- df %>%
      pivot_longer(cols = 1:length(age_pred),
                   names_to = "athlete",
                   values_to = dec_event)  %>%
      mutate(athlete = as.integer(athlete),
             row_num = row_number())
    prev_events <- left_join(prev_events, df)
    
  }
  
  sim_events <- prev_events
  
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_events[event] <- (prev_events[event] *event_sd ) + event_mean
    
  }
  
  sim_events <- sim_events %>%
    add_real_points2()
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}




get_comp_moment_spline_sim <- function(age_vec, 
                                      athlete_id,
                                      is_new_athlete,
                                      decathlon_data, event_sums,
                                      stan_dir, ind_data_dir,
                                      iter = 2000,
                                      return_all = F){
  sims_list <- list()
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  dec_age_vec <- c(decathlon_data$age)
  age_mean <- mean(dec_age_vec)
  age_sd <- sd(dec_age_vec)
  N_decathlon <- nrow(decathlon_data)
  age_pred <- (age_vec- age_mean) / age_sd
  N_pred <- length(age_pred)
  athlete_decathlon <- decathlon_data$athlete_id
  age_decathlon <- (decathlon_data$age - age_mean) / age_sd
  
  ind_data_list <- list()
  for (event in dec_events){
    name = paste0(event, "_ind_data")
    ind_data <- get_ind_data(event, paste(ind_data_dir,
                                          sep = ""),
                             event_sums = event_sums)
    assign(name, ind_data)
    ind_data_list[[name]] = ind_data
  }
  
  N_individual <- nrow(ind_data_list[[1]])
  Y_individual <- unlist(ind_data_list[[1]][["score"]])
  age_individual <- (unlist(ind_data_list[[1]][["age"]]) - age_mean) / age_sd
  ind_athlete_ids <- ind_data_list[[1]]$'athlete_id'
  pooled_ages <- c(age_individual, age_decathlon)
  knots <- quantile(pooled_ages, probs = seq(0.1, 0.9, by = 0.1))
  # For individual data
  age_basis_individual <- bs(age_individual, degree = 3, knots = knots, intercept = TRUE)
  age_basis_decathlon <- bs(age_decathlon, degree = 3, knots = knots, intercept = TRUE)
  # For predictions
  age_basis_pred <- predict(age_basis_individual, newx = age_pred)
  #simple cubic on individual data. no predictions.
  individual_spline_list <- list(N_decathlon = N_individual,
                                 N_pred = N_pred,
                                 A_decathlon = length(unique(ind_athlete_ids)),
                                 athlete_decathlon = ind_athlete_ids,
                                 Y_decathlon = Y_individual,
                                 age_basis_decathlon = age_basis_individual,
                                 age_basis_pred = age_basis_pred,  
                                 is_new_athlete = rep(1,N_pred), 
                                 athlete_pred = rep(1,N_pred))  
  ind_mod <- stan(paste0(stan_dir, "simple_spline.stan"),
                  data = individual_spline_list, chains=4)           
  mu <- (extract(ind_mod)$mu_alpha)
  beta_age <- apply(extract(ind_mod)$beta_age,
                    MARGIN = 2,
                    mean)
  sigma_decathlon <- mean(extract(ind_mod)$sigma_decathlon)
  beta_means <- apply(extract(ind_mod)$beta_age,
                      MARGIN = 2,
                      mean)
  beta_sds <- apply(extract(ind_mod)$beta_age,
                    MARGIN = 2,
                    sd)
  mu_mean <- mean(mu)
  mu_sd <- sd(mu)
  event = dec_events[1]
  Y_decathlon <- decathlon_data[[dec_events[1]]]
  
  athlete_pred <- athlete_id # athlete id's
  is_new_athlete <- is_new_athlete 
  A_decathlon <- max(c(decathlon_data$athlete_id, athlete_pred))
  moment_matched_spline_list <- list(N_decathlon = N_decathlon,
                                     N_pred = N_pred,
                                     A_decathlon = A_decathlon,
                                     athlete_decathlon = athlete_decathlon,
                                     Y_decathlon = Y_decathlon,
                                     age_basis_decathlon = age_basis_decathlon,
                                     age_basis_pred = age_basis_pred, 
                                     is_new_athlete = is_new_athlete, 
                                     athlete_pred = athlete_id,
                                     beta_means = beta_means,
                                     beta_sds = beta_sds,
                                     mu_mean = mu_mean,
                                     mu_sd = mu_sd)  
  moment_matched_mod <- stan(paste0(stan_dir, "simple_moment_matched_spline.stan"),
                             data = moment_matched_spline_list,
                             chains=4,
                             iter = iter)  
  post_sim <- (extract(moment_matched_mod)$Y_pred )
  post_pred <- apply(post_sim, 2, mean)
  mu <- (extract(moment_matched_mod)$mu_alpha)
  alpha_effect <- extract(moment_matched_mod)$alpha_effect
  beta_age <- extract(moment_matched_mod)$beta_age
  sigma <- (extract(moment_matched_mod)$sigma_decathlon)
  sim <- list(mu = mu,
              alpha_effect = alpha_effect,
              beta_age = beta_age,
              sigma = sigma,
              post_sim = post_sim,
              post_pred = post_pred)
  sims_list[[paste0(event, "_sims")]] <- sim
  prev_events <- data.frame(post_sim)
  prev_events <- prev_events %>%
    pivot_longer(cols = 1:length(age_vec), names_to = "age",
                 values_to = event) %>%
    mutate(athlete = as.integer(sub(".", "", age)),
           age = rep(age_vec, iter/2*4),
           age = as.numeric(age),
           row_num = row_number()) %>%
    relocate(athlete, row_num)
  for (i in 2:10) {
    event = dec_events[i]
    print(event)
    N_individual <- nrow(ind_data_list[[i]])
    Y_individual <- unlist(ind_data_list[[i]][["score"]])
    age_individual <- (unlist(ind_data_list[[i]][["age"]]) - age_mean) / age_sd
    ind_athlete_ids <- ind_data_list[[i]]$'athlete_id'
    pooled_ages <- c(age_individual, age_decathlon)
    knots <- quantile(pooled_ages, probs = seq(0.1, 0.9, by = 0.1))
    # For individual data
    age_basis_individual <- bs(age_individual, degree = 3, knots = knots, intercept = TRUE)
    age_basis_decathlon <- bs(age_decathlon, degree = 3, knots = knots, intercept = TRUE)
    # For predictions
    age_basis_pred <- predict(age_basis_individual, newx = age_pred)
    #simple cubic on individual data. no predictions.
    individual_spline_list <- list(N_decathlon = N_individual,
                                   N_pred = N_pred,
                                   A_decathlon = length(unique(ind_athlete_ids)),
                                   athlete_decathlon = ind_athlete_ids,
                                   Y_decathlon = Y_individual,
                                   age_basis_decathlon = age_basis_individual,
                                   age_basis_pred = age_basis_pred,  
                                   is_new_athlete = rep(1,N_pred), 
                                   athlete_pred = rep(1,N_pred))  
    ind_mod <- stan(paste0(stan_dir, "simple_spline.stan"),
                    data = individual_spline_list, chains=4)           
    mu <- (extract(ind_mod)$mu_alpha)
    beta_age <- apply(extract(ind_mod)$beta_age,
                      MARGIN = 2,
                      mean)
    sigma_decathlon <- mean(extract(ind_mod)$sigma_decathlon)
    beta_means <- apply(extract(ind_mod)$beta_age,
                        MARGIN = 2,
                        mean)
    beta_sds <- apply(extract(ind_mod)$beta_age,
                      MARGIN = 2,
                      sd)
    mu_mean <- mean(mu)
    mu_sd <- sd(mu)
    
    
    
    Y_decathlon <- decathlon_data[[dec_events[i]]]
    
    N_prev_events <- i-1
    Y_prev_events <- decathlon_data[dec_events[1:N_prev_events]]
    
    moment_matched_spline_list <- list(N_decathlon = N_decathlon,
                                      N_pred = N_pred,
                                      A_decathlon = A_decathlon,
                                      athlete_decathlon = athlete_decathlon,
                                      Y_decathlon = Y_decathlon,
                                      age_basis_decathlon = age_basis_decathlon,
                                      age_pred = age_pred,  
                                      N_prev_events = N_prev_events,
                                      Y_prev_events = Y_prev_events,
                                      is_new_athlete = is_new_athlete, 
                                      athlete_pred = athlete_pred,
                                      beta_means = beta_means,
                                      mu_mean = mu_mean)  
    
    
    moment_matched_mod <- stan(paste0(stan_dir, "compositional_moment_matched_spline.stan"),
                               data = moment_matched_spline_list, chains=4,
                               iter= iter)
    
    # extract parameters
    mu <- extract(moment_matched_mod)$mu_alpha
    alpha_effect <-extract(moment_matched_mod)$alpha_effect
    beta_age <- extract(moment_matched_mod)$beta_age
    betaY <- extract(moment_matched_mod)$betaY
    sigma <- extract(moment_matched_mod)$sigma_decathlon
    sim <- list(mu = mu,
                alpha_effect = alpha_effect,
                beta_age = beta_age,
                sigma = sigma,
                betaY = betaY)
    sims_list[[paste0(event, "_sims")]] <- sim
    # make predictions
    dec_event = dec_events[i]
    df <- as.data.frame(matrix(nrow =  length(mu),
                               ncol = length(age_pred)))
    
    for (k in 1:length(age_pred)) {
      pred_age = age_pred[k]
      mean_vec = alpha_effect[,k] + 
        (beta_age %*% age_basis_pred[k,])
      if (i == 2){
        Y_effect <-
          betaY * prev_events %>%
          filter(athlete == k) %>%
          select(dec_events[i-1])  %>%
          pull()
      } else{
        Y_effect <- 0
        for (j in 1:dim(betaY)[2]) {
          Y_effect <- Y_effect +  
            (betaY[,j] * prev_events %>%
               filter(athlete == k) %>%
               select(dec_events[j]) %>%
               pull()) 
        }
      }
      mean_vec <- mean_vec + drop(Y_effect)
      age_sim <- rnorm(length(mean_vec), mean_vec, sigma)
      df[,k] <- age_sim
      colnames(df)[k] <- k
    }
    df <- df %>%
      pivot_longer(cols = 1:length(age_pred),
                   names_to = "athlete",
                   values_to = dec_event)  %>%
      mutate(athlete = as.integer(athlete),
             row_num = row_number())
    prev_events <- left_join(prev_events, df)
    
  }
  
  sim_events <- prev_events
  
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_events[event] <- (sim_events[event] *event_sd ) + event_mean
    
  }
  
  sim_events <- sim_events %>%
    add_real_points2()
  if (return_all) {
    results <- list(
      sims_list = sims_list,
      sim_events = sim_events)
  } else{
    results <- list(
      # sims_list = sims_list,
      sim_events = sim_events)
  }
  return(results)
}




# other functions ---------------------------------------------------------

make_plots <- function(var, sim, dec_data, model_name){
  dec_events <- c("hundred_m", "long_jump", "shot_put",
                  "high_jump", "four_hundred_m", "hurdles",
                  "discus", "pole_vault", "javelin",
                  "fifteen_hundred_m")
  lb <- min(dec_data[[var]]) - (.5*sd(dec_data[[var]]))
  ub <- max(dec_data[[var]]) + (.5*sd(dec_data[[var]]))
  df <- sim$sim_events %>%
    select(-index) %>% 
    rename(points = calc_point) %>%
    mutate(model = model_name)
  graph_df <- rbind(dec_data %>%
                      mutate(model = "original") %>%
                      select(c(dec_events, points)),
                    df)
  hist_plot <- ggplot(df, mapping = aes(x = .data[[var]])) +
    geom_histogram(color = "white",
                   aes(y = stat(count / sum(count)))) +
    # xlim(c(lb, ub)) + # have to change this depending on the y
    labs(y = "density",
         title = model_name) +
    theme_bw()
  og_hist_plot <- ggplot(online_data_filter, mapping = aes(x = .data[[var]])) +
    geom_histogram(color = "white",
                   aes(y = stat(count / sum(count)))) +
    # xlim(c(lb, ub)) +
    labs(y = "density", title = "true data") +
    theme_bw()
  ribbon_df <- df %>%
    group_by(age) %>%
    summarize(mean_var = mean(.data[[var]]),
              quant2.5 = quantile(.data[[var]], .025),
              quant97.5 = quantile(.data[[var]], .975),
              quant5 = quantile(.data[[var]], .05),
              quant95 = quantile(.data[[var]], .95))
  
  
  ribbon_plot <- ggplot(data = ribbon_df %>%
                          filter(age <= 40)
                        , mapping = aes(x = age)) +
    geom_point(data = dec_data %>%
                 filter(age <= 40),
               mapping = aes(x = age, y = .data[[var]]),
               alpha = .15, color = "tomato") +
    geom_ribbon(mapping = aes(ymin = quant2.5,
                              ymax = quant97.5),
                alpha = .3,
                color = "#440154",
                fill = "#440154") +
    geom_ribbon(mapping = aes(ymin = quant5,
                              ymax = quant95),
                alpha = .2,
                color = "#3b528b",
                fill = "#3b528b")  +
    geom_line(mapping = aes(y = mean_var)) +
    labs(title = model_name) +
    theme_bw()
  plot_list <- list(hist_plot = hist_plot,
                    ribbon_plot = ribbon_plot,
                    og_hist_plot = og_hist_plot)
  return(plot_list)
}




