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

# same model as get_comp_cubic_sim. does not return the simulated age curves, but the model parameters, so we can remove the simulation aspect.
get_comp_cubic_sim_athlete_intercepts <- function(age_vec, 
                                                  athlete_id,
                                                  is_new_athlete,
                                                  decathlon_data,
                                                  stan_dir, iter = 2000){
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
  mu <- (extract(mod)$mu_alpha)
  alpha_decathlon <- extract(mod)$alpha_decathlon
  beta1 <- (extract(mod)$beta1)
  beta2 <- (extract(mod)$beta2)
  beta3 <- (extract(mod)$beta3)
  sigma <- (extract(mod)$sigma_decathlon)
  sigma_mu <- extract(mod)$sigma_alpha_decathlon
  sim <- list(mu = mu,
              beta1 = beta1,
              beta2 = beta2,
              beta3 = beta3,
              sigma = sigma,
              sigma_mu = sigma_mu,
              alpha_decathlon = alpha_decathlon)
  sims_list[[paste0(event, "_sims")]] <- sim
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
    alpha_decathlon <- extract(model)$alpha_decathlon
    beta1 <- extract(model)$beta1
    beta2 <- extract(model)$beta2
    beta3 <- extract(model)$beta3
    betaY <- extract(model)$betaY
    sigma <- extract(model)$sigma_decathlon
    sigma_mu <- extract(model)$sigma_athlete_decathlon # note: compositional model has athlete instead of alpha in the parameter name, but is same as sigma_alpha_decathlon from the simple model
    sim <- list(mu = mu,
                alpha_decathlon = alpha_decathlon,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                sigma = sigma,
                sigma_mu = sigma_mu,
                betaY = betaY)
    sims_list[[paste0(event, "_sims")]] <- sim
  }
  results <- list(sims_list = sims_list)
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




############################################################################################################################################################################################################################
#function to calculate points from a pred_df. returns same df with calc_points column
# add_pred_points <- function(pred_df){
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   track_events <- c("hundred_m",
#                     "four_hundred_m", 
#                     "hurdles", 
#                     "fifteen_hundred_m",
#                     "hundred_m_pred",
#                     "four_hundred_m_pred", 
#                     "hurdles_pred", 
#                     "fifteen_hundred_m_pred")
#   pred_df["index"] <- 1:nrow(pred_df)
#   pred_df <- pred_df  %>%
#     pivot_longer(cols = paste(y_vars, "_pred", sep = ""), 
#                  names_to = "event", values_to = "P") %>%
#     mutate(event_type = case_when(event %in% c(track_events) ~ "track",
#                                   .default = "field"),
#            A = case_when(event == "hundred_m_pred" ~ 25.4347,
#                          event == "long_jump_pred" ~ 0.14354,
#                          event == "shot_put_pred" ~ 51.39,
#                          event == "high_jump_pred" ~ 0.8465,
#                          event == "four_hundred_m_pred" ~ 1.53775,
#                          event == "hurdles_pred" ~ 5.74352,
#                          event == "discus_pred" ~ 12.91,
#                          event == "pole_vault_pred" ~ 0.2797,
#                          event == "javelin_pred" ~ 10.14,
#                          event == "fifteen_hundred_m_pred" ~ 0.03768),
#            B = case_when(event == "hundred_m_pred" ~ 18,
#                          event == "long_jump_pred" ~ 220,
#                          event == "shot_put_pred" ~ 1.5,
#                          event == "high_jump_pred" ~ 75,
#                          event == "four_hundred_m_pred" ~ 82,
#                          event == "hurdles_pred" ~ 28.5,
#                          event == "discus_pred" ~ 4,
#                          event == "pole_vault_pred" ~ 100,
#                          event == "javelin_pred" ~ 7,
#                          event == "fifteen_hundred_m_pred" ~ 480),
#            C = case_when(event == "hundred_m_pred" ~ 1.81,
#                          event == "long_jump_pred" ~ 1.4,
#                          event == "shot_put_pred" ~ 1.05,
#                          event == "high_jump_pred" ~ 1.42,
#                          event == "four_hundred_m_pred" ~ 1.81,
#                          event == "hurdles_pred" ~ 1.92,
#                          event == "discus_pred" ~ 1.1,
#                          event == "pole_vault_pred" ~ 1.35,
#                          event == "javelin_pred" ~ 1.08,
#                          event == "fifteen_hundred_m_pred" ~ 1.85),
#            calc_points = case_when(event_type == "track" ~ floor(A * ((B - P)^C)),
#                                    event_type == "field" ~ floor(A * ((P - B)^C)))) %>%  
#     select(index, event, calc_points, age, P) %>%
#     group_by(index, age) %>%
#     mutate(points_pred = sum(calc_points)) %>% 
#     select(-calc_points) %>%
#     pivot_wider(names_from = event, values_from = P)
#   return(pred_df)
# }


# add_real_points <- function(orig_data) {
#   data <- orig_data %>%
#     mutate(index = row_number(),
#            long_jump = long_jump * 100,
#            high_jump = high_jump*100,
#            pole_vault = pole_vault * 100) 
#   data_long <- data %>%
#     pivot_longer(cols =  c("hundred_m", 
#                            "long_jump",
#                            "shot_put",
#                            "high_jump",
#                            "four_hundred_m", 
#                            "hurdles",
#                            "discus",
#                            "pole_vault",
#                            "javelin",
#                            "fifteen_hundred_m") , names_to = "event", values_to = "P") %>%
#     mutate(event_type = case_when(event %in% c("hundred_m",
#                                                "four_hundred_m", 
#                                                "hurdles", 
#                                                "fifteen_hundred_m") ~ "track",
#                                   .default = "field"),
#            A = case_when(event == "hundred_m" ~ 25.4347,
#                          event == "long_jump" ~ 0.14354,
#                          event == "shot_put" ~ 51.39,
#                          event == "high_jump" ~ 0.8465,
#                          event == "four_hundred_m" ~ 1.53775,
#                          event == "hurdles" ~ 5.74352,
#                          event == "discus" ~ 12.91,
#                          event == "pole_vault" ~ 0.2797,
#                          event == "javelin" ~ 10.14,
#                          event == "fifteen_hundred_m" ~ 0.03768),
#            B = case_when(event == "hundred_m" ~ 18,
#                          event == "long_jump" ~ 220,
#                          event == "shot_put" ~ 1.5,
#                          event == "high_jump" ~ 75,
#                          event == "four_hundred_m" ~ 82,
#                          event == "hurdles" ~ 28.5,
#                          event == "discus" ~ 4,
#                          event == "pole_vault" ~ 100,
#                          event == "javelin" ~ 7,
#                          event == "fifteen_hundred_m" ~ 480),
#            C = case_when(event == "hundred_m" ~ 1.81,
#                          event == "long_jump" ~ 1.4,
#                          event == "shot_put" ~ 1.05,
#                          event == "high_jump" ~ 1.42,
#                          event == "four_hundred_m" ~ 1.81,
#                          event == "hurdles" ~ 1.92,
#                          event == "discus" ~ 1.1,
#                          event == "pole_vault" ~ 1.35,
#                          event == "javelin" ~ 1.08,
#                          event == "fifteen_hundred_m" ~ 1.85),
#            calc_points = case_when(event_type == "track" ~ floor(A * ((B - P)^C)),
#                                    event_type == "field" ~ floor(A * ((P - B)^C)))) %>%
#     group_by(index)  %>%
#     summarize(points = mean(points), calc_point = sum(calc_points))
#   
#   data[,"calc_point"] = data_long[,"calc_point"]
#   return(data)
# }


################################################################################
# functions to generate predictions given an age vector.
################################################################################
# use linear model without dependence between events. use age to predict hundred , age to predict long jump:

# gen_pred_lr_simple <- function(data, age_vec){
#   x_var <- c("age", "age2")
#   mult_lin_reg_mod <- 
#     rstan::stan_model(file = "/Users/paulnguyen/school/wisconsin/BART/research_repo/decathlon/stan/qr.stan")
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   data$age2 <- data$age^2
#   data <- as.data.frame((data))
#   N_samples <- 4000
#   list_of_fits <- list()
#   pred_df <- data.frame(age = age_vec) 
#   
#   for (y_var in y_vars) {
#     new_data <- as.matrix(data.frame(age = age_vec, age2 = age_vec^2))
#     
#     args <- list(N = nrow(data), K = length(x_var), x = as.matrix(data[,x_var]),
#                  y = data[,y_var], N_new = length(age_vec), 
#                  x_new = new_data)
#     fit <- 
#       rstan::sampling(object = mult_lin_reg_mod,
#                       data = args)
#     
#     list_of_fits <- list_of_fits %>%
#       list.append(c(fit, y_var))
#     
#     pred_y_samples <- rstan::extract(object = fit, 
#                                      pars = c("new_pred_y"))[[1]]
#     
#     
#     pred_y_mean <- apply(pred_y_samples, MARGIN = 2, FUN = mean)
# 
#  
#     pred_df[, paste(y_var , "_pred", sep = "")] <- pred_y_mean 
#   }
#   return(pred_df)
# }
# 
# # use linear model with dependece between events. use age to predict hundred m, age + hundred m to predict long jump, ... 
# gen_pred_lr_dep <- function(data, age_vec){
#   x_var <- c("age", "age2")
#   mult_lin_reg_mod <- 
#     rstan::stan_model(file = "/Users/paulnguyen/school/wisconsin/BART/research_repo/decathlon/stan/qr.stan")
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   data$age2 <- data$age^2
#   data <- as.data.frame((data))
#   N_samples <- 4000
#   list_of_fits <- list()
#   pred_df <- data.frame(age = age_vec) 
#   
#   
#   for (i in 1:length(y_vars)) {
#     y_var = y_vars[i]
#     print(y_var)
#     if (i == 1) { # use only age and age2 to predict hundred_m
#       x_var = x_var
#       new_data <- as.matrix(data.frame(age = age_vec, age2 = age_vec^2))
#     } else{
#       x_var = c(x_var, y_vars[i-1])
#     }
#     args <- list(N = nrow(data), K = length(x_var), x = as.matrix(data[,x_var]),
#                  y = data[,y_var], N_new = length(age_vec), 
#                  x_new = new_data)
#     fit <- 
#       rstan::sampling(object = mult_lin_reg_mod,
#                       data = args)
#     list_of_fits <- list_of_fits %>%
#       list.append(c(fit, y_var))
#     pred_y_samples <- rstan::extract(object = fit, 
#                                      pars = c("new_pred_y"))[[1]]
#     
#     pred_y_mean <- apply(pred_y_samples, MARGIN = 2, FUN = mean)
#     
#     pred_df[, paste(y_var , "_pred", sep = "")] <- pred_y_mean 
#     if (i < length(y_vars)) {
#       new_data <- cbind(new_data, pred_df[,i + 1])
#     }
#     
#     
#   }
#   return(pred_df)
# }
# 
# # use BART model without dependence between events. use age to predict hundred m, age to predict long jump, etc.
# simple_bart_pred <- function(data, y_var, age_vec){
#   mod_data <- data.frame(age = data$age)
#   x1_mod <- wbart(x.train = mod_data,
#                   y.train = (data[[y_var]]),
#                   x.test = data.frame(x1 = age_vec))
#   pred <- x1_mod$yhat.test.mean 
#   return(pred)
# }
# 
# gen_pred_bart_simple <- function(data, age_vec){
#   pred_df <- data.frame(age = age_vec)
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   for (i in 1:length(y_vars)) {
#     y_var = y_vars[i]
#     pred_df[,paste(y_var, "_pred", sep = "")] = simple_bart_pred(data, y_var, age_vec)
#   }
#   return(pred_df)
# }
# 
# 
# gen_pred_bart_dep <- function(data, age_vec){
#   pred_data <- data.frame(age = age_vec)
#   # hundred m
#   data_hundred_m <- data.frame(age = data$age) # could be age / whatever covariates later. 
#   x1_mod <- wbart(x.train = data_hundred_m,
#                   y.train = data$hundred_m,
#                   x.test = data.frame(x1 = age_vec))
#   pred_data$hundred_m_pred <- x1_mod$yhat.test.mean 
#   # long jump
#   data_long_jump <- data.frame(data[,c("age", "hundred_m")])
#   x2_mod <- wbart(x.train = data_long_jump,
#                   y.train = data$long_jump,
#                   x.test = pred_data)
#   
#   pred_data$long_jump_pred <- x2_mod$yhat.test.mean 
#   #shot put
#   data_shot_put <-  data.frame(age = data$age,
#                                hundred_m = data$hundred_m,
#                                long_jump = data$long_jump)
#   x3_mod <- wbart(x.train = data_shot_put,
#                   y.train = data$shot_put,
#                   x.test = pred_data)
#   
#   pred_data$shot_put_pred <- x3_mod$yhat.test.mean 
#   # high jump
#   data_high_jump <- data.frame(data_shot_put, data$shot_put)
#   x4_mod <- wbart(x.train = data_high_jump,
#                   y.train = data$high_jump,
#                   x.test = pred_data)
#   pred_data$high_jump_pred <- x4_mod$yhat.test.mean 
#   # four hundred meter
#   data_four_hundred_m <- data.frame(data_high_jump, data$high_jump)
#   x5_mod <- wbart(x.train = data_four_hundred_m,
#                   y.train = data$four_hundred_m,
#                   x.test = pred_data)
#   pred_data$four_hundred_m_pred <- x5_mod$yhat.test.mean 
#   # hurdles
#   data_hurdles <- data.frame(data_four_hundred_m, data$four_hundred_m)
#   x6_mod <- wbart(x.train = data_hurdles,
#                   y.train = data$hurdles,
#                   x.test = pred_data)
#   pred_data$hurdles_pred <- x6_mod$yhat.test.mean 
#   # discus
#   data_discus <- data.frame(data_hurdles, data$hurdles)
#   x7_mod <- wbart(x.train = data_discus,
#                   y.train = data$discus,
#                   x.test = pred_data)
#   pred_data$discus_pred <- x7_mod$yhat.test.mean 
#   # pole vault
#   data_pole_vault <- data.frame(data_discus, data$discus)
#   x8_mod <- wbart(x.train = data_pole_vault,
#                   y.train = data$pole_vault,
#                   x.test = pred_data)
#   pred_data$pole_vault_pred <- x8_mod$yhat.test.mean 
#   # javelin
#   data_javelin <- data.frame(data_pole_vault, data$pole_vault)
#   x9_mod <- wbart(x.train = data_javelin,
#                   y.train = data$javelin,
#                   x.test = pred_data)
#   pred_data$javelin_pred <- x9_mod$yhat.test.mean 
#   # fifteen hundred meter
#   data_fifteen_hundred_m <- data.frame(data_javelin, data$javelin)
#   x10_mod <- wbart(x.train = data_fifteen_hundred_m,
#                    y.train = data$fifteen_hundred_m,
#                    x.test = pred_data)
#   pred_data$fifteen_hundred_m_pred <- x10_mod$yhat.test.mean 
#   return(pred_data)
# }
# 
# ################################################################################
# # functions to generate samples of data. make predictions, then add noise. 
# ################################################################################
# 
# # function to generate samples of decathlon outcomes given training data and age vector using linear model without event dependence. use linear model to predict hundrem m given age. add noise to generate hundred m sample. use only age vector to repeat for long jump and other events. 
# 
# gen_sample_lr_simple <- function(data, age_vec){
#   x_var <- c("age", "age2")
#   mult_lin_reg_mod <- 
#     rstan::stan_model(file = "/Users/paulnguyen/school/wisconsin/BART/research_repo/decathlon/stan/qr.stan")
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   data$age2 <- data$age^2
#   data <- as.data.frame((data))
#   N_samples <- 4000
#   list_of_fits <- list()
#   sample_df <- data.frame(age = age_vec) 
#   for (y_var in y_vars) {
#     print(y_var)
#     new_data <- as.matrix(data.frame(age = age_vec, age2 = age_vec^2))
#     
#     args <- list(N = nrow(data), K = length(x_var), x = as.matrix(data[,x_var]),
#                  y = data[,y_var], N_new = length(age_vec), 
#                  x_new = new_data)
#     fit <- 
#       rstan::sampling(object = mult_lin_reg_mod,
#                       data = args)
#     list_of_fits <- list_of_fits %>%
#       list.append(c(fit, y_var))
#     
#     pred_y_samples <- rstan::extract(object = fit, 
#                                      pars = c("new_pred_y"))[[1]]
#     sigma_samples <- rstan::extract(object = fit,
#                                     pars = c("sigma"))[[1]]
#     
#     
#     pred_y_mean <- apply(pred_y_samples, MARGIN = 2, FUN = mean)
#     sigma_mean <- mean(sigma_samples)
#     sample_df[, paste(y_var , "_pred", sep = "")] <- pred_y_mean + 
#       rnorm(length(age_vec), 0, 1) * sigma_mean
#   }
#   return(sample_df)
# }
# 
# # function to generate samples of decathlon outcomes given training data and age vector using linear model with dependence. use linear model to predict hundred m given age. add noise to generate hundred m sample. then use age vector and hundrem m sample to predict long jump. add noise to make long jump sample. repeat.
# gen_sample_lr_dep <- function(data, age_vec){
#   x_var <- c("age", "age2")
#   mult_lin_reg_mod <- 
#     rstan::stan_model(file = "/Users/paulnguyen/school/wisconsin/BART/research_repo/decathlon/stan/qr.stan")
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   data$age2 <- data$age^2
#   data <- as.data.frame((data))
#   N_samples <- 4000
#   list_of_fits <- list()
#   sample_df <- data.frame(age = age_vec) 
#   
#   
#   for (i in 1:length(y_vars)) {
#     y_var = y_vars[i]
#     print(y_var)
#     if (i == 1) { # use only age and age2 to predict hundred_m
#       x_var = x_var
#       new_data <- as.matrix(data.frame(age = age_vec, age2 = age_vec^2))
#     } else{
#       x_var = c(x_var, y_vars[i-1])
#     }
#     args <- list(N = nrow(data), K = length(x_var), x = as.matrix(data[,x_var]),
#                  y = data[,y_var], N_new = length(age_vec), 
#                  x_new = new_data)
#     fit <- 
#       rstan::sampling(object = mult_lin_reg_mod,
#                       data = args)
#     list_of_fits <- list_of_fits %>%
#       list.append(c(fit, y_var))
#     pred_y_samples <- rstan::extract(object = fit, 
#                                      pars = c("new_pred_y"))[[1]]
#     sigma_samples <- rstan::extract(object = fit,
#                                     pars = c("sigma"))[[1]]
#     
#     pred_y_mean <- apply(pred_y_samples, MARGIN = 2, FUN = mean)
#     sigma_mean <- mean(sigma_samples)
#     
#     sample_df[, paste(y_var , "_pred", sep = "")] <- pred_y_mean + 
#       rnorm(length(age_vec), 0, 1) * sigma_mean
#     if (i < length(y_vars)) {
#       new_data <- cbind(new_data, sample_df[,i + 1])
#     }
#     
#     
#   }
#   return(sample_df)
# }
# 
# 
# # use BART model without dependence between events. use age to predict hundred m, add noise to get hundred m sample. repeat for long jump, etc.
# simple_bart_sample <- function(data, y_var, age_vec){
#   mod_data <- data.frame(age = data$age)
#   x1_mod <- wbart(x.train = mod_data,
#                   y.train = (data[[y_var]]),
#                   x.test = data.frame(x1 = age_vec))
#   pred <- x1_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x1_mod$sigma))
#   return(pred)
# }
# 
# 
# gen_sample_bart_simple <- function(data, age_vec){
#   sample_df <- data.frame(age = age_vec)
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")   
#   for (i in 1:length(y_vars)) {
#     y_var = y_vars[i]
#     sample_df[,paste(y_var, "_pred", sep = "")] = simple_bart_sample(data, y_var, age_vec)
#   }
#   return(sample_df)
# }
# 
# # use BART model with dependence to make samples in the following procedure: use age to predict hundred m, add noise to get hundred m sample. use age and hundred m sample to predict long jump, etc.
# gen_sample_bart_dep <- function(data, age_vec) {
#   pred_data <- data.frame(age = age_vec)
#   # hundred m
#   data_hundred_m <- data.frame(age = data$age) # could be age / whatever covariates later. 
#   x1_mod <- wbart(x.train = data_hundred_m,
#                   y.train = data$hundred_m,
#                   x.test = data.frame(x1 = age_vec))
#   pred_data$hundred_m_pred <- x1_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x1_mod$sigma))
#   # long jump
#   data_long_jump <- data.frame(data[,c("age", "hundred_m")])
#   x2_mod <- wbart(x.train = data_long_jump,
#                   y.train = data$long_jump,
#                   x.test = pred_data)
#   
#   pred_data$long_jump_pred <- x2_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x2_mod$sigma))
#   #shot put
#   data_shot_put <-  data.frame(age = data$age,
#                                hundred_m = data$hundred_m,
#                                long_jump = data$long_jump)
#   x3_mod <- wbart(x.train = data_shot_put,
#                   y.train = data$shot_put,
#                   x.test = pred_data)
#   
#   pred_data$shot_put_pred <- x3_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x3_mod$sigma))
#   # high jump
#   data_high_jump <- data.frame(data_shot_put, data$shot_put)
#   x4_mod <- wbart(x.train = data_high_jump,
#                   y.train = data$high_jump,
#                   x.test = pred_data)
#   pred_data$high_jump_pred <- x4_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x4_mod$sigma))
#   # four hundred meter
#   data_four_hundred_m <- data.frame(data_high_jump, data$high_jump)
#   x5_mod <- wbart(x.train = data_four_hundred_m,
#                   y.train = data$four_hundred_m,
#                   x.test = pred_data)
#   pred_data$four_hundred_m_pred <- x5_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x5_mod$sigma))
#   # hurdles
#   data_hurdles <- data.frame(data_four_hundred_m, data$four_hundred_m)
#   x6_mod <- wbart(x.train = data_hurdles,
#                   y.train = data$hurdles,
#                   x.test = pred_data)
#   pred_data$hurdles_pred <- x6_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x6_mod$sigma))
#   # discus
#   data_discus <- data.frame(data_hurdles, data$hurdles)
#   x7_mod <- wbart(x.train = data_discus,
#                   y.train = data$discus,
#                   x.test = pred_data)
#   pred_data$discus_pred <- x7_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x7_mod$sigma))
#   # pole vault
#   data_pole_vault <- data.frame(data_discus, data$discus)
#   x8_mod <- wbart(x.train = data_pole_vault,
#                   y.train = data$pole_vault,
#                   x.test = pred_data)
#   pred_data$pole_vault_pred <- x8_mod$yhat.test.mean +
#     (rnorm(length(age_vec), 0 , 1) * mean(x8_mod$sigma))
#   # javelin
#   data_javelin <- data.frame(data_pole_vault, data$pole_vault)
#   x9_mod <- wbart(x.train = data_javelin,
#                   y.train = data$javelin,
#                   x.test = pred_data)
#   pred_data$javelin_pred <- x9_mod$yhat.test.mean +
#     (rnorm(age_vec, 0 , 1) * mean(x9_mod$sigma))
#   # fifteen hundred meter
#   data_fifteen_hundred_m <- data.frame(data_javelin, data$javelin)
#   x10_mod <- wbart(x.train = data_fifteen_hundred_m,
#                    y.train = data$fifteen_hundred_m,
#                    x.test = pred_data)
#   pred_data$fifteen_hundred_m_pred <- x10_mod$yhat.test.mean +
#     (rnorm(length(age_vec)) * mean(x10_mod$sigma))
#   return(pred_data)
# }
# 
# 
# gen_sample_brms <- function(data, age_vec) {
#   sample_df <- data.frame(age = age_vec)
#   y_vars <-  c("hundred_m", "long_jump", "shot_put", "high_jump",
#                "four_hundred_m", "hurdles", "discus",
#                "pole_vault", "javelin", "fifteen_hundred_m")  
#   data$age2 <- data$age^2
#   data$age3 <- data$age^3
#   data <- as.data.frame((data))
#   x_var <- c("age", "age2", "age3")
#   
#   prior <- c(set_prior(prior = 'normal(0,4)', class='b', coef='age'), 
#              set_prior(prior = 'normal(0,3)', class='b', coef='age2'),
#              set_prior(prior = 'normal(0,2)', class='b', coef='age3'),
#              # global slope belongs to a normal distribution centered around 0
#              set_prior(prior = 'normal(0,6)', class='Intercept', coef=''))
#   # global intercept
#   list_of_fits <- list()
#   sample_df <- data.frame(age = age_vec,
#                           age2 = age_vec^2,
#                           age3 = age_vec^3) 
#   
#   for (i in 1:length(y_vars)) {
#     y_var = y_vars[i]
#     print(y_var)
#     if (i == 1) { # use only age and age2 and age 3 to predict hundred_m
#       x_var = x_var
#     } else{
#       x_var = c(x_var, y_vars[i-1])
#     }
#     model_data <- data %>%
#       select(all_of(y_var),
#              all_of(x_var))
#     f <- as.formula(paste(paste(y_var), paste(x_var, collapse = " + "),
#                           sep = " ~ "))
#     score_model <- brm(f,
#                        data = data,
#                        family = gaussian(),
#                        prior = prior,
#                        iter = 10000)
#     prediction_df <- data.frame(predict(score_model, newdata = sample_df))
#     simulated_df <- data.frame( prediction_df[,1] + rnorm(nrow(prediction_df), 
#                                                           mean = 0,
#                                                           sd = prediction_df[,2])) 
#     colnames(simulated_df) <- y_var
#     sample_df <- cbind(sample_df, simulated_df)
#   }
#   return(sample_df)
# }
# 
# 
# ################################################################################
# ## functions to analyze results
# ################################################################################
# # function to bind performance data vs predicted data
# bind_df <- function(data, new_data){
#   data["index"] = 1:nrow(data)
#   merge_data <- left_join(data, new_data)
#   return(merge_data)
#   
# }
# 
# # function to calculate rmse from actual vs predicted performance data
# get_rmse <- function(data, new_data, y_var){
#   y_var <- sym(y_var)
#   y_var_pred <- sym(paste(y_var, "_pred", sep = ""))
#   rmse <- bind_df(data, new_data) %>%
#     select(!!y_var, !!y_var_pred) %>%
#     summarize(rmse = sqrt(mean((!!y_var - !!y_var_pred)^2))) %>% 
#     pull()
#   
#   return(rmse)
# }
# 
# # function to plot actual performance data vs predicted performance data
# compare_perf_plot <- function(data, new_data, y_var){
#   y_var <- sym(y_var)
#   y_var_pred <- sym(paste(y_var, "_pred", sep = ""))
#   new_df <- bind_df(data, new_data)
#   new_plot <- ggplot(data = new_df, 
#                      mapping = aes(x = !!y_var, y = !!y_var_pred)) + 
#     geom_point()
#   return(new_plot)
# }