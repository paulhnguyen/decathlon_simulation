# date: Oct 24, 2025. goal of this document is to analyze the intercepts simulation. use athlete intercepts to find z-scores of events
# assumes wd is scripts, and that we ran study/study_intercepts.R 
library(tidyverse)
library(cowplot)
library(ggrepel)
library(knitr)

study = "../study"
data_dir = "../data/"
script_dir = "../study/"
stan_dir = "../stan_mods/"


source(paste0(script_dir, "decathlon_funs.R"))

# sim <- readRDS("../../results/intercepts_sim.RData")

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
age_mean = mean(dec_data_standard$age)
age_sd = sd(dec_data_standard$age)
dec_data_standard2 <- dec_data_standard %>%
  mutate(age = (age- age_mean)/age_sd)


# we do not need the simulated events, can get those using betas later. focus on athlete intercepts.
# note: we need to order athlete_id's not with the order that they come in, but from 1:length(athlete_ids) because of the stan code: alpha_decathlon[athlete_pred[n]] where athlete_pred is unique(dec_standard$athlete_id). so alpha_decathlon is the intercepts for athletes 1,2,3,4,..., 1007. and not in the order of athlete_ids (athlete_pred)
athlete_ids <- 1:length(unique(dec_data_standard$athlete_id))

hundred_m <- sim$sims_list[[1]]
mu <- data.frame(hundred_m$mu) %>%
  mutate(iter = row_number())
sigma_mu <- data.frame(hundred_m$sigma_mu) %>%
  mutate(iter = row_number())
beta1 <- data.frame(hundred_m$beta1) %>%
  mutate(iter = row_number())
beta2 <- data.frame(hundred_m$beta2) %>%
  mutate(iter = row_number())
beta3 <- data.frame(hundred_m$beta3) %>%
  mutate(iter = row_number())


athlete_df <- data.frame(t(hundred_m$alpha_decathlon)) 
colnames(athlete_df) <- 1:4000
athlete_df <- athlete_df %>%
  mutate(athlete_id = athlete_ids) %>%
  select(athlete_id, everything()) %>%
  pivot_longer(cols = 2:4001,
               names_to = "iter",
               values_to = "athlete_mu") %>%
  mutate(iter = as.integer(iter))
athlete_df <- athlete_df %>%
  left_join(mu, by = "iter") %>%
  left_join(sigma_mu, by = "iter") %>%
  rename(mu = hundred_m.mu,
         sigma_mu = hundred_m.sigma_mu) %>%
  mutate(z_score = (-athlete_mu - mu) / sigma_mu, # need to put a negative on "track events" since better times are smaller
         quantile = pnorm(-athlete_mu, mu, sigma_mu)) 

## trying to see if the intercepts make sense..

  
selected_athlete <- athlete_df %>%
  group_by(athlete_id) %>%
  left_join(beta1) %>%
  left_join(beta2) %>%
  left_join(beta3) %>%
  summarize(mean_mu = mean(athlete_mu),
            mean_beta1 = mean(hundred_m.beta1),
            mean_beta2 = mean(hundred_m.beta2),
            mean_beta3 = mean(hundred_m.beta3)) %>%
  filter(athlete_id == 91)
ggplot(data = dec_data_standard2 %>%
         filter(athlete_id == 91),
       mapping = aes(x = age, y = hundred_m)) + 
  geom_point(alpha = .2) + 
  stat_function(fun = function(x) selected_athlete$mean_mu +
                  (x*selected_athlete$mean_beta1) + 
                  (x^2*selected_athlete$mean_beta2) + 
                  (x^3*selected_athlete$mean_beta3),
                color = "blue", size = 1) 


total_summary_df <- athlete_df %>%
  group_by(athlete_id) %>%
  summarize(mean_z = mean(z_score)) 
colnames(total_summary_df) <- c("athlete_id", paste0(dec_events[1]))


total_quantile_df <- athlete_df %>%
  group_by(athlete_id) %>%
  summarize(mean_q = mean(quantile))
colnames(total_quantile_df) <- c("athlete_id", paste0(dec_events[1]))
# repeat for all other events
for (i in 2:length(sim$sims_list)) {
  track_events <- c("hundred_m", 'four_hundred_m', 'hurdles', 'fifteen_hundred_m')
  df <- sim$sims_list[[i]]
  event <- dec_events[i]
  mu <- data.frame(df$mu) %>%
    mutate(iter = row_number())
  sigma_mu <- data.frame(df$sigma_mu) %>%
    mutate(iter = row_number())
  
  athlete_df <- data.frame(t(df$alpha_decathlon)) 
  colnames(athlete_df) <- 1:4000
  athlete_df <- athlete_df %>%
    mutate(athlete_id = athlete_ids) %>%
    select(athlete_id, everything()) %>%
    pivot_longer(cols = 2:4001,
                 names_to = "iter",
                 values_to = "athlete_mu") %>%
    mutate(iter = as.integer(iter))
  if (event %in% track_events) {
    # lower scores better for track events
    athlete_df <- athlete_df %>%
      left_join(mu) %>%
      left_join(sigma_mu) %>%
      rename(mu = df.mu,
             sigma_mu = df.sigma_mu) %>%
      mutate(quantile = pnorm(-athlete_mu, mu, sigma_mu),
             z_score = (-athlete_mu - mu) / sigma_mu) 
  } else{
    athlete_df <- athlete_df %>%
      left_join(mu) %>%
      left_join(sigma_mu) %>%
      rename(mu = df.mu,
             sigma_mu = df.sigma_mu) %>%
      mutate(quantile = pnorm(athlete_mu, mu, sigma_mu),
             z_score = (athlete_mu - mu) / sigma_mu) 
  }
  
  
  
  summary_df <- athlete_df %>%
    group_by(athlete_id) %>%
    summarize(mean_z = mean(z_score)) 
  colnames(summary_df) <- c("athlete_id", paste0(dec_events[i]))
  quantile_df <- athlete_df %>%
    group_by(athlete_id) %>%
    summarize(mean_q = mean(quantile)) 
  colnames(quantile_df) <- c("athlete_id", paste0(dec_events[i]))
  total_summary_df <- left_join(total_summary_df,
                                summary_df)
  total_quantile_df <- left_join(total_quantile_df,
                                quantile_df)
}

# ashton eaton is pretty good at the 100m.
total_summary_df %>%
  filter(athlete_id == "91")

total_quantile_df %>%
  filter(athlete_id == "91")

ggplot(data = total_summary_df,
       mapping = aes(x = hundred_m, y = four_hundred_m)) +
  geom_point(alpha = .2)


z_df_long <- total_summary_df %>%
  pivot_longer(cols = dec_events,
               names_to = "event",
               values_to = "z")
q_df_long <- total_quantile_df %>%
  pivot_longer(cols = dec_events,
               names_to = "event",
               values_to = "q")


total_df_long <- left_join(z_df_long,
                           q_df_long) %>%
  mutate(day1 = event %in% dec_events[1:5],
         track = event %in% c("hundred_m",
                              "four_hundred_m", "hurdles", "fifteen_hundred_m"),
         jump = event %in% c("long_jump", "high_jump", "pole_vault"),
         throw = event %in% c('shot_put', 'discus', 'javelin'),
         pc1 = event %in% c("hundred_m", "hurdles"),
         pc2 = event %in% c("fifteen_hundred_m", "high_jump", "long_jump"))





athletes_df <- online_data_filter %>%
  ungroup() %>%
  select(name, athlete_id) %>%
  mutate(athlete_id = as.integer(athlete_id)) %>%
  distinct()
total_df_long <- left_join(total_df_long, athletes_df, by = "athlete_id") %>%
  select(athlete_id, name, everything())


day1_sum <- total_df_long %>%
  group_by(athlete_id, name, day1) %>%
  summarize(mean_z = mean(z)) %>%
  pivot_wider(names_from = day1,
              values_from = mean_z) %>%
  rename(day1 = "TRUE",
         day2 = "FALSE")

day1_plot <- ggplot(data = day1_sum, mapping = aes(x = day1,
                                      y = day2)) +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_vline(xintercept = 0,
             color = "green")+
  geom_point(alpha = .2) +
  geom_point(data = day1_sum %>%
              filter(name == 'Ashton EATON'|
                     name == 'Harrison WILLIAMS'),
             mapping = aes(color = name)) 
day1_plot

track_sum <- total_df_long %>%
  group_by(athlete_id, name, track) %>%
  summarize(mean_z = mean(z)) %>%
  pivot_wider(names_from = track,
              values_from = mean_z) %>%
  rename(track = "TRUE",
         field = "FALSE") 

track_plot <- ggplot(data = track_sum, mapping = aes(x = track,
                                      y = field)) +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_vline(xintercept = 0,
             color = "green")+
  geom_point(alpha = .2) +
  geom_point(data = track_sum %>%
               filter(name == 'Ashton EATON'|
                        name == 'Harrison WILLIAMS'),
             mapping = aes(color = name))  + 
  theme(legend.position="none")+
  labs(x = "run", y = "non_run")
track_plot

jumps_sum <- total_df_long %>%
  group_by(athlete_id, name, jump) %>%
  summarize(mean_z = mean(z)) %>%
  pivot_wider(names_from = jump,
              values_from = mean_z) %>%
  rename(jump = "TRUE",
         non_jump = "FALSE")

jumps_plot <- ggplot(data = jumps_sum, mapping = aes(x = jump,
                                                        y = non_jump)) +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = jumps_sum %>%
               filter(name == 'Ashton EATON'|
                        name == 'Harrison WILLIAMS'),
             mapping = aes(color = name))  + 
  theme(legend.position="none") 
jumps_plot

throws_sum <- total_df_long %>%
  group_by(athlete_id, name, throw) %>%
  summarize(mean_z = mean(z)) %>%
  pivot_wider(names_from = throw,
              values_from = mean_z) %>%
  rename(throw = "TRUE",
         non_throw = "FALSE")

throw_plot <- ggplot(data = throws_sum, mapping = aes(x = throw,
                                                        y = non_throw)) +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = throws_sum %>%
               filter(name == 'Ashton EATON'|
                        name == 'Harrison WILLIAMS'|
                      name == 'Kevin MAYER'),
             mapping = aes(color = name)) 
throw_plot

sprints_sum <- total_df_long %>%
  group_by(athlete_id, name, pc1) %>%
  summarize(mean_z = mean(z)) %>%
  pivot_wider(names_from = pc1,
              values_from = mean_z) %>%
  rename(sprint = "TRUE",
         non_sprint = "FALSE")

sprint_plot <- ggplot(data = sprints_sum, mapping = aes(x = sprint,
                                       y = non_sprint)) +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = sprints_sum %>%
               filter(name == 'Ashton EATON'|
                        name == 'Harrison WILLIAMS'),
             mapping = aes(color = name)) 
sprint_plot



track_plot + jumps_plot + throw_plot

day1_plot
sprint_plot

## can do event v event plots
total_summary_df <- left_join(total_summary_df, 
                              athletes_df, by = "athlete_id") %>%
  select(athlete_id, name, everything())


ggplot(data = total_summary_df,
       mapping = aes(x = shot_put,
                     y = discus)
       ) +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = total_summary_df %>%
               filter(athlete_id == "91"),
             mapping = aes(color = name))

ggplot(data = total_summary_df,
       mapping = aes(x = hundred_m,
                     y = shot_put)
) +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = total_summary_df %>%
               filter(athlete_id == "91"),
             mapping = aes(color = name))

ggplot(data = total_summary_df,
       mapping = aes(x = hundred_m,
                     y = four_hundred_m)
) +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = total_summary_df %>%
               filter(athlete_id %in% c(91, 441)),
             mapping = aes(color = name))

ggplot(data = total_summary_df, mapping = aes(x = pole_vault,
                                       y = javelin)) +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = total_summary_df  %>%
               filter(athlete_id %in% c(91, 441)),
             mapping = aes(color = name)) 

ggplot(data = total_summary_df,
       mapping = aes(x = four_hundred_m,
                     y = fifteen_hundred_m)
) +
  geom_vline(xintercept = 0,
             color = "green") +
  geom_hline(yintercept = 0,
             color = "green") +
  geom_point(alpha = .2) +
  geom_point(data = total_summary_df %>%
               filter(athlete_id 
                      %in% c(91, 310)),
             mapping = aes(color = name))



# quantiles ---------------------------------------------------------------

# quantile signature for each athlete.
total_quantile_df <- left_join(total_quantile_df, 
                               athletes_df, by = "athlete_id") %>%
  select(athlete_id, name, everything())

selected_athletes_quantiles <- total_quantile_df %>%
  filter(name == "Ashton EATON")

ggplot(data = total_quantile_df,
       mapping = aes(x = four_hundred_m,
                     y = fifteen_hundred_m)) +
  geom_vline(xintercept = 0.5,
             color = "tomato") +
  geom_hline(yintercept = 0.5,
             color = "tomato") +
  geom_point(alpha = .2) +
  geom_point(data = total_quantile_df %>%
               filter(athlete_id %in% c(91, 310)),
             mapping = aes(color = name),
             alpha = 1,
             size = 2) +
  theme_bw() +
  labs(color = "Name",
       y = "1500m Quantile",
       x = "400m Quantile") +
  scale_colour_brewer(type = "qual") 



test <- total_quantile_df %>%
  mutate(bad_both = rank(fifteen_hundred_m + four_hundred_m))

ggplot(data = total_quantile_df,
       mapping = aes(x = four_hundred_m,
                     y = fifteen_hundred_m)) +
  geom_vline(xintercept = 0.5,
             color = "tomato") +
  geom_hline(yintercept = 0.5,
             color = "tomato") +
  geom_point(alpha = .2) +
  geom_point(data = total_quantile_df %>%
               filter(athlete_id %in% c(91, 478, 60, 336, 778)),
             mapping = aes(color = name),
             alpha = 1,
             size = 2) +
  theme_bw() +
  geom_label_repel(data = total_quantile_df %>%
              filter(athlete_id %in% c(91, 478, 60, 336, 778)),
            mapping = aes(label = name,
                          color = name),
            size = 3,
  )  +
  labs(color = "Name",
       y = "1500m Quantile",
       x = "400m Quantile") +
  scale_colour_brewer(type = "qual",
                      palette = 'Dark2') +
  theme(legend.position = "none")
# ggsave("../../writing/decathlon_manu/figures/intercepts_graph.png", width = 6, height = 4,
#        units = "in")

# function to get simulated performance using sim -------------------------

age_vec <- seq(19, 30, by = .25)
quantile_vec <- c(rep(.99, 5), rep(.5, 5))
get_synth_simulated_perf <- function(sim,
                                     quantile_vec,
                                     age_vec,
                                     age_mean,
                                     age_sd,
                                     event_sums){ # simulation, vector of quantiles of length 10 (1 for each event), and vector of ages
  sim_df <- expand.grid(iter = 1:4000,
                        age = age_vec) %>%
    mutate(hundred_m = NA,
           long_jump = NA,
           shot_put = NA,
           high_jump = NA,
           four_hundred_m = NA,
           hurdles = NA,
           discus = NA,
           pole_vault = NA,
           javelin = NA,
           fifteen_hundred_m = NA,
           
    )
  
  
  for (i in 1:10){
    print(dec_events[i])
    sim_event <- sim$sims_list[[i]]
    track_events <- c("hundred_m", 'four_hundred_m', 'hurdles', 'fifteen_hundred_m')
    intercept_vec <- qnorm(quantile_vec[i],
                           mean = sim_event$mu,
                           sd = sim_event$sigma_mu)
    if (dec_events[i] %in% track_events) {
      intercept_vec = -intercept_vec
    }
    beta_df <- data.frame(beta1 = sim_event$beta1,
                          beta2 = sim_event$beta2,
                          beta3 = sim_event$beta3)
    if (i > 1){
      betaY <- sim_event$betaY
      colnames(betaY) <- paste0('beta_', dec_events[1:(i-1)])
      beta_df <- beta_df %>%
        cbind(betaY)
    }
    beta_df$iter <- 1:4000
    sigma_df <- data.frame(sigma = sim_event$sigma,
                           iter = 1:4000)
    base_df <- expand.grid(iter = 1:4000,
                           age = age_vec)
    event_df <- data.frame(iter = 1:4000, 
                           intercept = intercept_vec) %>%
      left_join(base_df) %>%
      left_join(beta_df) %>%
      left_join(sigma_df) %>%
      mutate(age = (age - age_mean) / age_sd)
    mean_pred <- event_df$intercept + 
      (event_df$beta1*event_df$age) +
      (event_df$beta2*(event_df$age^2)) + 
      (event_df$beta3*(event_df$age^3))
    event_df$mean_pred <- mean_pred
    if (i > 1) {
      prev_event_effect <- rep(0, length(mean_pred))
      for (j in 1:(i-1)) {
        prev_event_effect = prev_event_effect +
          (event_df[[paste0('beta_', dec_events[j])]] * sim_df[[dec_events[j]]])
      }
      mean_pred = mean_pred + prev_event_effect
    }
    
    event_simulated <- rnorm(n = length(mean_pred),
                             mean = mean_pred,
                             sd = event_df$sigma)
    event_df$event_simulated <- event_simulated
    sim_df[dec_events[i]] <- event_simulated
  }
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_df[event] <- (sim_df[event] *event_sd ) + event_mean
  }
  sim_df <- add_real_points2(sim_df) %>%
    select(-index)
  return(sim_df)
                                     }



## testing out the function on a bunch of different profiles
set.seed(2)
day1_athlete <- get_synth_simulated_perf(sim,
                         quantile_vec = c(rep(.95, 5), rep(.5, 5)),
                         age_vec = seq(19, 30, by = .5),
                         age_mean = age_mean,
                         age_sd = age_sd,
                         event_sums = event_sums)
day1_prop  <- day1_athlete %>%
  group_by(iter) %>%
  summarize(check = (max(calc_point) > 9200)) %>%
  summarize(prop = sum(check) / length(check) * 100)
day1_prop

set.seed(2)
day2_athlete <- get_synth_simulated_perf(sim,
                                         quantile_vec = c(rep(.5, 5), rep(.95, 5)),
                                         age_vec = seq(19, 30, by = .5),
                                         age_mean = age_mean,
                                         age_sd = age_sd,
                                         event_sums = event_sums)
day2_prop  <- day2_athlete %>%
  group_by(iter) %>%
  summarize(check = (max(calc_point) > 9200)) %>%
  summarize(prop = sum(check) / length(check) * 100)
day2_prop

# if you are a day 1 athlete (.95 quantile day 1) you have a 2.5% chance of breaking 9200.. assuming you perform a decathlon for each year. in reality, the world athletics is held every 2 years.
# on the other hand, day 2 athletes dont tend to perform as well. 

set.seed(2)
excellent_athlete <- get_synth_simulated_perf(sim,
                                         quantile_vec = c(rep(.95, 10)),
                                         age_vec = seq(19, 30, by = .5),
                                         age_mean = age_mean,
                                         age_sd = age_sd,
                                         event_sums = event_sums)
excellent_prop  <- excellent_athlete %>%
  group_by(iter) %>%
  summarize(check = (max(calc_point) > 9200)) %>%
  summarize(prop = sum(check) / length(check) * 100)
excellent_prop

set.seed(2)
good_athlete <- get_synth_simulated_perf(sim,
                                              quantile_vec = c(rep(.80, 10)),
                                              age_vec = seq(19, 30, by = .5),
                                              age_mean = age_mean,
                                              age_sd = age_sd,
                                              event_sums = event_sums)
good_prop  <- good_athlete %>%
  group_by(iter) %>%
  summarize(check = (max(calc_point) > 9200)) %>%
  summarize(prop = sum(check) / length(check) * 100)
good_prop


specific_quantiles <- total_quantile_df %>%
  filter(athlete_id %in% c(91, 60, 336))
neville_sim <- get_synth_simulated_perf(sim,
                         quantile_vec = unlist(specific_quantiles[1,3:12]),
                         age_vec = seq(19, 30, by = .25),
                         age_mean = age_mean,
                         age_sd = age_sd,
                         event_sums = event_sums) %>%
  mutate(athlete_id = 60) 
# %>%
#   rename(points = calc_point)
eaton_sim <- get_synth_simulated_perf(sim,
                                        quantile_vec = unlist(specific_quantiles[2,3:12]),
                                        age_vec = seq(19, 30, by = .25),
                                        age_mean = age_mean,
                                        age_sd = age_sd,
                                        event_sums = event_sums) %>%
  mutate(athlete_id = 91) 
# %>%
#   rename(points = calc_point)

khamedov_sim <- get_synth_simulated_perf(sim,
                                      quantile_vec = unlist(specific_quantiles[3,3:12]),
                                      age_vec = seq(19, 30, by = .25),
                                      age_mean = age_mean,
                                      age_sd = age_sd,
                                      event_sums = event_sums) %>%
  mutate(athlete_id = 336) 
# %>%
#   rename(points = calc_point)
  
selected_athletes_sim <- neville_sim %>%
  rbind(eaton_sim) %>%
  rbind(khamedov_sim)

selected_athletes_ribbon <- selected_athletes_sim %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 336 ~ "Igor KHAMEDOV",
                          athlete_id == 60 ~ 'Andrew Jeffery NEVILLE'))


good_prop  <- good_athlete %>%
  group_by(iter) %>%
  summarize(check = (max(calc_point) > 9200)) %>%
  summarize(prop = sum(check) / length(check) * 100)
good_prop


# uses actual alpha_decathlon draws!
get_real_simulated_perf <- function(sim,
                                     athlete_id,
                                     age_vec,
                                     age_mean,
                                     age_sd,
                                     event_sums){ # simulation, vector of quantiles of length 10 (1 for each event), and vector of ages
  sim_df <- expand.grid(iter = 1:4000,
                        age = age_vec) %>%
    mutate(hundred_m = NA,
           long_jump = NA,
           shot_put = NA,
           high_jump = NA,
           four_hundred_m = NA,
           hurdles = NA,
           discus = NA,
           pole_vault = NA,
           javelin = NA,
           fifteen_hundred_m = NA,
           
    )
  
  
  for (i in 1:10){
    print(dec_events[i])
    sim_event <- sim$sims_list[[i]]
    track_events <- c("hundred_m", 'four_hundred_m', 'hurdles', 'fifteen_hundred_m')
    event_df <- data.frame(iter = 1:4000,
                           intercept = sim_event$alpha_decathlon[,athlete_id],
                           beta1 = sim_event$beta1,
                           beta2 = sim_event$beta2,
                           beta3 = sim_event$beta3,
                           sigma = sim_event$sigma)
    if (i > 1){
      betaY <- sim_event$betaY
      colnames(betaY) <- paste0('beta_', dec_events[1:(i-1)])
      event_df <- event_df %>%
        cbind(betaY)
    }
    base_df <- expand.grid(iter = 1:4000,
                           age = age_vec)
    event_df <- base_df %>%
      left_join(event_df) %>%
      mutate(age = (age - age_mean) / age_sd)
    mean_pred <- event_df$intercept + 
      (event_df$beta1*event_df$age) +
      (event_df$beta2*(event_df$age^2)) + 
      (event_df$beta3*(event_df$age^3))
    event_df$mean_pred <- mean_pred
    if (i > 1) {
      prev_event_effect <- rep(0, length(mean_pred))
      for (j in 1:(i-1)) {
        prev_event_effect = prev_event_effect +
          (event_df[[paste0('beta_', dec_events[j])]] * sim_df[[dec_events[j]]])
      }
      mean_pred = mean_pred + prev_event_effect
    }
    
    event_simulated <- rnorm(n = length(mean_pred),
                             mean = mean_pred,
                             sd = event_df$sigma)
    event_df$event_simulated <- event_simulated
    sim_df[dec_events[i]] <- event_simulated
  }
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    sim_df[event] <- (sim_df[event] *event_sd ) + event_mean
  }
  sim_df <- add_real_points2(sim_df) %>%
    select(-index)
  return(sim_df)
}

neville_real_sim <- get_real_simulated_perf(sim,
                                        athlete_id = 60,
                                        age_vec = seq(19, 30, by = .25),
                                        age_mean = age_mean,
                                        age_sd = age_sd,
                                        event_sums = event_sums) %>%
  mutate(athlete_id = 60) 
# %>%
#   rename(points = calc_point)
eaton_real_sim <- get_real_simulated_perf(sim,
                                          athlete_id = 91,
                                      age_vec = seq(19, 30, by = .25),
                                      age_mean = age_mean,
                                      age_sd = age_sd,
                                      event_sums = event_sums) %>%
  mutate(athlete_id = 91) 
# %>%
#   rename(points = calc_point)

khamedov_real_sim <- get_real_simulated_perf(sim,
                                         athlete_id = 336,
                                         age_vec = seq(19, 30, by = .25),
                                         age_mean = age_mean,
                                         age_sd = age_sd,
                                         event_sums = event_sums) %>%
  mutate(athlete_id = 336) 
# %>%
#   rename(points = calc_point)

selected_athletes_real_sim <- neville_real_sim %>%
  rbind(eaton_real_sim) %>%
  rbind(khamedov_real_sim)

selected_athletes_real_ribbon <- selected_athletes_real_sim %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 336 ~ "Igor KHAMEDOV",
                          athlete_id == 60 ~ 'Andrew Jeffery NEVILLE'))

# note intercepts may be misleading! dont forget about the base of all the other previous events.

ggplot() +
  geom_line(data = selected_athletes_real_ribbon ,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name)) +
  geom_ribbon(data = selected_athletes_real_ribbon, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              color = NA) +
  geom_point(data = online_data_filter %>%
               filter(athlete_id %in% c(60, 91, 336)),
                      mapping = aes(x = age,
                                                      y = four_hundred_m,
                                                      color = name)) +
  geom_label(mapping = aes(x = 28,
                           y = 48),
             label = 'Ashton Eaton',
             color = 'purple') +
  geom_label(mapping = aes(x = 28,
                           y = 51),
             label = 'Igor Khamedov',
             color = 'orange') +
  geom_label(mapping = aes(x = 28,
                           y = 55),
             label = 'Andrew Jeffery Neville',
             color = 'darkgreen')  +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Simulated Age Curve: 400m") +
  scale_x_continuous(breaks = seq(18, 30, by = 2), 
                     lim = c(18, 30)) +
  theme(legend.position="none")+
  labs(x = "Age", y = "400m")

selected_athletes_real_ribbon2 <- selected_athletes_real_sim %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(fifteen_hundred_m),
            quant2.5 = quantile(fifteen_hundred_m, .025),
            quant97.5 = quantile(fifteen_hundred_m, .975),
            quant5 = quantile(fifteen_hundred_m, .05),
            quant95 = quantile(fifteen_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 336 ~ "Igor KHAMEDOV",
                          athlete_id == 60 ~ 'Andrew Jeffery NEVILLE'))

ggplot() +
  geom_line(data = selected_athletes_real_ribbon2 ,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name)) +
  geom_ribbon(data = selected_athletes_real_ribbon2, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  geom_point(data = online_data_filter %>%
               filter(athlete_id %in% c(60, 91, 336)),
             mapping = aes(x = age,
                           y = fifteen_hundred_m,
                           color = name)) +
  geom_label(mapping = aes(x = 28,
                           y = 285),
             label = 'Ashton Eaton',
             color = 'purple') +
  geom_label(mapping = aes(x = 28,
                           y = 245),
             label = 'Igor Khamedov',
             color = 'orange') +
  geom_label(mapping = aes(x = 28,
                           y = 340),
             label = 'Andrew Jeffery Neville',
             color = 'darkgreen')  +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Simulated Age Curve: 1500m") +
  scale_x_continuous(breaks = seq(18, 30, by = 2), 
                     lim = c(18, 30)) +
  theme(legend.position="none")+
  labs(x = "Age", y = "1500m")

selected_athletes_real_ribbon3 <- selected_athletes_real_sim %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(shot_put),
            quant2.5 = quantile(shot_put, .025),
            quant97.5 = quantile(shot_put, .975),
            quant5 = quantile(shot_put, .05),
            quant95 = quantile(shot_put, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 336 ~ "Igor KHAMEDOV",
                          athlete_id == 60 ~ 'Andrew Jeffery NEVILLE'))

ggplot() +
  geom_line(data = selected_athletes_real_ribbon3 ,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name)) +
  geom_ribbon(data = selected_athletes_real_ribbon3, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              color = NA) +
  geom_point(data = online_data_filter %>%
               filter(athlete_id %in% c(60, 91, 336)),
             mapping = aes(x = age,
                           y = shot_put,
                           color = name)) +
  geom_label(mapping = aes(x = 28,
                           y = 15),
             label = 'Ashton Eaton',
             color = 'purple') +
  geom_label(mapping = aes(x = 28,
                           y = 11),
             label = 'Igor Khamedov',
             color = 'orange') +
  geom_label(mapping = aes(x = 28,
                           y = 12),
             label = 'Andrew Jeffery Neville',
             color = 'darkgreen')  +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Simulated Age Curve: Shot put") +
  scale_x_continuous(breaks = seq(18, 30, by = 2), 
                     lim = c(18, 30)) +
  theme(legend.position="none")+
  labs(x = "Age", y = "Shot put")

test <- online_data_filter %>%
  filter(athlete_id %in% c(60, 91, 336))

eaton_mayer <-  total_quantile_df %>%
  filter(athlete_id %in% c(91, 478, 60, 336, 778)) %>%
  select(-athlete_id) 


kable(eaton_mayer,
      digits = 2,
      format = "latex",
      booktabs = T)

synthetic_table <- eaton_mayer %>%
  filter(name %in% eaton_mayer$name[c(2,4,5)])

synthetic_profiles <- data.frame(
  name = c("Day 1",
           "Day 2",
           "Excellent",
           "Good")
)
quantiles <- data.frame(matrix (
  c(rep(.95, 5), rep(.5, 5),
    rep(.5, 5), rep(.95, 5),
    rep(.95, 10),
    rep(.8, 10)),
  nrow = 4,
  byrow = T
))
colnames(quantiles) <- dec_events
synthetic_profiles <- synthetic_profiles %>%
  cbind(quantiles)
synthetic_table <- synthetic_table %>%
  rbind(synthetic_profiles)



synthetic_props <- c(day1_prop, day2_prop, excellent_prop, good_prop)
synthetic_table$record <- c("-", "-", "-", synthetic_props)


# replace synthetic[i,2:11] with 1:3 to get the other guys
set.seed(2)
eaton_athlete <- get_synth_simulated_perf(sim,
                                         quantile_vec = unlist(synthetic_table[1,2:11]),
                                         age_vec = seq(19, 30, by = .5),
                                         age_mean = age_mean,
                                         age_sd = age_sd,
                                         event_sums = event_sums)
eaton_prop  <- eaton_athlete %>%
  group_by(iter) %>%
  summarize(check = (max(calc_point) > 9200)) %>%
  summarize(prop = sum(check) / length(check) * 100)
eaton_prop

kable(synthetic_table,
      digits = 2,
      format = "latex",
      booktabs = T)

# sythetic profiles -------------------------------------------------------

# couple of profiles. 
# - top tier all around (95 all events)
# - decent all around (80 all events)
# - mediocre all around (50 all events)
# - sprinter/../.. vs non sprinter/../.. (95 specialized events, 50 else)
# - day 1 vs day 2 (95 day 1, 50 else, repeat for day 2)

mu_summary <- data.frame(event = dec_events,
                         mean = rep(NA, 10),
                         sd = rep(NA, 10),
                         q95 = rep(NA, 10),
                         q80 = rep(NA, 10),
                         q50 = rep(NA, 10))
for (i in (1:10)){
  df  <- sim$sims_list[[i]]
  event <- dec_events[i]
  mean_mu <- mean(df$mu)
  sd_mu <- mean(df$sigma_mu)
  mu_summary$mean[i] <- mean_mu
  mu_summary$sd[i] <- sd_mu
  track <- event %in%c("hundred_m",
                       "four_hundred_m", "hurdles", "fifteen_hundred_m")
  if (track) {
    mu_summary$q95[i] = -qnorm(.95, mean_mu, sd_mu)
    mu_summary$q80[i] = -qnorm(.80, mean_mu, sd_mu)
    mu_summary$q50[i] = -qnorm(.50, mean_mu, sd_mu)
  } else {
    mu_summary$q95[i] = qnorm(.95, mean_mu, sd_mu)
    mu_summary$q80[i] = qnorm(.80, mean_mu, sd_mu)
    mu_summary$q50[i] = qnorm(.50, mean_mu, sd_mu)
  }

}

mu_summary_long <- mu_summary %>%
  select(event, q95, q80, q50) %>%
  rename('.95' = q95,
         '.80' = q80,
         '.50' = q50) %>%
  pivot_longer(cols = c('.95', '.80', '.50'),
                        names_to = 'quantile',
                        values_to = 'intercept') %>%
  mutate(quantile = as.double(quantile))

profile = c("excellent",
            "good",
            "ok",
            "sprinter",
            "thrower",
            "jumper",
            "day1",
            "day2")

profile_df <- expand.grid(profile, dec_events) %>%
  rename(profile = Var1,
         event = Var2) %>%
  mutate(quantile = case_when(
    profile == 'excellent' ~ .95,
    profile == 'good' ~ .80,
    profile == 'ok' ~ .50,
    profile == 'sprinter' & event %in% c("hundred_m",
                                         "four_hundred_m", 
                                         "hurdles") ~ .95,
    profile == 'thrower' & event %in% c("long_jump", 
                                        "high_jump",
                                        "pole_vault") ~ .95,
    profile == 'jumper' & event %in% c('shot_put',
                                       'discus',
                                       'javelin') ~ .95,
    profile == 'day1' & event %in% dec_events[1:5] ~ .95,
    profile == 'day2' & event %in% dec_events[6:10] ~ .95,
    .default = .5
  ))
profile_df_wide <- profile_df %>%
  pivot_wider(names_from = event,
              values_from = quantile)

profile_df2 <- profile_df %>%
  left_join(mu_summary_long,
            by = c("event", "quantile"))

profile_df_rep <- merge(profile_df2, data.frame(iter = 1:4000)) %>%
  select(iter, everything())

## next steps.
# use the posterior draws from beta to calculate standardized results for each event
big_beta1_df <- data.frame(expand.grid(event = dec_events),
                           iter = 1:4000,
                           beta1 = NA) %>%
  pivot_wider(names_from = event,
              values_from = beta1)
big_beta2_df <- data.frame(expand.grid(event = dec_events),
                           iter = 1:4000,
                           beta2 = NA) %>%
  pivot_wider(names_from = event,
              values_from = beta2)
big_beta3_df <- data.frame(expand.grid(event = dec_events),
                           iter = 1:4000,
                           beta3 = NA) %>%
  pivot_wider(names_from = event,
              values_from = beta3)
for (i in 1:10) {
  event = dec_events[i]
  df <- sim$sims_list[[i]]
  big_beta1_df[[event]] <- df$beta1
  big_beta2_df[[event]] <- df$beta2
  big_beta3_df[[event]] <- df$beta3
}
big_beta1_df <- big_beta1_df %>%
  pivot_longer(cols = dec_events,
               names_to = 'event',
               values_to = 'beta1')
big_beta2_df <- big_beta2_df %>%
  pivot_longer(cols = dec_events,
               names_to = 'event',
               values_to = 'beta2')
big_beta3_df <- big_beta3_df %>%
  pivot_longer(cols = dec_events,
               names_to = 'event',
               values_to = 'beta3')
all_betas_df <- big_beta1_df %>%
  left_join(big_beta2_df) %>%
  left_join(big_beta3_df)

profile_df_rep <- profile_df_rep %>%
  left_join(all_betas_df,
            by = join_by(iter, event))
age_seq <- seq(19, 30, by = .05)
age_seq_st <- (age_seq - age_mean) / age_sd
age_df = data.frame(age = age_seq,
                    age_st = age_seq_st)

# going to make a more general function that takes a vector of quantiles that we want. then runs through 4000 age curves.
######### note: this does not have the proper uncertainty propogation. just means.. might have to go make a new stan file where we give the intercepts instead of being random. can talk to sameer about it.
get_performances_profile <- function(quantile_df_iter,
                                     age_st,
                                     age){ # function of standardized age
  result_st = quantile_df_iter$int + 
    (quantile_df_iter$beta1*age_st) +
    (quantile_df_iter$beta2*(age_st^2)) +
    (quantile_df_iter$beta3*(age_st^2))
  quantile_df_iter$age = age
  quantile_df_iter$result_st = result_st
  quant_df_wide <- quantile_df_iter %>%
    select(iter, profile, age, quantile, event, result_st) %>%
    pivot_wider(names_from = event,
                values_from = result_st) %>%
    select(iter, profile, age, quantile, dec_events)
  # now unstandardize results
  for (event in dec_events) {
    event_mean <- event_sums %>%
      filter(event == !!event) %>%
      select(mean_score) %>%
      pull()
    event_sd <- event_sums %>%
      filter(event == !!event) %>%
      select(sd_score) %>%
      pull()
    quant_df_wide[event] <- (quant_df_wide[event] *event_sd ) + event_mean
  }
  quant_df_wide <- add_real_points2(quant_df_wide)
  return(quant_df_wide)
}

get_age_curves_profile <- function(quant_vec, profile_name){
  mu_summary <- data.frame(event = dec_events,
                           mean = rep(NA, 10),
                           sd = rep(NA, 10),
                           quantile = quant_vec,
                           int = rep(NA, 10))
  for (i in (1:10)){
    df  <- sim$sims_list[[i]]
    event <- dec_events[i]
    mean_mu <- mean(df$mu)
    sd_mu <- mean(df$sigma_mu)
    mu_summary$mean[i] <- mean_mu
    mu_summary$sd[i] <- sd_mu
    track <- event %in%c("hundred_m",
                         "four_hundred_m", "hurdles", "fifteen_hundred_m")
    if (track) {
      mu_summary$int[i] = -qnorm(quant_vec[i], mean_mu, sd_mu)
    } else {
      mu_summary$int[i] = qnorm(quant_vec[i], mean_mu, sd_mu)
    }
    
  }
  age_seq <- seq(19, 30, by = .05)
  age_seq_st <- (age_seq - age_mean) / age_sd
  age_df = data.frame(age = age_seq,
                      age_st = age_seq_st)
  quantile_df <- data.frame(profile = profile_name,
                            event = dec_events,
                            quantile = quant_vec) %>%
    left_join(mu_summary) %>%
    select(-mean, -sd)
  quantile_df_iter <- merge(quantile_df, data.frame(iter = 1:4000)) %>%
    select(iter, everything()) %>%
    merge(all_betas_df) 
  age_st = age_seq_st[1]
  age = age_seq[1]
  performances_profile <- get_performances_profile(quantile_df_iter,
                                      age_st,
                                      age)
  for (i in 2:length(age_seq)) {
    age_st <- age_seq_st[i]
    age <- age_seq[i]
    results <- get_performances_profile(quantile_df_iter,
                                        age_st,
                                        age)
    performances_profile <- rbind(performances_profile, results)
    
  }
  decathlons <- performances_profile
  proportions <- performances_profile %>%
    group_by(iter) %>%
    summarize(seven = max(calc_point) >= 7000,
              seven_half = max(calc_point) >= 7500,
              eight = max(calc_point) >= 8000,
              eight_half = max(calc_point) >= 8500,
              nine = max(calc_point) >= 9000) %>%
    ungroup() %>%
    summarize(seven_prop = mean(seven),
              seven_half_prop = mean(seven_half),
              eight_prop = mean(eight),
              eight_half_prop = mean(eight_half),
              nine_prop = mean(nine)) %>%
    mutate(profile = profile_name) %>%
    select(profile, everything())
  return(list(
    decathlons = decathlons,
    proportions = proportions
  ))
}

# unstandardize each performance and calculate points
# track proportion of decathlons for each athlete type to crack 8000, 8500, 9000

excellent_profile <- get_age_curves_profile(quant_vec = rep(.95, 10),
                                            profile_name = "excellent"
                                            )

good_profile <- get_age_curves_profile(quant_vec = rep(.8, 10),
                                            profile_name = "good"
)

ok_profile <- get_age_curves_profile(quant_vec = rep(.5, 10),
                                       profile_name = "ok"
)




