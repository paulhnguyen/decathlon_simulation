# Aug 4, 2025
# script to find the maxima age for each individual event. uses full_comp_sim data
# procedure for each event, extract posterior samples for age betas. take derivative to calculate max/min
# mu = beta0 + age*beta1 + age^2*beta2 + age^3*beta3 + other
# dmu/dage = beta1 + 2beta2 + 3beta3... see notes for full calc

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

setwd("~/school/wisconsin/research_repo/decathlon")
study = "new_stan_sim"
data_dir = "new_stan_sim/data/"
script_dir = "new_stan_sim/study/"
stan_dir = "new_stan_sim/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))
# source(paste0(script_dir, "settings_gen_data.R"))

online_data_filter <- read_csv(paste0(data_dir,"online_data_filter.csv")) %>%
  filter(year(dob) > 1950,
         discus > 2) %>%
  group_by(name, dob) %>%
  mutate(athlete_id = cur_group_id()) %>%
  unique()
event_sums <- get_event_sums_df(online_data_filter)
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
event_sums <- get_event_sums_df(online_data_filter)
dec_data_standard <- standardize_decathlon_data(online_data_filter,
                                                event_sums)

comp_sim <- read_rds(file = "results/comp_sim_for_select_athletes.RData")

test <- comp_sim$sims_list$hundred_m_sims
beta1 <- test$beta1[1]
beta2 <- test$beta2[1]
beta3 <- test$beta3[1]
# function to get max x for y = beta0 + beta1x + beta2x^2 + beta3x^3
get_max <- function(beta1, beta2, beta3){
  discriminant <- 4*(beta2^2) - (12*beta1*beta3)
  if (discriminant > 0) {
    crit1 <- ((-2*beta2) + sqrt((4*beta2^2) - (12*beta1*beta3))) / (6*beta3)
    crit2 <- ((-2*beta2) - sqrt((4*beta2^2) - (12*beta1*beta3))) / (6*beta3)
    second_deriv1 <- (2*beta2) + (6*beta3 * crit1)
    second_deriv2 <- (2*beta2) + (6*beta3 * crit2)
    if (second_deriv1 < 0){
      max <- crit1
    } else{
      max <- crit2
    }
  } 
  else{
    max <- NA
  }
  return(max)
}
get_min <- function(beta1, beta2, beta3){
  discriminant <- 4*(beta2^2) - (12*beta1*beta3)
  if (discriminant > 0) {
    crit1 <- ((-2*beta2) + sqrt((4*beta2^2) - (12*beta1*beta3))) / (6*beta3)
    crit2 <- ((-2*beta2) - sqrt((4*beta2^2) - (12*beta1*beta3))) / (6*beta3)
    second_deriv1 <- (2*beta2) + (6*beta3 * crit1)
    second_deriv2 <- (2*beta2) + (6*beta3 * crit2)
    if (second_deriv1 > 0){
      min <- crit1
    } else{
      min <- crit2
    }
  }
  else{
    min <- NA
  }
  return(min)
}

age_range <- 20:30
get_max_grid <- function(beta1, beta2, beta3, age_range, age_mean, age_sd){
  df <- data.frame(age = age_range,
                   value = rep(NA, length(age_range))) %>%
    mutate(sd_age = (age - age_mean) / age_sd) %>%
    select(age, sd_age, value)
  for (i in 1:length(age_range)) {
    df$value[i] <- beta1*df$sd_age[i] + beta2*(df$sd_age[i]^2) + beta3*(df$sd_age[i]^3)
  } 
  max_age <- df %>%
    mutate(max = max(value)) %>%
    filter(value == max) %>%
    pull(age)
  return(max_age)
}

get_min_grid <- function(beta1, beta2, beta3, age_range, age_mean, age_sd){
  df <- data.frame(age = age_range,
                   value = rep(NA, length(age_range))) %>%
    mutate(sd_age = (age - age_mean) / age_sd) %>%
    select(age, sd_age, value)
  for (i in 1:length(age_range)) {
    df$value[i] <- beta1*df$sd_age[i] + beta2*(df$sd_age[i]^2) + beta3*(df$sd_age[i]^3)
  } 
  min_age <- df %>%
    mutate(min = min(value)) %>%
    filter(value == min) %>%
    pull(age)
  return(min_age)
}

age_mean <- mean(online_data_filter$age)
age_sd <- sd(online_data_filter$age)

(age_sd * get_min(-.03, .16, -.02)) + age_mean

best_age_df <- data.frame(matrix(nrow = 2000, ncol = 10))
best_age_df_grid <- data.frame(matrix(nrow = 2000, ncol = 10))
colnames(best_age_df) <- dec_events
colnames(best_age_df_grid) <- dec_events
# want mins for  hundred_m, four_hundred_m, hurdles, fifteen_hundred_m
min_list <- c(1,4,5,6,10) # corresponding sim
for (j in min_list) {
  event_sim <- comp_sim$sims_list[[j]]
  event <- dec_events[j]
  for (i in 1:nrow(best_age_df)) {
    beta1 <- event_sim$beta1[i]
    beta2 <- event_sim$beta2[i]
    beta3 <- event_sim$beta3[i]
    best_age_standardized <- get_min(beta1, beta2, beta3)
    best_age <- (age_sd * best_age_standardized) + age_mean
    best_age_df[[event]][i] <- best_age
    best_age_grid <- get_min_grid(beta1, beta2, beta3, 
                                  age_range = seq(18, 40, by = .01),
                                  age_mean = age_mean,
                                  age_sd = age_sd)
    best_age_df_grid[[event]][i] <- best_age_grid
  }
}
ggplot(data = best_age_df, mapping = aes(x = fifteen_hundred_m)) +
  geom_histogram(color = "white")
ggplot(data = best_age_df_grid, mapping = aes(x = fifteen_hundred_m)) +
  geom_histogram(color = "white")
# want maxes for want max for long_jump, shot_put, high_jump, discus, pole_vault, javelin
max_list <- c(2,3,4,7,8,9)
for (j in max_list) {
  event_sim <- comp_sim$sims_list[[j]]
  event <- dec_events[j]
  for (i in 1:nrow(best_age_df)) {
    beta1 <- event_sim$beta1[i]
    beta2 <- event_sim$beta2[i]
    beta3 <- event_sim$beta3[i]
    best_age_standardized <- get_max(beta1, beta2, beta3)
    best_age <- (age_sd * best_age_standardized) + age_mean
    best_age_df[[event]][i] <- best_age
    best_age_grid <- get_max_grid(beta1, beta2, beta3, 
                                  age_range = seq(18, 40, by = .01),
                                  age_mean = age_mean,
                                  age_sd = age_sd)
    best_age_df_grid[[event]][i] <- best_age_grid
  }
}

summary(best_age_df)
summary(best_age_df_grid)

ggplot(online_data_filter, mapping = aes(x = age, y = javelin)) +
  geom_point(alpha = .2) +
  geom_vline(xintercept = 27, color = "tomato") +
  geom_smooth()



comp_sim_df <- (comp_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)

comp_ribbon_df_pole_vault <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(pole_vault),
            quant2.5 = quantile(pole_vault, .025),
            quant97.5 = quantile(pole_vault, .975),
            quant5 = quantile(pole_vault, .05),
            quant95 = quantile(pole_vault, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

comp_ribbon_df_fifteen_hundred_m <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(fifteen_hundred_m),
            quant2.5 = quantile(fifteen_hundred_m, .025),
            quant97.5 = quantile(fifteen_hundred_m, .975),
            quant5 = quantile(fifteen_hundred_m, .05),
            quant95 = quantile(fifteen_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


ggplot() +
  geom_point(data = online_data_filter %>%
               filter(athlete_id %in% c(91, 310)),
             mapping = aes(x = age,
                           y = fifteen_hundred_m,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_fifteen_hundred_m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df_fifteen_hundred_m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) 
  

dec_data_standard2 <- dec_data_standard %>%
  ungroup() %>%
  mutate(mean_age = mean(age),
         sd_age = sd(age), 
         age_sd = (age - mean_age) / sd_age)
pv_sum <- lmer(fifteen_hundred_m~ (1|athlete_id) + age_sd + I(age_sd^2) + I(age_sd^3), dec_data_standard2)
pv_sum
test <- data.frame(age_sd =  dec_data_standard2$age_sd
) %>%
  mutate(age = (age_sd *sd(dec_data_standard2$age)) + mean(dec_data_standard2$age),
         y = 0.033847 + 0.054666*age_sd +  0.063291*(age_sd^2) +  -0.002337*(age_sd^3))




ggplot(data = test, mapping = aes(x = age, y = y)) + 
  geom_line() +
  geom_point(data = dec_data_standard2, mapping = aes(x = age, y = fifteen_hundred_m),
             alpha = .1) +
  geom_vline(xintercept = get_min(coefs[[1]], coefs[[2]], coefs[[3]]) * age_sd + age_mean)
coefs <- coef(pv_sum)$athlete_id[1,c(2,3,4)]

get_min(coefs[[1]], coefs[[2]], coefs[[3]]) * age_sd + age_mean
get_min_grid(coefs[[1]], coefs[[2]], coefs[[3]], seq(18, 30, by = .01), 
             age_mean = age_mean,
             age_sd = age_sd)



# new age curve max -------------------------------------------------------

# Aug 6. figured out that only looking at age coefficients is insufficient to producing age curve. look at full posterior predictive instead.

comp_sim_df_indexed <- comp_sim_df
comp_sim_df_indexed$index2 <- (comp_sim_df$index - 1) %/% 222

best_ages_long <- comp_sim_df_indexed %>%
  select(-points,-name) %>%
  pivot_longer(cols = dec_events,
               names_to = "event",
               values_to = "score") %>%
  group_by(athlete_id,event, index2) %>%
  mutate(min_score = min(score),
         max_score = max(score),
         best_score = case_when(event %in% dec_events[c(2,3,4,7,8,9)] ~ max_score,
                                .default = min_score)) %>%
  filter(score == best_score)


top_points  <-  comp_sim_df_indexed %>%
  add_real_points2() %>%
  select(athlete_id, calc_point, index2, age) %>%
  group_by(athlete_id, index2) %>%
  mutate(max_point = max(calc_point)) %>%
  filter(calc_point == max_point)

best_age_dec <- top_points %>%
  ungroup() %>%
  summarize(q25 = quantile(age, .25),
            q75 = quantile(age, .75),
            mean = mean(age))

best_ages_long %>%
  filter(event == "four_hundred_m")  %>%
  ggplot(mapping = aes(x = age)) +
  geom_histogram(color = "white",
                 bins = 20) +
  geom_vline(xintercept = quantile(best_ages_long %>%
                                     filter(event == "four_hundred_m") %>%
                                     pull(age), c(.25, .75)),
             color = "tomato")

best_ages_summary <- best_ages_long %>%
  group_by(event) %>%
  summarize(q25 = quantile(age, .25),
            q75 = quantile(age, .75),
            mean = mean(age))



### remaking age curves for events: no simple. add maximal age ranges
full_test_df <- online_data_filter %>%
  filter((subj_id == 239)|(subj_id == 759)) %>%
  mutate(status = case_when(((subj_id==239) & (age < 25) )|((subj_id==759) & (age < 25)) ~ "train",
                            ((subj_id==239) & (age >= 25) )|((subj_id==759) & (age >= 25)) ~ "test"))

comp_ribbon_df_shot_put <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(shot_put),
            quant2.5 = quantile(shot_put, .025),
            quant97.5 = quantile(shot_put, .975),
            quant5 = quantile(shot_put, .05),
            quant95 = quantile(shot_put, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

comp_shot_put_curve <- ggplot()  +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = shot_put,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_shot_put %>%
                filter((age <= 28.9) & (age >= 26.2)),
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3,
              fill = "tomato") +
  geom_ribbon(data = comp_ribbon_df_shot_put,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3)  +
  geom_line(data = comp_ribbon_df_shot_put,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  theme_bw() + 
  labs(title = "Shot put age curve",
       x = "Age",
       y = "Shot Put",
       color = "Name",
       fill = "Name")  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.2),
        legend.background = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10))  +
  guides(color = guide_legend(override.aes = list(size = 0.5)),
         fill = guide_legend(override.aes = list(size = 0.5))) +
scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") 
# xmin and xmax taken from comp_sim_df_long
comp_shot_put_curve


comp_ribbon_df_400m <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

comp_400m_curve <- ggplot()  +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_400m %>%
                filter((age <= 25.7) & (age >= 22.1)),
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3,
              fill = "tomato") +
  geom_ribbon(data = comp_ribbon_df_400m,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3)  +
  geom_line(data = comp_ribbon_df_400m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "400m age curve",
       x = "Age",
       y = "400m",
       color = "Name",
       fill = "Name") +
  ylim(c(43, 50.5)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2,0.2),
        legend.background = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10))  +
  guides(color = guide_legend(override.aes = list(size = 0.5)),
         fill = guide_legend(override.aes = list(size = 0.5)))
# xmin and xmax taken from comp_sim_df_long
comp_400m_curve



comp_400m_curve /comp_shot_put_curve 
# + plot_annotation(title = "95% Posterior predictive intervals for event results by age")

ggsave("writing/decathlon_manu/figures/age_curve_comp.png", width = 7, height = 9,
       units = "in")


# actually shouldnt use the posterior predictive. 
# use posterior mean prediction to calculate maximal age. --------


# 100m
age_seq <- (seq(19, 30, by = .05) - age_mean) / age_sd
big_event_df <- data.frame(age = NA, index = NA, y = NA) %>%
  drop_na()
for (i in 1:length(age_seq)) {
  if (i %% 100 == 0) {
    print(i)
  }
  event_df <- data.frame(age = rep(age_seq[i], 4000))
  event_sim <- comp_sim$sims_list[[1]]
  y_vec = event_sim$mu + event_sim$beta1 * age_seq[i] + (event_sim$beta2 * (age_seq[i])^2) + (event_sim$beta3 * age_seq[i]^3)
  event_df$y <- y_vec
  index <- 1:4000
  event_df$index <- index
  big_event_df <- rbind(big_event_df, event_df)
}

min_df <- big_event_df %>%
  group_by(index) %>%
  mutate(min_score = min(y),
         age = age*age_sd + age_mean) %>%
  filter(y == min_score)

ggplot(data = min_df, mapping = aes(x = age)) +
  geom_histogram(color = "white",
                 binwidth = .05) +
  geom_vline(color = "tomato",
             xintercept = mean(min_df$age)) +
  theme_bw() + 
  labs(title = "Ages at minimum 100m scores",
       x = "Age",
       y = "Count") 



## full dataset
full_post_data <- expand.grid(index = 1:4000,
                              age = age_seq)
for (i in 1:length(dec_events)) {
  big_event_df <- data.frame(age = NA, index = NA, y = NA) %>%
    drop_na()
  event_sim <- comp_sim$sims_list[[i]]
  for (j in 1:length(age_seq)) {
    event_df <- data.frame(age = rep(age_seq[j], 4000))
    age <- age_seq[j]
    if (i == 1) { # if 100m
      y_vec = event_sim$mu + event_sim$beta1 * age + (event_sim$beta2 * (age)^2) + (event_sim$beta3 * age^3)
    } else{
      age2 <- age
      prev_event_df  <- as.matrix(full_post_data %>%
        filter(age == age2) %>%
        select(dec_events[1:(i-1)]) )
      y_vec = event_sim$mu + (event_sim$beta1 * age) + (event_sim$beta2 * (age)^2) + (event_sim$beta3 * age^3)
      event_effect <- rep(NA, 4000)
      for (k in 1:length(event_effect)) {
        event_effect[k] <- event_sim$betaY[k,] %*% prev_event_df[k,] 
      }
      y_vec <- y_vec + event_effect
    }
    
    event_df$age <- age
    event_df[[dec_events[i]]] <- y_vec
    event_df$index <- 1:4000
    big_event_df <- rbind(big_event_df, event_df)
  }
    full_post_data <- left_join(full_post_data, big_event_df)
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
  full_post_data[event] <- (full_post_data[event] *event_sd ) + event_mean
}


full_post_data_long <- full_post_data %>%
  pivot_longer(cols = dec_events,
               names_to = "event",
               values_to = "score")

best_df <- full_post_data_long %>%
  group_by(index, event) %>%
  mutate(min_score = min(score),
         max_score = max(score),
         best_score = case_when(event %in% dec_events[c(2,3,4,7,8,9)] ~ max_score,
                                .default = min_score)) %>%
  ungroup() %>%
  filter(score == best_score) %>%
  mutate(age = (age*age_sd) + age_mean)

summary_best_df <- best_df %>%
  group_by(event) %>%
  summarize(q2.5 = quantile(age, .025),
            q97.5 = quantile(age, .975),
            mean = mean(age))
target <- dec_events
summary_best_df <- summary_best_df[match(target, summary_best_df$event),]
summary_best_df$event <- c("100m", "Long Jump", "Shot Put", "High Jump" ,
                           "400m", "110mH", "Discus Throw", "Pole Vault", "Javelin Throw",
                           "1500m") 
summary_best_df %>%
  select(event, mean, q2.5, q97.5) %>%
  kable(caption = "Summary statistics for ages for optimal performance in each decathlon event across 4000 decathlon age curves. Optimal performance is defined as the lowest time for sprints and furthest distance for throws and jumps.",
        escape = F,
        label = "best_age_table",
        digits = 2,
        format = "latex",
        booktabs = T) # Output format = latex 


ggplot(best_df %>%
         filter(event %in% c("hundred_m", "four_hundred_m", "shot_put", "javelin")), mapping = aes(x = age)) +
  geom_histogram(color = "white") + 
  theme_bw() +
  labs(x = "Age",
       y = "Count",
       title = "Ages at maximal scores") +
  facet_wrap(~event, nrow = 2)


comp_shotput_curve <- ggplot()  +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = shot_put,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_shot_put %>%
                filter((age >=  28.4 ) & (age <= 29.4 )),
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3,
              fill = "tomato") +
  geom_ribbon(data = comp_ribbon_df_shot_put,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3)  +
  geom_line(data = comp_ribbon_df_shot_put,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  theme_bw() + 
  labs(title = "Shot Put",
       x = "Age",
       y = "Shot Put",
       color = "Name",
       fill = "Name")  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.15),
        legend.background = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10))  +
  guides(color = guide_legend(override.aes = list(size = 0.5)),
         fill = guide_legend(override.aes = list(size = 0.5))) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") 

comp_400m_curve <- ggplot()  +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_400m %>%
                filter((age >= 23.4) & (age <= 23.8)),
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3,
              fill = "tomato") +
  geom_ribbon(data = comp_ribbon_df_400m,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3)  +
  geom_line(data = comp_ribbon_df_400m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "400m",
       x = "Age",
       y = "400m",
       color = "Name",
       fill = "Name") +
  ylim(c(43, 50.5)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.15),
        legend.background = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10))  +
  guides(color = guide_legend(override.aes = list(size = 0.5)),
         fill = guide_legend(override.aes = list(size = 0.5)))
# xmin and xmax taken from comp_sim_df_long
comp_shotput_curve / comp_400m_curve
ggsave("writing/decathlon_manu/figures/age_curve_comp.png", width = 7, height = 9,
       units = "in")
