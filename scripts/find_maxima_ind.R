# Aug 28, 2025
# script to find the maximal age for each individual event. Uses simulations generated from code_to_generate_sims.R (compositional case)
# procedure for each event: extract posterior samples for age betas and event coefficeints. Run through grid of ages to find best performances. make sure to include indirect age effects through the previous event coefficients. 


set.seed(2)
library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(knitr)


study = "../study"
data_dir = "../data/"
script_dir = "../study/"
stan_dir = "../stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))

online_data_filter <- read_csv(paste0(data_dir,"online_data_filter.csv"))  %>%
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
age_mean <- mean(online_data_filter$age)
age_sd <- sd(online_data_filter$age)

# run code_to_generate_sims.R to get compositional model simulation. we can generate sims for only some athletes, and not the entire dataset, because the important part for the age curves are the coefficients, which are the same for each athlete. Including other athletes only changes the random intercept. 
comp_sim <- read_rds(file = "../../results/comp_sim_for_select_athletes.RData")

comp_sim_df <- (comp_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)


# use posterior draws of age and event coefficients to calculate maximal age. --------

# 100m ex.
# standardize ages to plug into model.
age_seq <- (seq(19, 30, by = .05) - age_mean) / age_sd
big_event_df <- data.frame(age = NA, index = NA, y = NA) %>%
  drop_na()
# no event coefficients, so just use beta for 100m
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
  filter(y == min_score) %>%
  unique()

ggplot(data = min_df, mapping = aes(x = age)) +
  geom_histogram(color = "white",
                 binwidth = .05) +
  geom_vline(color = "tomato",
             xintercept = mean(min_df$age)) +
  theme_bw() + 
  labs(title = "Ages at minimum 100m scores",
       x = "Age",
       y = "Count") 



## full dataset. repeat the above with all ten events.
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


#plot the age curves for eaton and williams
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
comp_shotput_curve 
# ggsave("../../writing/decathlon_manu/figures/shot_put_best_age_curve.png", width = 6, height = 4,
#        units = "in")
# ggsave("../../writing/decathlon_manu_cmu_copy/figures/shot_put_best_age_curve.png", width = 6, height = 4,
#        units = "in")


comp_400m_curve
# ggsave("../../writing/decathlon_manu/figures/400m_best_age_curve.png", width = 6, height = 4,
#        units = "in")
# ggsave("../../writing/decathlon_manu_cmu_copy/figures/400m_best_age_curve.png", width = 6, height = 4,
#        units = "in")
