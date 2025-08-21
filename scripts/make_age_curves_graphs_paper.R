# May 28, 2025
# Code to make initial motivating graph for decathlon paper, as well as graphs for experiment section.


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


ggplot(data = full_test_df  %>%
         filter(status == "train"),
       mapping = aes(x = age,
                     y = points,
                     color = name)) +
  geom_point() +
  scale_color_brewer(type = "qual") +
  theme_bw() +
  labs(title = "Initial Decathlon Performances for Eaton and Williams",
       color = "Athlete",
       y = "Points",
       x = "Age")  +
  scale_x_continuous(breaks = seq(18, 30, by = 2), 
                     lim = c(18, 30)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.15),
        legend.background = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10))  +
  guides(color = guide_legend(override.aes = list(size = 1)),
         fill = guide_legend(override.aes = list(size = 1))) 
ggsave("writing/decathlon_manu/figures/intro_plot.png", width = 6, height = 4,
       units = "in")
event_sums <- get_event_sums_df(online_data_filter)



# points age curve --------------------------------------------------------


baseline_sim <- read_rds(file = "results/baseline_sim_for_select_athletes.RData")
baseline_sim_ribbon_df <- data.frame(quant2.5 = apply((baseline_sim$post_sim), MARGIN = 2, 
                                                  function(x) quantile(x,.025)),
                                     quant97.5 = apply((baseline_sim$post_sim), MARGIN = 2, 
                                                  function(x) quantile(x,.975)),
                                     mean = apply((baseline_sim$post_sim), MARGIN = 2,function(x) mean(x)),
                                     age = age_vec,
                                     athlete = athlete_id) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

simple_sim <- read_rds(file = "results/simple_sim_for_select_athletes.RData")
simple_sim_df <- (simple_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)
simple_ribbon_df <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(points),
            quant2.5 = quantile(points, .025),
            quant97.5 = quantile(points, .975),
            quant5 = quantile(points, .05),
            quant95 = quantile(points, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

comp_sim <- read_rds(file = "results/comp_sim_for_select_athletes.RData")
comp_sim_df <- (comp_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)
comp_ribbon_df <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(points),
            quant2.5 = quantile(points, .025),
            quant97.5 = quantile(points, .975),
            quant5 = quantile(points, .05),
            quant95 = quantile(points, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

baseline_age_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = baseline_sim_ribbon_df,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = baseline_sim_ribbon_df,
            mapping = aes(x = age,
                          y = mean,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(x = "Age",
       y = "Points",
       color = "Name",
       fill = "Name",
       title = "Baseline")  + 
  theme(legend.position="none",
        axis.title.x=element_blank()) +
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) + 
  ylim(c(7000, 9200)) 

baseline_age_curve

simple_age_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = simple_ribbon_df,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(x = "Age",
       y = "Points",
       color = "Name",
       fill = "Name",
       title = "Simple")+
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) +
  theme(legend.position="none",
        axis.title.y=element_blank()) + 
  ylim(c(7000, 9200))
simple_age_curve



comp_age_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(x = "Age",
       y = "Points",
       color = "Name",
       fill = "Name",
       title = "Compositional")+
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) + 
  ylim(c(7000, 9200)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())  +
  theme(legend.position = "inside",
        legend.position.inside = c(0.65,0.1),
        legend.background = element_blank(),
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 7),
        legend.key.size = unit(.3, 'cm')) +
  guides(color = guide_legend(override.aes = list(size = 1)))
comp_age_curve  
(baseline_age_curve + simple_age_curve + comp_age_curve) + 
  plot_annotation(title = "95% Credible intervals for decathlon results by age")

ggsave("writing/decathlon_manu/figures/points_age_curve.png", width = 7, height = 4,
       units = "in")


# new event age curves ----------------------------------------------------
simple_ribbon_df_400m <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))
comp_ribbon_df_400m <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


simple_400m_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df_400m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = simple_ribbon_df_400m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Simple",
       x = "Age",
       y = "400m",
       fill ="Name",
       color = "Name") + 
  theme(legend.position="none")

comp_400m_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_400m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df_400m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Compositional",
       x = "Age",
       y = "400m",
       fill ="Name",
       color = "Name") + 
  theme(axis.title.y = element_blank())

simple_400m_curve + comp_400m_curve +
  plot_annotation(title = "95% Posterior predictive intervals for 400m results by age")
ggsave("writing/decathlon_manu/figures/400m_age_curve.png", width = 9, height = 6,
       units = "in")


simple_ribbon_df_javelin <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(javelin),
            quant2.5 = quantile(javelin, .025),
            quant97.5 = quantile(javelin, .975),
            quant5 = quantile(javelin, .05),
            quant95 = quantile(javelin, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

comp_ribbon_df_javelin <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(javelin),
            quant2.5 = quantile(javelin, .025),
            quant97.5 = quantile(javelin, .975),
            quant5 = quantile(javelin, .05),
            quant95 = quantile(javelin, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

simple_javelin_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = javelin,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df_javelin,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = simple_ribbon_df_javelin,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "Simple",
       x = "Age",
       y = "Javelin",
       color = "Name",
       fill = "Name") +
  theme(legend.position = "")
comp_javelin_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = javelin,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_javelin,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df_javelin,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "Compositional",
    x = "Age",
    y = "Javelin",
    color = "Name",
    fill = "Name") +
  theme(axis.title.y = element_blank())
simple_javelin_curve + comp_javelin_curve +
  plot_annotation(title = "95% Posterior predictive intervals for javelin results by age")

ggsave("writing/decathlon_manu/figures/javelin_age_curve.png", width = 9, height = 6,
       units = "in")

for (i in 1:10) {
  target_event = dec_events[i]
  print(c())
  for (j in dim(comp_sim$sims_list[[i]]$betaY)[2]) {
    predictor_event = dec_events[j]
    print(c(paste0("target: ", target_event, "; predictor: ", predictor_event, sep = " "), mean(comp_sim$sims_list[[i]]$betaY[,j])))
  }
  
}



# comp model graphs (event v event) ---------------------------------------
total_comp_results <- read_rds("results/new_stan_sim_results/new_stan_sim_compositional_none_cubic.RData")
for (i in 1:10) {
  target_event = dec_events[i]
  print(c())
  for (j in dim(total_comp_results$sims_list[[i]]$betaY)[2]) {
    predictor_event = dec_events[j]
    print(c(paste0("target: ", target_event, "; predictor: ", predictor_event, sep = " "), mean(comp_sim$sims_list[[i]]$betaY[,j])))
  }
  
}

comp_event_df <- data.frame(long_jump = comp_sim_df$long_jump,
                       hundred_m = comp_sim_df$hundred_m,
                       javelin = comp_sim_df$javelin,
                       athlete_id = comp_sim_df$athlete_id,
                       name = comp_sim_df$name) 
set.seed(2)
comp_sample_event_df <- comp_event_df[sample(1:nrow(comp_event_df), 10000),]
simple_event_df <- data.frame(long_jump = simple_sim_df$long_jump,
                            hundred_m = simple_sim_df$hundred_m,
                            javelin = simple_sim_df$javelin,
                            athlete_id = simple_sim_df$athlete_id,
                            name = simple_sim_df$name) 
set.seed(2)
simple_sample_event_df <- simple_event_df[sample(1:nrow(simple_event_df), 10000),]
simple_event_plot <- ggplot(data = simple_sample_event_df,
                            mapping = aes(x = hundred_m,
                                          y = long_jump, 
                                          color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "") +
  labs(x = "Predicted 100m", y = "Predicted Long Jump",
       title = "Simple") +
  ylim(c(6,8.5)) +
  xlim(c(9.75, 11.5))
comp_event_plot <- ggplot(data = comp_sample_event_df, 
                          mapping = aes(x = hundred_m, 
                                        y = long_jump,
                                        color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(axis.title.y=element_blank())+
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Predicted 100m", y = "Predicted Long Jump",
       title = "Compositional",
       color = "Name") +
  ylim(c(6, 8.5))+
  xlim(c(9.75, 11.5)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.1),
        legend.background = element_blank(),
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 12),
        legend.key.size = unit(.3, 'cm')) 

simple_event_plot + comp_event_plot + plot_annotation(title = "Predicted long jump scores vs predicted 100m times")
ggsave("writing/decathlon_manu/figures/event_v_event.png", width = 9, height = 6,
       units = "in")


simple_event_plot2 <- ggplot(data = simple_sample_event_df,
                            mapping = aes(x = long_jump,
                                          y = javelin, 
                                          color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "") +
  labs(x = "Predicted Long Jump", y = "Predicted Javelin",
       title = "Simple") +
  ylim(c(35,70)) +
  xlim(c(6,8.5))
comp_event_plot2 <- ggplot(data = comp_sample_event_df, 
                          mapping = aes(x = long_jump, 
                                        y = javelin,
                                        color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(axis.title.y=element_blank())+
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Predicted Long Jump", y = "Predicted Javelin",
       title = "Compositional",
       color = "Name") +
  ylim(c(35,70))+
  xlim(c(6,8.5)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.1),
        legend.background = element_blank(),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10),
        legend.key.size = unit(.3, 'cm')) 

simple_event_plot2 + comp_event_plot2 + plot_annotation(title = "Predicted javelin distances vs predicted long jump scores")
ggsave("writing/decathlon_manu/figures/event_v_event2.png", width = 9, height = 6,
       units = "in")





# remaking age curves with full data --------------------------------------
setwd("~/school/wisconsin/research_repo/decathlon")
study = "new_stan_sim"
data_dir = "new_stan_sim/data/"
script_dir = "new_stan_sim/study/"
stan_dir = "new_stan_sim/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))
source(paste0(script_dir, "settings_gen_data.R"))

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


age_vec <- c(seq(19, 30, by = .1),
             seq(19, 30, by = .1))
athlete_id <- c(rep(91, length(seq(19, 30, by = .1))),
                rep(310, length(seq(19, 30, by = .1))))
is_new_athlete <- rep(0, length(age_vec))
full_test_df <- online_data_filter %>%
  filter((subj_id == 239)|(subj_id == 759)) %>%
  mutate(status = case_when(((subj_id==239) & (age < 25) )|((subj_id==759) & (age < 25)) ~ "train",
                            ((subj_id==239) & (age >= 25) )|((subj_id==759) & (age >= 25)) ~ "test"))

full_baseline_sim <- get_baseline_cubic_sim(age_vec = age_vec,
                                       athlete_id = athlete_id,
                                       is_new_athlete = is_new_athlete,
                                       decathlon_data = dec_data_standard,
                                       stan_dir = stan_dir,
                                       iter = 2000,
                                       return_all = T)
full_simple_sim <- get_simple_cubic_sim(age_vec = age_vec,
                                   athlete_id = athlete_id,
                                   is_new_athlete = is_new_athlete,
                                   decathlon_data = dec_data_standard,
                                   event_sums = event_sums,
                                   stan_dir = stan_dir,
                                   iter = 2000,
                                   return_all = T)
full_comp_sim <- get_comp_cubic_sim(age_vec = age_vec,
                               athlete_id = athlete_id,
                               is_new_athlete = is_new_athlete,
                               decathlon_data = dec_data_standard,
                               event_sums = event_sums,
                               stan_dir = stan_dir,
                               iter = 2000,
                               return_all = T)
saveRDS(object = full_baseline_sim, file = "results/full_baseline_sim_for_select_athletes.RData")
saveRDS(object = full_simple_sim, file = "results/full_simple_sim_for_select_athletes.RData")
saveRDS(object = full_comp_sim, file = "results/full_comp_sim_for_select_athletes.RData")

baseline_sim <- read_rds(file = "results/full_baseline_sim_for_select_athletes.RData")
simple_sim <- read_rds(file = "results/full_simple_sim_for_select_athletes.RData")
comp_sim <- read_rds(file = "results/full_comp_sim_for_select_athletes.RData")
baseline_sim_ribbon_df <- data.frame(quant2.5 = apply((baseline_sim$post_sim), MARGIN = 2, 
                                                      function(x) quantile(x,.025)),
                                     quant97.5 = apply((baseline_sim$post_sim), MARGIN = 2, 
                                                       function(x) quantile(x,.975)),
                                     mean = apply((baseline_sim$post_sim), MARGIN = 2,function(x) mean(x)),
                                     age = age_vec,
                                     athlete = athlete_id) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

simple_sim_df <- (simple_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)
simple_ribbon_df <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(points),
            quant2.5 = quantile(points, .025),
            quant97.5 = quantile(points, .975),
            quant5 = quantile(points, .05),
            quant95 = quantile(points, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


comp_sim_df <- (comp_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)
comp_ribbon_df <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(points),
            quant2.5 = quantile(points, .025),
            quant97.5 = quantile(points, .975),
            quant5 = quantile(points, .05),
            quant95 = quantile(points, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

baseline_age_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = baseline_sim_ribbon_df,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = baseline_sim_ribbon_df,
            mapping = aes(x = age,
                          y = mean,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(x = "Age",
       y = "Points",
       color = "Name",
       fill = "Name",
       title = "Baseline")  + 
  theme(legend.position="none",
        axis.title.x=element_blank()) +
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) + 
  ylim(c(7000, 9200)) 

baseline_age_curve

simple_age_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = simple_ribbon_df,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(x = "Age",
       y = "Points",
       color = "Name",
       fill = "Name",
       title = "Simple")+
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) +
  theme(legend.position="none",
        axis.title.y=element_blank()) + 
  ylim(c(7000, 9200))
simple_age_curve



comp_age_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(x = "Age",
       y = "Points",
       color = "Name",
       fill = "Name",
       title = "Compositional")+
  scale_x_continuous(breaks = seq(18, 30, by = 4), 
                     lim = c(18, 30)) + 
  ylim(c(7000, 9200)) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
comp_age_curve  
(baseline_age_curve + simple_age_curve + comp_age_curve) + 
  plot_annotation(title = "95% Posterior predictive intervals for decathlon results by age")

ggsave("writing/decathlon_manu/figures/points_age_curve.png", width = 7, height = 4,
       units = "in")


# new event age curves ----------------------------------------------------
simple_ribbon_df_400m <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))
comp_ribbon_df_400m <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


simple_400m_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df_400m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = simple_ribbon_df_400m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Simple",
       x = "Age",
       y = "400m",
       fill ="Name",
       color = "Name") + 
  theme(legend.position="none")

comp_400m_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_400m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df_400m,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "Compositional",
       x = "Age",
       y = "400m",
       fill ="Name",
       color = "Name") + 
  theme(axis.title.y = element_blank())

simple_400m_curve + comp_400m_curve +
  plot_annotation(title = "95% Credible intervals for 400m results by age")
ggsave("writing/decathlon_manu/figures/400m_age_curve.png", width = 9, height = 6,
       units = "in")


simple_ribbon_df_javelin <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(javelin),
            quant2.5 = quantile(javelin, .025),
            quant97.5 = quantile(javelin, .975),
            quant5 = quantile(javelin, .05),
            quant95 = quantile(javelin, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

comp_ribbon_df_javelin <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(javelin),
            quant2.5 = quantile(javelin, .025),
            quant97.5 = quantile(javelin, .975),
            quant5 = quantile(javelin, .05),
            quant95 = quantile(javelin, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

simple_javelin_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = javelin,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df_javelin,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = simple_ribbon_df_javelin,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "Simple",
       x = "Age",
       y = "Javelin",
       color = "Name",
       fill = "Name") +
  theme(legend.position = "")
comp_javelin_curve <- ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = javelin,
                           color = name)) +
  geom_ribbon(data = comp_ribbon_df_javelin,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2,
              linetype = 3) +
  geom_line(data = comp_ribbon_df_javelin,
            mapping = aes(x = age,
                          y = mean_var,
                          color = name))+ 
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "Compositional",
       x = "Age",
       y = "Javelin",
       color = "Name",
       fill = "Name") +
  theme(axis.title.y = element_blank())
simple_javelin_curve + comp_javelin_curve +
  plot_annotation(title = "95% Credible intervals for javelin results by age")

ggsave("writing/decathlon_manu/figures/javelin_age_curve.png", width = 9, height = 6,
       units = "in")

for (i in 1:10) {
  target_event = dec_events[i]
  print(c())
  for (j in dim(comp_sim$sims_list[[i]]$betaY)[2]) {
    predictor_event = dec_events[j]
    print(c(paste0("target: ", target_event, "; predictor: ", predictor_event, sep = " "), mean(comp_sim$sims_list[[i]]$betaY[,j])))
  }
  
}

total_comp_results <- read_rds("results/new_stan_sim_results/new_stan_sim_compositional_none_cubic.RData")
for (i in 1:10) {
  target_event = dec_events[i]
  print(c())
  for (j in dim(total_comp_results$sims_list[[i]]$betaY)[2]) {
    predictor_event = dec_events[j]
    print(c(paste0("target: ", target_event, "; predictor: ", predictor_event, sep = " "), mean(comp_sim$sims_list[[i]]$betaY[,j])))
  }
  
}

comp_event_df <- data.frame(long_jump = comp_sim_df$long_jump,
                            hundred_m = comp_sim_df$hundred_m,
                            javelin = comp_sim_df$javelin,
                            athlete_id = comp_sim_df$athlete_id,
                            name = comp_sim_df$name) 
set.seed(2)
comp_sample_event_df <- comp_event_df[sample(1:nrow(comp_event_df), 10000),]
simple_event_df <- data.frame(long_jump = simple_sim_df$long_jump,
                              hundred_m = simple_sim_df$hundred_m,
                              javelin = simple_sim_df$javelin,
                              athlete_id = simple_sim_df$athlete_id,
                              name = simple_sim_df$name) 
set.seed(2)
simple_sample_event_df <- simple_event_df[sample(1:nrow(simple_event_df), 10000),]
simple_event_plot <- ggplot(data = simple_sample_event_df,
                            mapping = aes(x = hundred_m,
                                          y = long_jump, 
                                          color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "") +
  labs(x = "Predicted 100m", y = "Predicted Long Jump",
       title = "Simple") +
  ylim(c(6,8.5))
comp_event_plot <- ggplot(data = sample_event_df, 
                          mapping = aes(x = hundred_m, 
                                        y = long_jump,
                                        color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(axis.title.y=element_blank())+
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Predicted 100m", y = "Predicted Long Jump",
       title = "Compositional",
       color = "Name") +
  ylim(c(6, 8.5))

simple_event_plot + comp_event_plot + plot_annotation(title = "Predicted long jump scores vs predicted 100m times")
ggsave("writing/decathlon_manu/figures/event_v_event.png", width = 9, height = 6,
       units = "in")


simple_event_plot2 <- ggplot(data = simple_sample_event_df,
                             mapping = aes(x = long_jump,
                                           y = javelin, 
                                           color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  theme(legend.position = "") +
  labs(x = "Predicted Long Jump", y = "Predicted Javelin",
       title = "Simple") +
  ylim(c)
comp_event_plot2 <- ggplot(data = comp_sample_event_df, 
                           mapping = aes(x = long_jump, 
                                         y = javelin,
                                         color = name)) +
  geom_point(alpha = .1) +
  theme_bw() +
  theme(axis.title.y=element_blank())+
  scale_color_brewer(type = "qual") + 
  geom_smooth(method = "lm", se = F) +
  labs(x = "Predicted Long Jump", y = "Predicted Javelin",
       title = "Compositional",
       color = "Name") 

simple_event_plot2 + comp_event_plot2 + plot_annotation(title = "Predicted javelin distances vs predicted long jump scores")
ggsave("writing/decathlon_manu/figures/event_v_event2.png", width = 9, height = 6,
       units = "in")


# make beta histograms ----------------------------------------------------
long_jump_sd <- event_sums %>%
  filter(event == "long_jump") %>%
  pull(sd_score)
javelin_sd  <- event_sums %>%
  filter(event == "javelin") %>%
  pull(sd_score)
hundred_m_sd <- event_sums %>%
  filter(event == "hundred_m") %>%
  pull(sd_score)

lh_hundred_m_df <- data.frame(beta = comp_sim$sims_list$long_jump_sims$betaY) %>%
  mutate(new_beta = beta * long_jump_sd / hundred_m_sd)
hist_1 <- ggplot(data = lh_hundred_m_df,
       mapping = aes(x = new_beta)) +
  geom_histogram(color = "white") +
  labs(x = "Beta",
       title = "LJ x 100m") +
  geom_vline(xintercept = mean(lh_hundred_m_df$new_beta),
             color = "tomato") +
  geom_vline(xintercept = quantile(lh_hundred_m_df$new_beta, c(.025, .975)),
             color = "tomato",
             linetype = 3) +
  theme_bw()

javelin_lj_df <- data.frame(beta = comp_sim$sims_list$javelin_sims$betaY[,2]) %>%
  mutate(new_beta = beta * javelin_sd / long_jump_sd)
hist_2 <- ggplot(data = javelin_lj_df,
       mapping = aes(x = new_beta)) +
  geom_histogram(color = "white") +
  labs(x = "Beta",
       title = "Javelin x LJ") +
  geom_vline(xintercept = mean(javelin_lj_df$new_beta),
             color = "tomato") +
  geom_vline(xintercept = quantile(javelin_lj_df$new_beta, c(.025, .975)),
             color = "tomato",
             linetype = 3) +
  theme_bw() +
  theme(axis.title.y = element_blank())
hist_1 + hist_2 + plot_annotation(title = "Beta coefficients for inter-event relationships")
ggsave("writing/decathlon_manu/figures/beta_hist.png", width = 9, height = 6,
       units = "in")



# full comp sim top athletes ----------------------------------------------



set.seed(2)
library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(mvtnorm)

setwd("~/school/wisconsin/research_repo/decathlon")
study = "new_stan_sim"
data_dir = "new_stan_sim/data/"
script_dir = "new_stan_sim/study/"
stan_dir = "new_stan_sim/stan_mods/"

source(paste0(script_dir, "decathlon_funs.R"))
source(paste0(script_dir, "settings_gen_data.R"))

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

top_athletes <- dec_data_standard %>%
  group_by(athlete_id, name) %>%
  summarize(max_points = max(points)) %>%
  arrange(desc(max_points)) %>%
  head(10)
test_df <- expand_grid(age_vec = seq(19,30, by = .1),
                       athlete_id = top_athletes$athlete_id,
                       is_new_athlete = 0)
age_vec <- test_df$age_vec
athlete_id <- test_df$athlete_id
is_new_athlete <- test_df$is_new_athlete

comp_results_top_athletes <- get_comp_cubic_sim(age_vec = age_vec,
                               athlete_id = athlete_id,
                               is_new_athlete = is_new_athlete,
                               decathlon_data = dec_data_standard,
                               event_sums = event_sums,
                               stan_dir = stan_dir,
                               iter = 2000,
                               return_all = T)

saveRDS(object = comp_results_top_athletes, file = "results/comp_results_top_athletes.RData")





# best tab ----------------------------------------------------------------



comp_results_top_athletes <- read_rds("results/baseline_sim_for_top_athletes.RData")
sim_events <- comp_results_top_athletes$sim_events
sim_events$athlete_id <- athlete_id
sim_events <- sim_events %>%
  select(-athlete, -row_num)
test <- sim_events %>%
       filter(athlete_id == 478)
ggplot(data = test, mapping = aes(x = age, y = calc_point)) +
  geom_point(alpha = .2)

best_tab <- data.frame(event = rep(0, 14))
best_tab$event <- c("100m", "Long jump", "Shot put", "High jump", "400m", "Hurdles", "Discus Throw", "Pole vault", "Javelin throw", "1500m", "Total", "Sprints", "Jumps", "Throws")
best_tab$real_best <- c(10.55,
                        7.80,
                        16.00,
                        2.05,
                        48.42,
                        13.75,
                        50.54,
                        5.45,
                        71.90,
                        276,
                        9126,
                        0.39,
                        0.32,
                        0.29)
best_tab$virtual_best <- c(10.21,
                           8.23,
                           15.40,
                           2.11,
                           45.00,
                           13.35,
                           47.36,
                           5.40,
                           66.64,
                           254,
                           9543,
                           0.42,
                           0.32,
                           0.26)
best_tab$sim_best <- c(10.64,
                       7.86,
                       16.88,
                       2.16,
                       46.09,
                       13.18,
                       51.39,
                       5.98,
                       70.04,
                       239,
                       9887,
                       0.40,
                       0.32,
                       0.27)

best_tab$decathlon_best <- c(10.12,
                             8.45,
                             19.17,
                             2.28,
                             45.00,
                             13.27,
                             57.70,
                             5.76,
                             79.80,
                             239,
                             10681,
                             0.39,
                             0.32, 
                             0.29)
colnames(best_tab) <- c("Event", "Real Best", "Virtual Best", "Simulated Best",  "Decathlon Best")

best_tab_calc <- best_tab[-(11:nrows(best_tab)),]
best_tab_calc$event <- dec_events
best_tab_calc <- best_tab_calc %>% 
  select(-Event) %>%
  pivot_longer(cols = ends_with("Best"),
               names_to = "best_type",
               values_to = "P") %>%
  mutate(P = case_when(event %in% c("long_jump", "high_jump", "pole_vault") ~ P*100,
                       .default = P),
        event_type = case_when(event %in% c("hundred_m",
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
                                 event_type == "field" ~ floor(A * ((P - B)^C))),
        event_group = case_when(event %in% c("hundred_m", 
                                             "four_hundred_m", 
                                             "hurdles", 
                                             "fifteen_hundred_m") ~ "sprints",
                                event %in% c("long_jump", "high_jump", "pole_vault") ~ "jumps",
                                event %in% c("shot_put", "discus", "javelin") ~ "throws")
        
        ) 
prop_df <- best_tab_calc %>%
  group_by(best_type) %>%
  mutate(total_points = sum(calc_points)) %>%
  group_by(best_type, event_group) %>%
  reframe(point_prop = round(sum(calc_points) / total_points, 2)) %>%
  unique()


best_tab %>%
  kable(caption = "Breakdown of maximal decathlon performances. Real best denotes Kevin Mayer's decathlon world record. Virtual best combines Ashton Eaton's decathlon personal bests. Simulated best contains the maximal score from a single decathlon through our simulations. Decathlon best is the theoretical maximum score from the best decathlon performances from all athletes. We have also calculated the proportion of points from each event grouping: sprints, jumps, and throws.",
        escape = F,
        label = "best_table",
        format = "latex",
        booktabs = T) # Output format = latex 
################ old code ----------------------------------------------------------------

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
simple_sim_df <- (simple_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)

simple_ribbon_df <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(points),
            quant2.5 = quantile(points, .025),
            quant97.5 = quantile(points, .975),
            quant5 = quantile(points, .05),
            quant95 = quantile(points, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "95% Credible intervals for future decathlon points (Eaton and Williams)")+
  scale_x_continuous(breaks = seq(18, 30, by = 2), 
                     lim = c(18, 30))



ggsave("writing/decathlon_manu/figures/simple_point_age_curve.png", width = 9, height = 6,
       units = "in")

simple_ribbon_df_400m <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df_400m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "95% Credible intervals for future 400m (Eaton and Williams)",
       x = "Age",
       y = "400m",
       fill ="Name",
       color = "Name")
ggsave("writing/decathlon_manu/figures/simple_400m_age_curve.png", width = 9, height = 6,
       units = "in")


simple_ribbon_df_javelin <- simple_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(javelin),
            quant2.5 = quantile(javelin, .025),
            quant97.5 = quantile(javelin, .975),
            quant5 = quantile(javelin, .05),
            quant95 = quantile(javelin, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = javelin,
                           color = name)) +
  geom_ribbon(data = simple_ribbon_df_javelin,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  theme_bw() + 
  labs(title = "95% Credible intervals for future javelin (Eaton and Williams)",
       x = "Age",
       y = "Javelin",
       color = "Name",
       fill = "Name")
ggsave("writing/decathlon_manu/figures/simple_javelin_age_curve.png", width = 9, height = 6,
       units = "in")


comp_sim <- get_comp_cubic_sim(age_vec = age_vec,
                          athlete_id = athlete_id,
                          is_new_athlete = is_new_athlete,
                          decathlon_data = train_df,
                          event_sums = event_sums,
                          stan_dir = stan_dir,
                          iter = 2000,
                          return_all = T)
comp_sim_df <- (comp_sim$sim_events) %>%
  mutate(athlete_id = case_when(athlete <= 111 ~ 91,
                                athlete > 111 ~ 310),
         name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS
"))%>%
  select(-c(athlete, row_num)) %>%
  rename(points = calc_point)
                                    
ribbon_df <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(points),
            quant2.5 = quantile(points, .025),
            quant97.5 = quantile(points, .975),
            quant5 = quantile(points, .05),
            quant95 = quantile(points, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))

ggplot(data = full_test_df  %>%
         filter(status == "train"),
       mapping = aes(x = age,
                     y = points,
                     color = name)) +
  geom_point() +
  scale_color_brewer(type = "qual")

ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = points,
                           color = name)) +
  geom_ribbon(data = ribbon_df, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "95% Credible intervals for future decathlon points (Eaton and Williams)")+
  scale_x_continuous(breaks = seq(18, 30, by = 2), 
                     lim = c(18, 30))
ggsave("writing/decathlon_manu/figures/intro_plot_with_intervals_points.png", width = 6, height = 4,
       units = "in")

## make same graph, but with different variables
ribbon_df_400m <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(four_hundred_m),
            quant2.5 = quantile(four_hundred_m, .025),
            quant97.5 = quantile(four_hundred_m, .975),
            quant5 = quantile(four_hundred_m, .05),
            quant95 = quantile(four_hundred_m, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = four_hundred_m,
                           color = name)) +
  geom_ribbon(data = ribbon_df_400m, 
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual")  +
  theme_bw() + 
  labs(title = "95% Credible intervals for future 400m (Eaton and Williams)")
ggsave("writing/decathlon_manu/figures/intro_plot_with_intervals_400m.png", width = 6, height = 4,
       units = "in")


ribbon_df_javelin <- comp_sim_df %>%
  group_by(athlete_id, age) %>%
  summarize(mean_var = mean(javelin),
            quant2.5 = quantile(javelin, .025),
            quant97.5 = quantile(javelin, .975),
            quant5 = quantile(javelin, .05),
            quant95 = quantile(javelin, .95)) %>%
  mutate(name = case_when(athlete_id == 91 ~ "Ashton EATON",
                          athlete_id == 310 ~ "Harrison WILLIAMS"))


ggplot() +
  geom_point(data = full_test_df,
             mapping = aes(x = age,
                           y = javelin,
                           color = name)) +
  geom_ribbon(data = ribbon_df_javelin,
              mapping = aes(x = age,
                            ymin = quant2.5,
                            ymax = quant97.5,
                            color = name,
                            fill = name),
              alpha = .2) +
  scale_color_brewer(type = "qual") +
  scale_fill_brewer(type = "qual") +
  geom_line(data = ribbon_df_javelin %>% 
              filter(((athlete_id == 1) & (age >= 22)) |
                       ((athlete_id == 773) &(age >= 30)))
            , mapping = aes(x = age, y = mean_var,
                            color = name))  +
  theme_bw() + 
  labs(title = "95% Credible intervals for future javelin (Eaton and Williams)")
ggsave("writing/decathlon_manu/figures/intro_plot_with_intervals_javelin.png", width = 6, height = 4,
       units = "in")


saveRDS(object = baseline_sim, file = "results/baseline_sim_for_select_athletes.RData")
saveRDS(object = simple_sim, file = "results/simple_sim_for_select_athletes.RData")
saveRDS(object = comp_sim, file = "results/comp_sim_for_select_athletes.RData")
