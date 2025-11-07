# Aug 28, 2025
# Code to make initial motivating graph for decathlon paper, as well as graphs for experiment section.


set.seed(2)
library(tidyverse)
library(readxl)
library(rstan)
library(rlist)
library(tidyselect)
library(patchwork)
library(forcats)
library(knitr)


study = "study"
data_dir = "data/"
script_dir = "study/"
stan_dir = "stan_mods/"

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

event_sums <- get_event_sums_df(online_data_filter)



# points age curve --------------------------------------------------------

# read in age curve. simulation from code_to_generate_sims.R
baseline_sim <- read_rds(file = "../results/baseline_sim_for_select_athletes.RData")
simple_sim <- read_rds(file = "../results/simple_sim_for_select_athletes.RData")
comp_sim <- read_rds(file = "../results/comp_sim_for_select_athletes.RData")
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



# make histogram for event x event coefficients ----------------------------------------------------
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