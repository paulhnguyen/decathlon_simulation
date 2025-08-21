# July 15, 2025
# Code to make best results tables in decathlon paper


set.seed(2)
library(tidyverse)
library(readxl)
library(knitr)

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




# descriptive graphs data -------------------------------------------------

ggplot(online_data_filter, mapping = aes(x = points)) + geom_histogram(color = "white") +
  theme_bw() +
  labs(x = "Points", y = "Count", title = "Distribution of points")
ggsave("writing/decathlon_manu/figures/point_hist.png", width = 6, height = 4,
       units = "in")
ggplot(online_data_filter, mapping = aes(x = age)) + geom_histogram(color = "white") +
  theme_bw() +
  labs(x = "Age", y = "Count", title = "Distribution of ages")
ggsave("writing/decathlon_manu/figures/age_hist.png", width = 6, height = 4,
       units = "in")
target <- c("age", dec_events, "points")

d_table <- online_data_filter %>%
  select(dec_events, age, points, -name, -dob) %>%
  pivot_longer(cols = c(dec_events, age, points),
               names_to = "Variable",
               values_to = "Value") %>%
  group_by(Variable) %>%
  summarize(Mean = mean(Value),
            Median = median(Value),
            SD = sd(Value))
d_table <- d_table[match(target, d_table$Variable),]
d_table$Variable <- c("Age", "100m", "LJ", "SP", "HJ", "400m", "110mH", "DT", "PV", "JV", "1500m", "Points")

d_table %>%
  kable(caption = "Descriptive statistics for decathlon events, age, and overall points.",
        escape = F,
        digits = 2,
        label = "descriptive_table",
        format = "latex",
        booktabs = T) #

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


comp_results_top_athletes <- read_rds("results/comp_results_top_athletes.RData")
sim_events <- comp_results_top_athletes$sim_events
sim_events$athlete_id <- athlete_id
sim_events <- sim_events %>%
  select(-athlete, -row_num)

sim_df_ribbon <- sim_events %>%
  group_by(athlete_id, age) %>%
  summarize(q2.5 = quantile(calc_point, 0.025),
            q97.5 = quantile(calc_point, 0.975))

test <- sim_events %>%
  filter(athlete_id == 478)
test_ribbon <- sim_df_ribbon %>%
  filter(athlete_id == 478)
ggplot() +
  geom_point(data = sim_events %>%
               filter(athlete_id == 478), mapping = aes(x = age, y = calc_point),
             alpha = .2) +
  geom_point(data = sim_events %>%
               filter(athlete_id == 478) %>%
               filter(calc_point == 9887),
             mapping = aes(x = age, y = calc_point),
             color = "tomato") +
  geom_ribbon(data = sim_df_ribbon %>%
                filter(athlete_id == 478),
              mapping = aes(x = age, ymin = q2.5, ymax = q97.5),
              color = "tomato")

ggplot() +
  geom_point(data = sim_events %>%
               filter(athlete_id == 778), mapping = aes(x = age, y = calc_point),
             alpha = .2) +
  geom_ribbon(data = sim_df_ribbon %>%
                filter(athlete_id == 778),
              mapping = aes(x = age, ymin = q2.5, ymax = q97.5),
              color = "tomato")


sim_df_ribbon %>%
       group_by(athlete_id) %>%
       summarize(q = max(q97.5)) %>%
       arrange(desc(q))
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