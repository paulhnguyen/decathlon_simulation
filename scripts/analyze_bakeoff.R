# This file assumes you have a results directory, containing the results from from running the bakeoff study.
dir = "../../results/mse_table_pred_bakeoff/"

library(tidyverse)
library(knitr)
library(ggplot2)
library(patchwork)
source("../study/decathlon_funs.R")

data_dir = "../data/"
online_data_filter <- read_csv(paste0(data_dir,"online_data_filter.csv")) 
event_sums <- get_event_sums_df(online_data_filter)

### load results ###

# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(dir, list.files(dir), sep = "")
results <- read_csv(files[1]) %>%
  mutate(pred_type = "athlete")
for (i in 2:length(files)) {
  if (grepl( "athlete", files[i], fixed = TRUE)) {
    pred_type = "athlete"
  }  else if (grepl( "future", files[i], fixed = TRUE)) {
    pred_type = "future"
  } else {
    pred_type = "general"
  }
  results <- results %>%
    rbind(read_csv(files[i]) %>%
            mutate(pred_type = pred_type))
}

results %>%
  group_by(pred_type) %>%
  filter(event == "points") %>%
  summarise(n = n())

results %>%
  group_by(pred_type, type, prior, comp, event) %>%
  summarise(n = n()) %>%
  print(n = 1000) %>%
  arrange((n))

results %>%
  group_by(pred_type, type, comp) %>%
  summarize(mean_smse = mean(smse))

gen_results <- results %>%
  filter(pred_type == "general",
         prior != "joint") %>%
  mutate(prior = fct_relevel(prior,
                             "moment",
                             "none"),
         comp = fct_relevel(comp,
                            "compositional",
                            "simple",
                            "baseline"),
         type = fct_relevel(type,
                            "cubic",
                            "spline")
         )
future_results <- results %>%
  filter(pred_type == "future",
         prior != "joint") %>%
  mutate(prior = fct_relevel(prior,
                             "moment",
                             "none"),
         comp = fct_relevel(comp,
                            "compositional",
                            "simple",
                            "baseline"),
         type = fct_relevel(type,
                            "cubic",
                            "spline")
  )

gen_results %>%
  filter(event == "points") %>%
  group_by(comp, prior, type) %>%
  summarize(mean_smse = mean(smse)) %>%
  arrange((mean_smse))

gen_results %>%
  filter(event == "points",
         prior == "none",
         type == "cubic")  %>%
  group_by(comp) %>%
  summarize(median_smse = median(smse))

ggplot(data = gen_results %>%
         filter(event == "points"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = prior,
                     linetype  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "Open-Event Data",
       linetype = "f(age)",
       title = "SMSE's for predicting overall points (general) by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 

#removing open event data
ggplot(data = gen_results %>%
         filter(event == "points",
                prior == "none"),
       mapping = aes(x = smse,
                     y = comp,
                     fill  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "f(age)",
       title = "SMSE's for predicting overall points (general) by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 

gen_results %>%
  filter(event == "points") %>%
  group_by(comp, type, prior) %>%
  summarize(mean_smse = mean(smse))

ggplot(data = future_results %>%
         filter(event == "points",
                prior == "none"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "f(age)",
       title = "SMSE's for predicting overall points (tail) by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 

ggsave("../writing/decathlon_manu/figures/tail_point_boxplot.png", width = 6, height = 4,
       units = "in")

# we have confirmed they are not identical
ggplot(data = gen_results %>%
         filter(event == "fifteen_hundred_m"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = prior,
                     linetype  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "Open-Event Data",
       linetype = "f(age)",
       title = "SMSE's for predicting 100m by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 


ggplot(data = gen_results %>%
         filter(event == "four_hundred_m"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = prior,
                     linetype  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "Open-Event Data",
       linetype = "f(age)",
       title = "SMSE's for predicting 400m by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 

# tail results
future_results <- results %>%
  filter(pred_type == "future",
         prior != "joint") %>%
  mutate(prior = fct_relevel(prior,
                             "moment",
                             "none"),
         comp = fct_relevel(comp,
                            "compositional",
                            "simple",
                            "baseline"),
         type = fct_relevel(type,
                            "cubic",
                            "spline")
  )
future_results %>%
  filter(event == "points") %>%
  group_by(comp, prior, type) %>%
  summarize(mean_smse = mean(smse)) %>%
  arrange((mean_smse))
ggplot(data = future_results %>%
         filter(event == "points"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = prior,
                     linetype  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "Open-Event Data",
       linetype = "f(age)",
       title = "SMSE's for predicting overall points by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 


ggplot(data = future_results %>%
         filter(event == "hundred_m"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = prior,
                     linetype  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "Open-Event Data",
       linetype = "f(age)",
       title = "SMSE's for predicting 100m by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 


ggplot(data = future_results %>%
         filter(event == "four_hundred_m"),
       mapping = aes(x = smse,
                     y = comp,
                     fill = prior,
                     linetype  = type)) +
  geom_boxplot(alpha = .8) +
  labs(x = "SMSE",
       y = "Model",
       fill = "Open-Event Data",
       linetype = "f(age)",
       title = "SMSE's for predicting 400m by model") +
  theme_bw()+
  scale_fill_brewer(type = "qual") 




results_filtered <- results %>%
  filter(event == 'points')
results_filtered_comp <- results_filtered %>%
  filter(comp == "compositional",
         prior == "none",
         pred_type == "general",
         type == "cubic") 
results_filtered_simp <- results_filtered %>%
  filter(comp == "simple",
         prior == "none",
         pred_type == "general",
         type == "cubic") 
summary(results_filtered_comp)
summary(results_filtered_simp)
results_combined <- rbind(results_filtered_comp, results_filtered_simp) 

target <- c("baseline none", "simple none", "simple moment", "compositional none", "compositional moment")


# making tables with SMSE's -----------------------------------------------



general_table_cube <- results  %>%
  filter(pred_type == "general") %>%
  mutate(model_prior = paste0(comp, " ", prior)) %>%
  group_by(event,
           model_prior,
           type
           ) %>%
  summarize(mean_smse = round(mean(smse), digits = 3)) %>%
  pivot_wider(names_from = event,
              values_from = mean_smse) %>%
  select(model_prior, type, hundred_m,
         long_jump, shot_put, high_jump,
         four_hundred_m, hurdles, discus,
         pole_vault, javelin,
         fifteen_hundred_m, points) %>%
  filter(type == "cubic")
general_table_cube <- general_table_cube[match(target, general_table_cube$model_prior),]
names(general_table_cube) <- c("model prior" , "f(age)", "100m" , "LJ",
                          "SP" , "HJ" , "400m" , "110mH",
                          "DT", "PV" , "JV" , "1500m" ,
                          "points")

general_table_spline <- results  %>%
  filter(pred_type == "general") %>%
  mutate(model_prior = paste0(comp, " ", prior)) %>%
  group_by(event,
           model_prior,
           type
  ) %>%
  summarize(mean_smse = round(mean(smse), digits = 3)) %>%
  pivot_wider(names_from = event,
              values_from = mean_smse) %>%
  select(model_prior, type, hundred_m,
         long_jump, shot_put, high_jump,
         four_hundred_m, hurdles, discus,
         pole_vault, javelin,
         fifteen_hundred_m, points) %>%
  filter(type == "spline")
general_table_spline <- general_table_spline[match(target, general_table_spline$model_prior),]
names(general_table_spline) <- c("model prior" , "f(age)", "100m" , "LJ",
                               "SP" , "HJ" , "400m" , "110mH",
                               "DT", "PV" , "JV" , "1500m" ,
                               "points")



future_table_cube <- results  %>%
  filter(pred_type == "future") %>%
  mutate(model_prior = paste0(comp, " ", prior)) %>%
  group_by(event,
           model_prior,
           type
  )%>%
  summarize(mean_smse = round(mean(smse), digits = 3)) %>%
  pivot_wider(names_from = event,
              values_from = mean_smse) %>%
  select(model_prior, type, hundred_m,
         long_jump, shot_put, high_jump,
         four_hundred_m, hurdles, discus,
         pole_vault, javelin,
         fifteen_hundred_m, points)%>%
  filter(type == "cubic")
future_table_cube <- future_table_cube[match(target, future_table_cube$model_prior),]
names(future_table_cube) <- c("model prior" , "f(age)", "100m" , "LJ",
                         "SP" , "HJ" , "400m" , "110mH",
                         "DT", "PV" , "JV" , "1500m" ,
                         "points")
future_table_spline <- results  %>%
  filter(pred_type == "future") %>%
  mutate(model_prior = paste0(comp, " ", prior)) %>%
  group_by(event,
           model_prior,
           type
  )%>%
  summarize(mean_smse = round(mean(smse), digits = 3)) %>%
  pivot_wider(names_from = event,
              values_from = mean_smse) %>%
  select(model_prior, type, hundred_m,
         long_jump, shot_put, high_jump,
         four_hundred_m, hurdles, discus,
         pole_vault, javelin,
         fifteen_hundred_m, points)%>%
  filter(type == "spline")
future_table_spline <- future_table_spline[match(target, future_table_spline$model_prior),]
names(future_table_spline) <- c("model prior" , "f(age)", "100m" , "LJ",
                              "SP" , "HJ" , "400m" , "110mH",
                              "DT", "PV" , "JV" , "1500m" ,
                              "points")


athlete_table <- results  %>%
  filter(pred_type == "athlete") %>%
  mutate(model_prior = paste0(comp, " ", prior)) %>%
  group_by(event,
           model_prior
  ) %>%
  summarize(mean_smse = round(mean(smse), digits = 3)) %>%
  pivot_wider(names_from = event,
              values_from = mean_smse) %>%
  select(model_prior, hundred_m,
         long_jump, shot_put, high_jump,
         four_hundred_m, hurdles, discus,
         pole_vault, javelin,
         fifteen_hundred_m, points)
athlete_table <- athlete_table[match(target, athlete_table$model_prior),]
names(athlete_table) <- c("model prior" , "100m" , "LJ",
                             "SP" , "HJ" , "400m" , "110mH",
                             "DT", "PV" , "JV" , "1500m" ,
                             "points")






general_table_cube %>%
  kable(caption = "Mean standardized MSE across 10 cross validations in predicting decathlon performance with randomly removed observations. ",
        escape = F,
        label = "gen_table",
        format = "latex",
        booktabs = T) # Output format = latex 
general_table_spline %>%
  kable(caption = "Mean standardized MSE across 10 cross validations in predicting decathlon performance with randomly removed observations. ",
        escape = F,
        label = "gen_table",
        format = "latex",
        booktabs = T) # Output format = latex 
future_table_cube %>%
  kable(caption = "Mean standardized MSE across 10 cross validations in predicting decathlon performance with tail-removed observations. ",
        escape = F,
        label = "future_table",
        format = "latex",
        booktabs = T) # Output format = latex 
future_table_spline %>%
  kable(caption = "Mean standardized MSE across 10 cross validations in predicting decathlon performance with tail-removed observations. ",
        escape = F,
        label = "future_table",
        format = "latex",
        booktabs = T) # Output format = latex 

athlete_table %>%
  kable(caption = "Mean standardized MSE across 10 cross validations in predicting decathlon performance with removed athletes. ",
        escape = F,
        label = "athlete_table",
        format = "latex",
        booktabs = T) # Output format = latex 


decathlon_scoring <- data.frame(
  Event = c("100 m", "Long jump", "Shot put", "High jump", "400 m", 
            "110 m hurdles", "Discus throw", "Pole vault", "Javelin throw", "1500 m"),
  a = c(25.4347, 0.14354, 51.39, 0.8465, 1.53775,
        5.74352, 12.91, 0.2797, 10.14, 0.03768),
  b = c(18, 220, 1.5, 75, 82,
        28.5, 4, 100, 7, 480),
  c = c(1.81, 1.4, 1.05, 1.42, 1.81,
        1.92, 1.1, 1.35, 1.08, 1.85)
) %>%
  mutate(a = round(a, digits = 3),
         b = round(b, digits = 3),
         c = round(c, digits = 3))

decathlon_scoring %>%
  kable(caption = "Parameter values by event for point calculation ",
        escape = F,
        label = "point_params",
        format = "latex",
        booktabs = T)
