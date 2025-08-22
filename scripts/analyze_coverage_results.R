setwd("~/school/wisconsin/research_repo/decathlon/new_stan_sim")
results_dir = "../results/coverage_results/"
data_dir = "data/"

library(tidyverse)
library(knitr)
library(ggplot2)
library(patchwork)
source("study/decathlon_funs.R")
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
sim_beta_coef_list <- readRDS(paste0(data_dir, "beta_list_sim_manual.RData"))

### load results ###

# loads .RData object
loadRData <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

# gets names of all the files in dir
files = stringr::str_c(results_dir, list.files(results_dir), sep = "")
results <- read_csv(files[1]) 
for (i in 2:length(files)) {
  results <- results %>%
    rbind(read_csv(files[i]))
}


# add true coefficients to results
for (i in 1:nrow(results)) {
  target_id <- results$target[i]
  predictor_id <- results$predictor[i]
  if (predictor_id == "age2") {
    predictor_id = "I(age^2)"
  } else if (predictor_id == "age3"){
    predictor_id = "I(age^3)"
  }
  beta_coef <- sim_beta_coef_list[[target_id]][predictor_id]
  results$true_coef[i] <- beta_coef
}
results <- results %>%
  mutate(cov_check = (true_coef < ub) & (true_coef > lb))


comp_prop <- results %>%
  filter(comp == "compositional",
         type != "spline") %>%
  group_by(target, predictor, type, comp) %>%
  summarize(prop = sum(cov_check) / n()) %>%
  drop_na()




# testing why we missed ---------------------------------------------------
test <- results %>%
  filter(target == "hundred_m",
         predictor == "age2")
sum(test$cov_check)

sim_data_list <- readRDS(paste0(data_dir, "sim_data_list_manual.RData"))
iter = 1
sim_data <- sim_data_list[[iter]]
event_sums <- get_event_sums_df(sim_data)
dec_data_standard <- standardize_decathlon_data(sim_data,  event_sums)


age_vec <- c(0,0)
athlete_id <- rep(0, length(age_vec))
is_new_athlete <- rep(1, length(age_vec))
stan_dir = "stan_mods/"
sim <- get_comp_cubic_sim(age_vec = age_vec,
                          athlete_id = athlete_id,
                          is_new_athlete = is_new_athlete,
                          decathlon_data = dec_data_standard,
                          event_sums = event_sums,
                          stan_dir = stan_dir,
                          iter = 2000,
                          return_all = T)
# 
# sim_df_100m <- data.frame(age = dec_data_standard$age,
#                           age2 = dec_data_standard$age^2,
#                           age3 = dec_data_standard$age^3,
#                           index = sample(2000:4000, size = length(dec_data_standard$age), replace = T))
#                           
# sim_df_100m <- sim_df_100m %>%
#   mutate(mu = sim$sims_list$hundred_m_sims$mu[index],
#          beta1 = sim$sims_list$hundred_m_sims$beta1[index],
#          beta2 = sim$sims_list$hundred_m_sims$beta2[index],
#          beta3 = sim$sims_list$hundred_m_sims$beta3[index],
#          mean = mu + (beta1*age) + (beta2*age2) + (beta3*age3),
#          sigma = sim$sims_list$hundred_m_sims$sigma[index]
#           )
# sim_df_100m$sim_hundred_m <- rnorm(length(sim_df_100m$age), sim_df_100m$mean, sim_df_100m$sigma)
ribbon_df <- data.frame(x = seq(0, 9, by = .01)) %>%
  mutate(ymin = mean(sim$sims_list$hundred_m_sims$mu) + quantile(sim$sims_list$hundred_m_sims$beta2, .025) * x,
         ymax = mean(sim$sims_list$hundred_m_sims$mu) + quantile(sim$sims_list$hundred_m_sims$beta2, .975) * x)
ggplot() +
  geom_point(data = dec_data_standard, 
             mapping = aes(x = age^2,
                           y = hundred_m),
             alpha = .2) +
  geom_abline(intercept = sim_beta_coef_list$hundred_m[1],
              slope = sim_beta_coef_list$hundred_m[3],
              color = "tomato") +
  geom_abline(intercept = mean(sim$sims_list$hundred_m_sims$mu),
              slope = mean(sim$sims_list$hundred_m_sims$beta2),
              color = "blue") +
  geom_ribbon(data = ribbon_df,
              mapping = aes(x = x,
                            ymin = ymin,
                            ymax = ymax),
              fill = "green",
              alpha = .4) +
  labs(title = "age2 vs 100m. red (real), blue (posterior mean), green (95% cred. int)")
  
ribbon_df_discus <- data.frame(x = seq(-4.5, 4.5, by = .01)) %>%
  mutate(ymin = mean(sim$sims_list$discus_sims$mu) + quantile(sim$sims_list$discus_sims$betaY[,3], .025) * x,
         ymax = mean(sim$sims_list$discus_sims$mu) + quantile(sim$sims_list$discus_sims$betaY[,3], .975) * x)
ggplot() +
  geom_point(data = dec_data_standard, 
             mapping = aes(x = shot_put,
                           y = discus),
             alpha = .2) +
  geom_abline(intercept = sim_beta_coef_list$discus[1],
              slope = sim_beta_coef_list$discus[7],
              color = "tomato") +
  geom_abline(intercept = mean(sim$sims_list$discus_sims$mu),
              slope = mean(sim$sims_list$discus_sims$betaY[,3]),
              color = "blue") +
  geom_ribbon(data = ribbon_df_discus,
              mapping = aes(x = x,
                            ymin = ymin,
                            ymax = ymax),
              fill = "green",
              alpha = .4) + 
  labs(title = "shotput vs discus. red (real), blue (posterior mean), green (95% cred. int)")

  



simple_prop <- results %>%
  filter(comp == "simple",
         type != "spline") %>%
  group_by(target, predictor, type, comp) %>%
  summarize(prop = sum(cov_check) / n()) %>%
  drop_na()

prop_df <- rbind(simple_prop, comp_prop)
ggplot(data= prop_df, mapping = aes(x = prop, y = target,
                                    fill = comp)) +
  geom_boxplot()


comp_prop_wide <- comp_prop %>%
  ungroup() %>%
  select(-type, -comp) %>%
  pivot_wider(names_from = target,
              values_from = prop) %>%
  arrange(factor(predictor, levels = c('age', 'age2', 'age3', dec_events[1:9]))) %>%
  select(all_of(c('predictor', dec_events)))
simple_prop_wide <- simple_prop %>%
  ungroup() %>%
  select(-type, -comp) %>%
  pivot_wider(names_from = target,
              values_from = prop) %>%
  arrange(factor(predictor, levels = c('age', 'age2', 'age3', dec_events[1:9]))) %>%
  select(all_of(c('predictor', dec_events)))
  
comp_prop_wide %>%
  kable(caption = "Proportion of 95% uncertainty intervals containing the true parameter associated with corresponding predictor. NA's have been replaced with '_'",
        escape = F,
        label = "athlete_table",
        digits = 2,
        format = "latex",
        booktabs = T) # Output format = latex 


## no longer need to unstandardize bounds with new simulation as of 6/11
## need to un-standardize the bounds now.
sim_data_list <- readRDS(paste0(data_dir, "sim_data_list_manual.RData"))
event_sums <- get_event_sums_df(sim_data_list[[1]]) %>%
  mutate(iter = 1)
for (i in 2:200) {
  event_sums <- rbind(event_sums,
                      get_event_sums_df(sim_data_list[[i]]) %>%
                        mutate(iter = i))
}
iter = 1:200
age1_sums <- expand.grid(mean_score = mean(sim_data_list[[1]]$age),
                        sd_score = sd(sim_data_list[[1]]$age),
                        iter = iter,
                        event = "age")
age2_sums <- expand.grid(mean_score = mean(sim_data_list[[1]]$age^2),
                         sd_score = sd(sim_data_list[[1]]$age^2),
                         iter = iter,
                         event = "I(age^2)")
age3_sums <- expand.grid(mean_score = mean(sim_data_list[[1]]$age^3),
                         sd_score = sd(sim_data_list[[1]]$age^3),
                         iter = iter,
                         event = "I(age^3)")
covariate_sums <- rbind(event_sums,
                        age1_sums,
                        age2_sums,
                        age3_sums)
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
results <- results %>%
  mutate(predictor = case_when(predictor == "age1" ~ "age",
                               predictor == "age2" ~ "I(age^2)",
                               predictor == "age3" ~ "I(age^3)",
                               TRUE ~ predictor),
         target_sd = NA,
         predictor_sd = NA) %>%
  filter(predictor %in% c("age", "I(age^2)", "I(age^3)", dec_events))
for (i in 1:nrow(results)) {
  print(i)
  iter_num = results$iter[i]
  predictor = results$predictor[i]
  target_name = results$target[i]
  target_sd <- covariate_sums %>%
    filter(event == target_name,
           iter == iter_num) %>%
    select(sd_score) %>%
    pull()
  predictor_sd <- covariate_sums %>%
    filter(event == predictor,
           iter == iter_num) %>%
    select(sd_score) %>%
    pull()
  results$target_sd[i] <- target_sd
  results$predictor_sd[i] <- predictor_sd
}

results <- results %>%
  mutate(lb_new = lb * target_sd / predictor_sd,
         ub_new = ub * target_sd / predictor_sd)





# testing discus and shotput ----------------------------------------------

sim_events <- sim$sim_events
cor(sim_events$shot_put, sim_events$discus)
cor(sim_data$shot_put, sim_data$discus)

#list of betas for predicting discus. shotput should be 3rd

shotput_betas <- sim$sims_list$discus_sims$betaY[,3]
quantile(shotput_betas, c(0.025, 0.975))
sim_beta_coef_list$discus
#truth is .364 so we're good. at least for this iteration.