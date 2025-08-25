# Author: Paul Nguyen
# Date: August 25, 2025
# Purpose: create file to create bakeoff splits for decathlon models bakeoff
# Details: 
# Dependencies: dplyr

#3 types of simulations: general (remove obs at random), future (remove last observations for some athletes), athlete (remove entire athletes)

study = "study"
data_dir = "data/"
script_dir = "study/"
stan_dir = "stan_mods/"
source(paste0(script_dir, "decathlon_funs.R"))
# general case
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
set.seed(4)
test_split_list = replicate(10, sample(seq(1, nrow(dec_data_standard)), size = ceiling(nrow(dec_data_standard) / 10), replace = FALSE), simplify = FALSE)
save(test_split_list, file = paste0(data_dir, "test_split_list_general.RData"))

test_df <- dec_data_standard[test_split_list[[1]],]
train_df <- dec_data_standard[-test_split_list[[1]],]

# future case. 1007 unique athlete id's. take 10% of athletes. see if you can predict last decathlon performance.
dec_data_standard_index <- dec_data_standard %>%
  ungroup()%>%
  mutate(index = row_number())
last_perf_df <- dec_data_standard_index %>%
  group_by(athlete_id) %>%
  filter(age == max(age)) 
set.seed(4)
# test set size is 10% of data, but only among the last performances by an athlete.
test_split_list_future= replicate(10, sample(last_perf_df$index, size = ceiling(nrow(online_data_filter) / 10), replace = FALSE), simplify = FALSE)
save(test_split_list_future, file = paste0(data_dir, "test_split_list_future.RData"))



#now, by athlete.
# 10% of athletes as test, 90% as train
sample(dec_data_standard_index$athlete_id, ceiling(length(unique(dec_data_standard$athlete_id)) / 10), replace = F)
test_split_list_athlete = list()
set.seed(4)
for (i in 1:10) {
  test_ind <- sample(dec_data_standard_index$athlete_id,
                     ceiling(length(unique(dec_data_standard$athlete_id)) / 10),
                     replace = F)
  test_split_list_athlete[[i]] <- dec_data_standard_index %>%
    filter(athlete_id %in% test_ind) %>%
    select(index) %>%
    pull()
}

save(test_split_list_athlete, file = paste0(data_dir, "test_split_list_athlete.RData"))
