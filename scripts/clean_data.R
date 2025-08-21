# Date: August 21. Code taken from compare_data.R (original directory). Take original data and clean. remove 'bad' observations and filter appropriately.



online_data <- read_csv("decathlon_simulation/data/DecData.csv")
dec_events <- c("hundred_m", "long_jump", "shot_put",
                "high_jump", "four_hundred_m", "hurdles",
                "discus", "pole_vault", "javelin",
                "fifteen_hundred_m")
colnames(online_data) <- tolower(gsub(" ", 
                                      "_", 
                                      colnames(online_data),
                                      fixed = TRUE))
colnames(online_data) <- gsub("men_",
                              "",
                              colnames(online_data))
online_data <- online_data %>%
  rename(name = competitor,
         hundred_m = "100",
         long_jump = lj,
         shot_put = sp,
         high_jump = hj, 
         four_hundred_m = "400", 
         hurdles = "110h", 
         discus = dt,
         pole_vault = pv, 
         javelin = jt,
         fifteen_hundred_m = "1500")  %>%
  mutate(dob = mdy(dob),
         date = mdy(date),
         age = as.numeric(date - dob) / 365.25,
         fifteen_hundred_m = hms(fifteen_hundred_m),
         fifteen_hundred_m = (hour(fifteen_hundred_m)*60) + minute(fifteen_hundred_m) + (second(fifteen_hundred_m)/60)
  ) %>%
  filter(!is.na(dob)) %>%
  group_by(name, dob) %>%
  mutate(subj_id = cur_group_id(),
         subj_id = as.factor(subj_id))

athlete_count <- online_data %>%
  group_by(subj_id) %>%
  summarize(n = n()) %>%
  filter(n >= 4)


online_data_filter <- online_data %>%
  filter(subj_id %in% athlete_count$subj_id) %>%
  mutate(points = overall_score) %>%
  select(name, subj_id, dec_events, age, points, date)  %>%
  filter(year(dob) > 1950) %>%
  group_by(name, dob) %>%
  mutate(athlete_id = cur_group_id())  %>%
  filter(discus > 2) %>%
  unique()



ggplot(athlete_count, mapping = aes(x = n)) + 
  geom_histogram(color = "white") +
  labs(x = "Performance count by athlete")


ggplot(online_data_filter, mapping = aes(x = points)) + 
  geom_histogram(color = "white") 


write_csv(online_data_filter, "decathlon_simulation/data/online_data_filter.csv")
