# Aug 28, 2025
# Code to make descriptive graphs for data section in paper.


set.seed(2)
library(tidyverse)
library(readxl)
library(knitr)


study = "../study"
data_dir = "../data/"
script_dir = "../study/"

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




# descriptive graphs data -------------------------------------------------

ggplot(online_data_filter, mapping = aes(x = points)) + geom_histogram(color = "white") +
  theme_bw() +
  labs(x = "Points", y = "Count", title = "Distribution of points")

ggplot(online_data_filter, mapping = aes(x = age)) + geom_histogram(color = "white") +
  theme_bw() +
  labs(x = "Age", y = "Count", title = "Distribution of ages")

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
