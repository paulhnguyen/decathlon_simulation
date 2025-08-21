# Author: Paul Nguyen
# Date: April 24, 2025
# Purpose: create file to set simulation setting for comparing decathlon model performance
# Details: 
# Dependencies: dplyr
library(dplyr)

# curve type
type = c(
  "cubic",
  "spline"
)

# comp or simple
comp = c(
  "simple",
  "compositional"
)

# how to incorporate individual data
prior = c(
  "none",
  # "joint",
  "moment"
)


# general, future, athlete
pred_type <- c("general",
               "future",
               "athlete")

# 10 fold cv
iter <- 1:10


baseline_settings = expand.grid(type = type,
                                comp = "baseline",
                                prior = "none",
                                pred_type = pred_type,
                                iter = iter)

# save settings
settings = expand.grid(type = type, comp = comp, prior = prior,
                       pred_type = pred_type,
                       iter = iter) %>%
  rbind(baseline_settings) %>%
  filter(!((type == "spline") & (prior == "joint")))


