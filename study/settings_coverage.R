# Author: Paul Nguyen
# Date: July 30, 2024
# Purpose: create file to set simulation setting
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

iter = 1:200


# save settings
settings = expand.grid(type = type,
                       comp = comp,
                       iter = iter) 


