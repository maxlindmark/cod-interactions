library(tidyverse)
library(sdmTMB)

home <- here::here()
d <- read_csv(paste0(home, "/data/clean/aggregated_stomach_data.csv"))

mesh <- make_mesh(d, xy_cols = c("X", "Y"), cutoff = 15)

plot(mesh)
