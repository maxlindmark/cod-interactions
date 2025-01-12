library(dplyr)

set.seed(1)
prey_weights <- rlnorm(12)
fish_ids <- 1:12
haul_ids <- c(rep(1, 6), rep(2, 6))
predator_weights <- rlnorm(12, 1)
haul_weights_total <- c(rep(rlnorm(1, 5), 6), rep(rlnorm(1, 4), 6))

df <- data.frame(fish_ids, haul_ids, predator_weights, haul_weights_total)
df

df <- group_by(df, haul_ids) |>
  mutate(fraction_sampled = sum(predator_weights) / haul_weights_total[1])
select(df, fraction_sampled)

# must upweight the samples from haul_id 1
# take inverse of fraction sampled:
df <- mutate(df, weight = 1 / fraction_sampled)

df |>
  mutate(check = weight * predator_weights) |>
  group_by(haul_ids) |>
  summarise(check_total = sum(check))

# so now your first tow is worth proportionally more than your 2nd tow as desired
# here the proportion is based on tow weight
# could instead be based on numbers of fish

# could be turned into something smaller as long as ratio is right:

df$scaled_weight <- df$weight / mean(df$weight)
