# Logic of weights:
# say we catch 80 fish
# and we sample 5 fish
# each fish now represents 80/5 = 16 fish
# so, log likelihood weight is 16

# say we catch 10 fish
# and we sample 10 fish
# each fish now represents 1/1 = 1 fish
# so, log likelihood weight is 1

# so, weight = N in set / N sampled

library(dplyr)
library(ggplot2)

sim_set <- function(i) {
  N <- rnbinom(1, size = 0.1, mu = 100)
  if (N == 0) {
    return(data.frame(N = N, prey_values = 0, fishing_event = i))
  }
  if (N > 0) {
    prey_values <- rlnorm(N, 0, 0.2)
  }
  data.frame(N = N, prey_values = prey_values, fishing_event = i)
}
set.seed(123)
out <- do.call(rbind, lapply(1:2000, sim_set))

ggplot(out, aes(log(N), prey_values)) + 
  geom_point()

out <- filter(out, N >= 5)
out_downsampled <- out |>
  group_by(fishing_event) |>
  sample_n(5) |>
  mutate(n_sampled = 5)

# check intercept:
coef(lm(prey_values ~ 1, data = out))
coef(lm(prey_values ~ 1, data = out_downsampled))
# note the weights are not set weight or count, it's
# set weight / sampled weight or set count / sampled count
# here that's the same because my example always samples 5
weights <- out_downsampled$N / out_downsampled$n_sampled
weights <- weights / mean(weights)
coef(lm(prey_values ~ 1, data = out_downsampled, weights = weights))
# works as expected

# log space:
coef(lm(log(prey_values) ~ 1, data = out))
coef(lm(log(prey_values) ~ 1, data = out_downsampled))
weights <- out_downsampled$N / out_downsampled$n_sampled
weights <- weights / mean(weights)
coef(lm(log(prey_values) ~ 1, data = out_downsampled, weights = weights))
# same, of course

# what about with an N covariate?
coef(lm(log(prey_values) ~ log(N), data = out))
coef(lm(log(prey_values) ~ log(N), data = out_downsampled))
coef(lm(log(prey_values) ~ log(N), data = out_downsampled, weights = weights))
# still works

# look at it:

bind_rows(
  mutate(out, type = "all"),
  mutate(out_downsampled, type = "downsampled")
) |> 
  ggplot(aes(log(N), log(prey_values), colour = type)) + geom_point() +
  geom_smooth(se = FALSE, formula = y ~ x, method = "lm") +
  scale_colour_manual(values = c("black", "red"))

# so, what's happening is the 'downsampled' regression line gets pulled down
# on the left because you end up upweighting those small sets compared
# to the full catch
# that inflates the coefficient
# here, the weights fix that


# Check CI coverage:
sim_set <- function(i) {
  N <- rnbinom(1, size = 0.1, mu = 100)
  if (N == 0) {
    return(data.frame(N = N, prey_values = 0, fishing_event = i))
  }
  if (N > 0) {
    prey_values <- log(N) * 1 * rlnorm(N, 0, 0.2)
  }
  data.frame(N = N, prey_values = prey_values, fishing_event = i)
}
set.seed(1)
out <- do.call(rbind, lapply(1:2000, sim_set))
out <- filter(out, N >= 5)
m1 <- lm(log(prey_values) ~ log(N), data = out)

# check slope param coverage:
# unbalanced:
set.seed(1)
x <- lapply(1:50, function(i) {
  print(i)
  out_downsampled <- out |>
    group_by(fishing_event) |>
    sample_n(5) |>
    mutate(n_sampled = 5)
  weights <- out_downsampled$N / out_downsampled$n_sampled
  weights <- weights / mean(weights)
  m2 <- lm(log(prey_values) ~ log(N), data = out_downsampled, weights = weights)
  # m2 <- lm(log(prey_values) ~ log(N), data = out_downsampled)
  ci <- confint(m2)
  data.frame(lwr = ci[2,1], upr = ci[2,2], true = coef(m1)[2], est = coef(m2)[2])
})
x <- do.call(rbind, x)
mutate(x, covered = lwr < true & upr > true) |> summarise(coverage = mean(covered))
x |> 
  mutate(iter = seq_len(nrow(x))) |> 
  ggplot(aes(ymin = lwr, ymax = upr, y = est, x = iter)) + geom_point() + geom_linerange() +
  geom_hline(yintercept = x$true[1]) +
  geom_hline(yintercept = mean(x$est), lty = 2)

# same without weights:
set.seed(1)
x <- lapply(1:50, function(i) {
  print(i)
  out_downsampled <- out |>
    group_by(fishing_event) |>
    sample_n(5) |>
    mutate(n_sampled = 5)
  weights <- out_downsampled$N / out_downsampled$n_sampled
  weights <- weights / mean(weights)
  # m2 <- lm(log(prey_values) ~ log(N), data = out_downsampled, weights = weights)
  m2 <- lm(log(prey_values) ~ log(N), data = out_downsampled)
  ci <- confint(m2)
  data.frame(lwr = ci[2,1], upr = ci[2,2], true = coef(m1)[2], est = coef(m2)[2])
})
x <- do.call(rbind, x)
mutate(x, covered = lwr < true & upr > true) |> summarise(coverage = mean(covered))
x |> 
  mutate(iter = seq_len(nrow(x))) |> 
  ggplot(aes(ymin = lwr, ymax = upr, y = est, x = iter)) + geom_point() + geom_linerange() +
  geom_hline(yintercept = x$true[1]) +
  geom_hline(yintercept = mean(x$est), lty = 2)

# balanced:
set.seed(1)
x <- lapply(1:50, function(i) {
  print(i)
  out_downsampled <- out |>
    group_by(fishing_event) |>
    sample_frac(0.10) |>
    mutate(n_sampled = n())
  weights <- out_downsampled$N / out_downsampled$n_sampled
  weights <- weights / mean(weights)
  m2 <- lm(log(prey_values) ~ log(N), data = out_downsampled)
  # m2 <- lm(log(prey_values) ~ log(N), data = out_downsampled, weights = weights)
  ci <- confint(m2)
  data.frame(lwr = ci[2,1], upr = ci[2,2], true = coef(m1)[2], est = coef(m2)[2])
})
x <- do.call(rbind, x)
mutate(x, covered = lwr < true & upr > true) |> summarise(coverage = mean(covered))
x |> 
  mutate(iter = seq_len(nrow(x))) |> 
  ggplot(aes(ymin = lwr, ymax = upr, y = est, x = iter)) + geom_point() + geom_linerange() +
  geom_hline(yintercept = x$true[1]) +
  geom_hline(yintercept = mean(x$est), lty = 2)

