# test marginal effects
library(sdmTMB)
library(tidyverse)

d <- pcod_2011

# add fake variable
d2 <- d %>% mutate(oxy = rnorm(n = nrow(d), mean = 5, sd = 0.2))

plot(d2$density ~ d2$depth_scaled)
plot(d2$density ~ d2$oxy)
d2 <- d2 %>% mutate(oxy_scaled = scale(oxy))
plot(d2$density ~ d2$oxy_scaled)

pcod_spde <- make_mesh(d, c("X", "Y"), cutoff = 30) # a coarse mesh for example speed
m <- sdmTMB(
  data = d2, formula = density ~ 0 + as.factor(year) + oxy_scaled +  depth_scaled + depth_scaled2,
  time = "year", spde = pcod_spde, family = tweedie(link = "log")
)

# Now predict
nd <- data.frame(depth_scaled =
                   seq(min(d$depth_scaled), max(d$depth_scaled), length.out = 100))
nd$depth_scaled2 <- nd$depth_scaled^2
nd$oxy_scaled <- 0

# You'll need at least one time element. If time isn't also a fixed effect
# then it doesn't matter what you pick:
nd$year <- 2011L

p <- predict(m, newdata = nd, se_fit = TRUE, re_form = NA)

ggplot(p, aes(depth_scaled, exp(est),
              ymin = exp(est - 1.96 * est_se), ymax = exp(est + 1.96 * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4)

# Now try specific values...
nd2 <- data.frame(depth_scaled = c(min(d2$depth_scaled), max(d2$depth_scaled)))
nd2$depth_scaled2 <- nd2$depth_scaled^2
nd2$oxy_scaled <- c(min(d2$oxy_scaled), max(d2$oxy_scaled))
nd2$year <- 2011L

p2 <- predict(m, newdata = nd2, se_fit = TRUE, re_form = NA)

ggplot(p2, aes(depth_scaled, exp(est), 
               ymin = exp(est - 1.96 * est_se), ymax = exp(est + 1.96 * est_se))) +
  geom_point() + geom_errorbar()

# For the condition paper, start with the new mean-weighted values of depth and oxygen and temperature
# Have a categorical variable on x (good/bad condition)
# Then find a good value for sprat

