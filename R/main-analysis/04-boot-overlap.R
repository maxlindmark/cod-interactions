# 2025.06.26
# Code modified from example written by Sean C. Anderson
library(tidyr)
library(ggdist)
library(stringr)
library(purrr)
library(dplyr)
library(readr)
library(ggstats)
library(RColorBrewer)
library(sdmTMB)
library(patchwork)
library(ggplot2)
library(ggsidekick); theme_set(theme_sleek())

# for_overlap <- read_csv("https://raw.githubusercontent.com/maxlindmark/cod-interactions/refs/heads/main/data/clean/for_overlap.csv")
# trawl_data <- read_csv("https://raw.githubusercontent.com/maxlindmark/cod-interactions/refs/heads/main/data/clean/trawl_data_avg.csv")

home <- here::here()

for_overlap <- read_csv(paste0(home, "/data/clean/for_overlap.csv"))
trawl_data <- read_csv(paste0(home, "/data/clean/trawl_data_avg.csv"))
                       
# Function to calculate diet overlap
calculate_diet_overlap <- function(data) {
  data |>
    # Weighted mean diet in spatiotemporal units
    pivot_longer(ends_with("_tot")) |>
    summarise(wm_prey_by_grp = weighted.mean(value, weight),
      .by = c(year, quarter, ices_rect, group, name)) |>
    mutate(tot_prey = sum(wm_prey_by_grp),
      prop_prey = wm_prey_by_grp / tot_prey,
      .by = c(year, quarter, ices_rect, group)) |>
    dplyr::select(-wm_prey_by_grp, -tot_prey) |>
    pivot_wider(names_from = group, values_from = prop_prey) |>
    # Calculate overlap
    summarise(`Flounder\nSmall cod` = 1 - 0.5*sum(abs(Flounder - `Small cod`)),
      `Flounder\nLarge cod` = 1 - 0.5*sum(abs(Flounder - `Large cod`)),
      `Small cod\nLarge cod` = 1 - 0.5*sum(abs(`Large cod` - `Small cod`)),
      .by = c(year, quarter, ices_rect))

}

# Calculate mean overlap (and slopes) in all data
ovr <- calculate_diet_overlap(for_overlap) |>
  pivot_longer(c(`Flounder\nSmall cod`, `Flounder\nLarge cod`, `Small cod\nLarge cod`),
    names_to = "overlap_group", values_to = "overlap") |>
  drop_na(overlap)

ovr <- ovr |>
  left_join(trawl_data, by = c("year", "quarter", "ices_rect")) |>
  mutate(
    overlap_group = as.factor(overlap_group),
    mean_biom_sc = as.numeric(scale(log((flounder + small_cod + large_cod) / 3)))
    )

# Replace zeroes. They are so few we can't estimate much from it (e.g., with a delta-beta), especially in the bootstraps
ovr |>
  summarise(pct_zero = mean(overlap == 0) * 100)
ovr |>
  summarise(pct_zero = mean(overlap == 0) * 100, .by = overlap_group)
ovr |> arrange(overlap) |> as.data.frame()
min_pos <- 3.348926e-04#min(ovr$overlap[ovr$overlap > 0], na.rm = TRUE)
max_pos <- max(ovr$overlap[ovr$overlap > 0], na.rm = TRUE)
ovr_test <- ovr
ovr <- ovr |>
  mutate(overlap = ifelse(overlap < min_pos, min_pos, overlap))
summary(ovr$overlap)
str(ovr)

# m <- sdmTMB(
#   list(overlap ~ 1,
#        overlap ~ 0 + overlap_group*mean_biom_sc),
#   data = ovr,
#   spatial = "off",
#   family = sdmTMB::delta_beta()
# )
# tidy(m)
# sanity(m)

set.seed(9999)
m <- glmmTMB::glmmTMB(
  overlap ~ 0 + overlap_group*mean_biom_sc,
  data = ovr,
  family = glmmTMB::beta_family()
)

nrow(ovr)

# simulate residuals
sim_res <- DHARMa::simulateResiduals(fittedModel = m, n = 1000)
plot(sim_res)

# Create theoretical quantiles for Q-Q plot
n <- length(sim_res$scaledResiduals)
theoretical_quantiles <- qunif(ppoints(n))

# Sort scaled residuals for Q-Q plot
sorted_residuals <- sort(sim_res$scaledResiduals)

# Q-Q plot data
qq_data <- data.frame(
  theoretical = theoretical_quantiles,
  sample = sorted_residuals
)

# Create Q-Q plot
ggplot(qq_data, aes(x = theoretical, y = sample)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_abline(slope = 1, intercept = 0) +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(
    x = "Expected",
    y = "Observed"
  )

ggsave(paste0(home, "/figures/supp/qq_beta.pdf"), width = 11, height = 11, units = "cm")

str(ovr)

# Make a plot of overlap values
ggplot(ovr, aes(x = as.numeric(overlap_group) - 0.1, y = overlap, color = overlap_group, fill = overlap_group)) +
  stat_slab(
    aes(x = overlap_group, thickness = after_stat(pdf*n)),
    scale = 0.8,
    side = "right", 
    breaks = 30,
    alpha = 0.85, 
    density = "histogram"
  ) +
  geom_point(
    size = 0.8,
    alpha = 0.5,
    position = position_jitter(seed = 1, width = 0.03, height = 0)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  labs(y = "Schoener's Overlap")

ggsave(paste0(home, "/figures/supp/schoener_histo.pdf"), width = 11, height = 13, units = "cm")


# Test a conditional effect plot to see how much the overlap changes going from highest to lowest density
nd <- tibble(
  expand_grid(
    mean_biom_sc = seq(min(ovr_test$mean_biom_sc), max(ovr_test$mean_biom_sc), length.out = 30),
    overlap_group = unique(ovr$overlap_group))
  )

nd$pred <- predict(m, newdata = nd, type = "response")

nd |>
  filter(mean_biom_sc %in% c(min(mean_biom_sc), max(mean_biom_sc))) |> 
  pivot_wider(names_from = overlap_group, values_from = pred)

ggplot(nd, aes(mean_biom_sc, pred, color = overlap_group)) +
  geom_line() +
  theme_light()

summary(m)

intercepts <- broom.mixed::tidy(m)  |>
  filter(!str_detect(term, "mean_biom_sc")) |>
  mutate(overlap_group = str_remove(term, "overlap_group"),
         overlap_group = as.factor(overlap_group)) |>
  # Note we are using the bootstrapped CI, not CI of estimates!
  mutate(estimate = plogis(estimate)) |> 
  dplyr::select(overlap_group, estimate) |> 
  mutate(par = "intercept")

intercepts

slopes <- broom.mixed::tidy(m) |>
  filter(str_detect(term, "mean_biom_sc"))

base_slope <- slopes |> filter(term == "mean_biom_sc")

slopes <- slopes |>
  mutate(
    estimate = if_else(term == "mean_biom_sc", estimate, base_slope$estimate + estimate),
    #conf.low = if_else(term == "mean_biom_sc", conf.low, base_slope$conf.low + conf.low),
    #conf.high = if_else(term == "mean_biom_sc", conf.high, base_slope$conf.high + conf.high)
  ) |>
  mutate(overlap_group = str_remove(term, "overlap_group"),
         overlap_group = str_remove(overlap_group, ":mean_biom_sc"),
         overlap_group = ifelse(overlap_group == "mean_biom_sc", "Flounder\nLarge cod", overlap_group),
         overlap_group = as.factor(overlap_group)) |> 
  dplyr::select(overlap_group, estimate) |> 
  mutate(par = "slope")

slopes

all_fits <- bind_rows(slopes, intercepts)

### Now bootstrap

fit_model <- function(d, i = seq(1, nrow(d))) {
  dat <- d[i,]
  ovr_sub <- calculate_diet_overlap(dat) |>
    pivot_longer(c(`Flounder\nSmall cod`, `Flounder\nLarge cod`, `Small cod\nLarge cod`),
      names_to = "overlap_group", values_to = "overlap") |>
    drop_na(overlap) |>
    left_join(trawl_data, by = c("year", "quarter", "ices_rect")) |>
    mutate(overlap_group = as.factor(overlap_group),
           mean_biom_sc = as.numeric(scale(log(flounder + small_cod + large_cod) / 3))
           )
  ovr_sub <- ovr_sub |>
    mutate(overlap = ifelse(overlap < min_pos, min_pos, overlap),
           overlap = ifelse(overlap >= 1, max_pos, overlap))
  
  m <- glmmTMB::glmmTMB(
    overlap ~ 0 + overlap_group*mean_biom_sc,
    data = ovr_sub,
    family = glmmTMB::beta_family()
  )

  slopes <- broom.mixed::tidy(m) |>
    filter(str_detect(term, "mean_biom_sc"))

  #slopes$estimate

  intercepts <- broom.mixed::tidy(m)  |>
    filter(!str_detect(term, "mean_biom_sc")) |>
    mutate(overlap_group = str_remove(term, "overlap_group"),
           overlap_group = as.factor(overlap_group)) |>
    # Note we are using the bootstrapped CI, not CI of estimates!
    mutate(estimate = plogis(estimate))
    #mutate(across(c(estimate, conf.low, conf.high), plogis))

  #intercepts$estimate
  
  intercepts <- broom.mixed::tidy(m)  |>
    filter(!str_detect(term, "mean_biom_sc")) |>
    mutate(overlap_group = str_remove(term, "overlap_group"),
           overlap_group = as.factor(overlap_group),
           estimate = plogis(estimate))

  # return(list(
  #   intercepts = intercepts$estimate,
  #   slopes = slopes$estimate
  # ))
  
  names_out <- c(as.character(intercepts$overlap_group), as.character(slopes$term))
  out <- c(intercepts$estimate, slopes$estimate)
  names(out) <- names_out
  
  return(out)
  
}

fit_model(for_overlap)

set.seed(1)
b <- boot::boot(for_overlap, fit_model, R = 10000L)
boot::boot.ci(b, type = "perc", index = 1)
boot::boot.ci(b, type = "perc", index = 2)
boot::boot.ci(b, type = "perc", index = 3)
boot::boot.ci(b, type = "perc", index = 4)
boot::boot.ci(b, type = "perc", index = 5)
boot::boot.ci(b, type = "perc", index = 6)
#slopes$overlap_group

param_names <- c(
  "Flounder\nLarge cod",
  "Flounder\nSmall cod",
  "Small cod\nLarge cod",
  "Flounder\nLarge cod",
  "Flounder\nSmall cod",
  "Small cod\nLarge cod"
)

cis <- map(1:ncol(b$t), ~boot::boot.ci(b, type = "perc", index = .x))

ci_tbl <- tibble(
  overlap_group = param_names,
  par = rep(c("intercept", "slope"), each = 3),
  lower = map_dbl(cis, ~.x$percent[4]),
  upper = map_dbl(cis, ~.x$percent[5])
)

#
boot::boot.ci(b, type = "perc", index = 1)$percent
#
ci_tbl

all_fits_ci <- ci_tbl |> left_join(all_fits, by = c("overlap_group", "par"))

all_fits_ci

all_fits_ci <- all_fits_ci |> 
  mutate(par2 = ifelse(par == "intercept",
                       "Mean overlap index",
                       "Slope of biomass density"))

# Add jittered values
ovr <- ovr |> 
  mutate(par2 = "Mean overlap index")

all_fits_ci

## Now plot
p1 <- ggplot(all_fits_ci, aes(estimate, overlap_group)) +
  geom_vline(data = filter(all_fits_ci, par == "slope"), 
             aes(xintercept = 0), linetype = "dashed", alpha = 0.6) +
  geom_jitter(data = ovr, aes(overlap, overlap_group, color = overlap_group), alpha = 0.3, width = 0) +
  geom_point(color = "grey30", size = 2.5) +
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(~par2, scales = "free_x") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0, alpha = 0.75) +
  labs(y = "", x = "Estimate") +
  theme(legend.position.inside = c(0.15, 0.1)) +
  guides(alpha = guide_legend(position = "inside", override.aes = list(alpha = c(0.2, 0.8))),
         fill = "none", color = "none") +
  theme(strip.text.x = element_text(margin = unit(rep(2, 4), "pt"))) +
  geom_stripped_rows(color = NA)

egg::tag_facet(p1, fontface = 1, open = "", close = "")
 
ggsave(paste0(home, "/figures/schoener_zib.pdf"), width = 17, height = 8, units = "cm")

