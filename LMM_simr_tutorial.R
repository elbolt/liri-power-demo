# Power analysis using `simr` for lexical decision task with three conditions

library(dplyr)
library(simr)
library(rstudioapi)

# Set seed and working directory
set.seed(2025)
setwd(dirname(getActiveDocumentContext()$path))
# load("LMM_simr_results.RData")

# --------------------------------------
# Step 1: Create design
# Identical with `ANOVA_pwr_tutorial.R`
# --------------------------------------

n_participants <- 30
n_items_per_condition <- 50
conditions <- c("nonword", "lowfreq", "highfreq")
n_items <- length(conditions) * n_items_per_condition

participant_id <- factor(rep(paste0("p", 1:n_participants), each = n_items))
item_id <- factor(rep(paste0("w", 1:n_items), times = n_participants))

condition_lookup <- data.frame(
  item_id = paste0("w", 1:n_items),
  condition = rep(conditions, each = n_items_per_condition)
)
condition <- factor(
  condition_lookup$condition[
    match(as.character(item_id), condition_lookup$item_id)
  ],
  levels = conditions
)

design <- data.frame(
  participant_id = participant_id,
  item_id = item_id,
  condition = condition
)
View(design)

# --------------------------------------
# Step 2: Model parameters
# --------------------------------------

# Fixed effects (slide 38):
# — nonword = 600 (baseline, from Mustermann et al.)
# — lowfreq = 590 (−10 ms, middleground guess)
# — highfreq = 580 (−20 ms, from Mustermann et al.)
fixef_vals <- c(600, -10, -20)

# Random effects (slide 39):
# — Participant: SD = 30 ms
# — Item: SD = 30 ms
# — Residuals: SD = 80 ms
rand_intercepts <- list(
  participant_id = 30^2,
  item_id = 30^2
)
resid_sd <- 80

# --------------------------------------
# Step 3: Create model
# --------------------------------------

model <- makeLmer(
  rt_ms ~ condition + (1 | participant_id) + (1 | item_id),
  fixef = fixef_vals,
  VarCorr = rand_intercepts,
  sigma = resid_sd,
  data = design
)

# --------------------------------------
# Step 4: Initial power estimate
# --------------------------------------

power_lmm <- powerSim(model, alpha = 0.05, nsim = 1000)
print(power_lmm)

# --------------------------------------
# Step 5: Power curve – participants
# --------------------------------------

model_p <- extend(model, along = "participant_id", n = 120)
curve_p <- powerCurve(
  model_p,
  along = "participant_id",
  breaks = seq(20, 120, by = 10),
  nsim = 1000
)
plot(curve_p)
print(curve_p)

# --------------------------------------
# Step 6: Power curve – items
# --------------------------------------

model_i <- extend(model, along = "item_id", n = 390)
curve_i <- powerCurve(
  model_i,
  along = "item_id",
  breaks = seq(90, 390, by = 30),
  nsim = 1000
)
plot(curve_i)
print(curve_i)

# --------------------------------------
# Potentially re-run final model (L15-68)
# n_participants <- 60
# n_items_per_condition <- 60
# --------------------------------------
save.image("LMM_simr_results.RData")