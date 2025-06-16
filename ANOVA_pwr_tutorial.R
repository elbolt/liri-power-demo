# Power analysis for between-condition ANOVA using pwr

library(dplyr)
library(pwr)
library(rstudioapi)

# Set seed and working directory
set.seed(2025)
setwd(dirname(getActiveDocumentContext()$path))

# --------------------------------------
# Step 1: Create design for illustration
# Identical with `LMM_simr_tutorial.R`
# --------------------------------------

# Experimental design: lexical decision task with 3 conditions
n_participants <- 30
n_items_per_condition <- 50
conditions <- c("nonword", "lowfreq", "highfreq")
n_items <- length(conditions) * n_items_per_condition

# Create participant and item identifiers
participant_id <- factor(rep(paste0("p", 1:n_participants), each = n_items))
item_id <- factor(rep(paste0("w", 1:n_items), times = n_participants))

# Assign each item to a condition
condition_lookup <- data.frame(
  item_id = paste0("w", 1:n_items),
  condition = rep(conditions, each = n_items_per_condition)
)

# Match conditions to item_id
condition <- factor(
  condition_lookup$condition[match(as.character(item_id), condition_lookup$item_id)],
  levels = conditions
)

# Create design matrix
design <- data.frame(
  participant_id = participant_id,
  item_id = item_id,
  condition = condition
)

# RT (which we will eventually measure)
design$rt_ms <- NA
View(design)

# Simulate RTs (ms): nonword = 600, lowfreq = 585, highfreq = 570
# For illustrative purposes only, not necessary for power analysis
design <- design |>
  mutate(
    rt_ms = case_when(
      condition == "nonword" ~ rnorm(n(), mean = 600, sd = 80),
      condition == "lowfreq" ~ rnorm(n(), mean = 585, sd = 80),
      condition == "highfreq" ~ rnorm(n(), mean = 570, sd = 80)
    )
  )
View(design)

# --------------------------------------
# Step 3: Aggregate RTs to participant x condition level
# --------------------------------------

agg_data <- design |>
  group_by(participant_id, condition) |>
  summarise(rt_ms = mean(rt_ms), .groups = "drop")
View(agg_data)

# --------------------------------------
# Step 4: Model for illustration
# --------------------------------------

model_anova <- anova(lm(rt_ms ~ condition, data = agg_data))

# --------------------------------------
# Step 5: Power analysis using pwr.f2.test
# --------------------------------------

power_result <- pwr.f2.test(
  u = 2,          # number of predictors (3 levels - 1)
  f2 = 0.17,      # corresponds to d = 0.41 (see slides)
  sig.level = 0.05,
  power = 0.80
)

# Compute required sample size from power result
# - u: numerator df (number of predictors tested, e.g., 2 contrasts)
# - v: denominator df (residual degrees of freedom)
# Total sample size N is reconstructed as: N = u + v + 1
# We use ceiling() to round up to the next whole number
n_required <- ceiling(power_result$u + power_result$v + 1)

print(power_result)
print(n_required)
