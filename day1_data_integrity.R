# =============================================================================
# Day 1: Data Acquisition & Integrity Checks
# ML Study on Type 2 Diabetes Classification
# Dataset: diabetes_dataset.csv (100,000 rows, 31 columns)
# =============================================================================

library(tidyverse)
library(janitor)
library(naniar)
library(visdat)

# =============================================================================
# 1. LOAD DATA
# =============================================================================

# Update this path to wherever you saved the CSV
df_raw <- read_csv("diabetes_dataset.csv")

glimpse(df_raw)

# =============================================================================
# 2. INTEGRITY CHECKS
# =============================================================================

cat("\n--- Dataset Dimensions ---\n")
cat("Rows:", nrow(df_raw), "\n")
cat("Columns:", ncol(df_raw), "\n")

# --- 2a. Data Types ---
cat("\n--- Column Types ---\n")
print(sapply(df_raw, class))

# --- 2b. Missing Values ---
cat("\n--- Missing Values ---\n")
print(miss_var_summary(df_raw))

# Visual missing value map (comment out if running non-interactively)
vis_miss(slice_sample(df_raw, n = 5000), warn_large_data = FALSE)

# --- 2c. Duplicate Records ---
n_dupes <- sum(duplicated(df_raw))
cat("\n--- Duplicate Rows ---\n")
cat("Number of duplicates:", n_dupes, "\n")

df_clean <- df_raw %>%
  distinct()

cat("Rows after deduplication:", nrow(df_clean), "\n")

# =============================================================================
# 3. INSPECT TARGET VARIABLE (diabetes_stage)
# =============================================================================

cat("\n--- Target Variable Distribution (raw) ---\n")
print(table(df_clean$diabetes_stage))
print(prop.table(table(df_clean$diabetes_stage)))

# Expected output (approx):
#   No Diabetes  :  7,981  (8.0%)
#   Pre-Diabetes : 31,845 (31.8%)
#   Type 2       : 59,774 (59.8%)
#   Type 1       :    122  (0.1%)   <- to be dropped
#   Gestational  :    278  (0.3%)   <- to be dropped

# =============================================================================
# 4. CONSOLIDATE TO 3 CLASSES
# Drop Type 1 and Gestational (severe class imbalance — 0.1% and 0.3%)
# =============================================================================

CLASSES_TO_KEEP <- c("No Diabetes", "Pre-Diabetes", "Type 2")

df_3class <- df_clean %>%
  filter(diabetes_stage %in% CLASSES_TO_KEEP) %>%
  mutate(
    diabetes_class = factor(
      diabetes_stage,
      levels = c("No Diabetes", "Pre-Diabetes", "Type 2")
    )
  )

cat("\n--- Rows dropped (Type 1 + Gestational) ---\n")
cat(nrow(df_clean) - nrow(df_3class), "rows removed\n")

cat("\n--- Final 3-Class Distribution ---\n")
print(table(df_3class$diabetes_class))
print(prop.table(table(df_3class$diabetes_class)))

# =============================================================================
# 5. FLAG COLUMNS TO EXCLUDE BEFORE MODELLING
# (do NOT drop yet — keep them in for EDA, drop in Day 3 preprocessing)
# =============================================================================

# These will be excluded in Day 3 preprocessing:
LEAKAGE_COLS <- c(
  "glucose_fasting",      # direct diagnostic marker (tautological)
  "glucose_postprandial", # direct diagnostic marker (tautological)
  "insulin_level",        # direct diagnostic marker (tautological)
  "hba1c",               # direct diagnostic marker (tautological)
  "diabetes_risk_score",  # pre-computed score = direct data leakage
  "diagnosed_diabetes",   # the outcome itself encoded as 0/1
  "diabetes_stage"        # original string target (replaced by diabetes_class)
)

cat("\n--- Columns flagged for exclusion in Day 3 ---\n")
cat(paste(LEAKAGE_COLS, collapse = ", "), "\n")
cat("(Keep these in for EDA — drop only before model training)\n")

# =============================================================================
# 6. BASIC COLUMN AUDIT
# =============================================================================

cat("\n--- Summary Statistics ---\n")
print(summary(df_3class))

# Check near-zero variance numeric columns
low_var_cols <- df_3class %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), var, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "column", values_to = "variance") %>%
  filter(variance < 1e-6)

cat("\n--- Near-Zero Variance Columns ---\n")
if (nrow(low_var_cols) == 0) {
  cat("None found.\n")
} else {
  print(low_var_cols)
}

# =============================================================================
# 7. SAVE INTERIM CLEAN FILE
# =============================================================================

write_csv(df_3class, "diabetes_3class_clean.csv")
cat("\n✓ Saved: diabetes_3class_clean.csv\n")
cat("  Rows:", nrow(df_3class), "| Columns:", ncol(df_3class), "\n")

# =============================================================================
# SUMMARY CHECKLIST
# =============================================================================
cat("\n=== Day 1 Checklist ===\n")
cat("[✓] Data loaded (100,000 rows, 31 columns)\n")
cat("[✓] Data types checked\n")
cat("[✓] Missing values assessed\n")
cat("[✓] Duplicates removed\n")
cat("[✓] Type 1 & Gestational dropped (400 rows)\n")
cat("[✓] Target recoded to 3-class factor (diabetes_class)\n")
cat("[✓] Leakage columns flagged for Day 3\n")
cat("[✓] Interim file saved\n")
cat("\nReady for Day 2: EDA\n")
