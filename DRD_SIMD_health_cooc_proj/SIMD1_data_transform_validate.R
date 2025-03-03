# ==================================================
# Script: SIMD1_data_prep_metrics.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD1 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd1_adj <- read.csv("data/SIMD 1 weighted adjacency with obs above 20.csv", row.names = 1)
simd1_bootstrap <- read.csv("data/SIMD 1 weighted adjacency bootstrap proportions with obs above 20.csv")
simd1_ci <- read.csv("data/SIMD 1 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd1_adj_long <- simd1_adj %>%
  mutate(condition1 = row.names(simd1_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd1_adj_long$index <- seq_len(nrow(simd1_adj_long))
simd1_bootstrap$index <- seq_len(nrow(simd1_bootstrap))
simd1_ci$index <- seq_len(nrow(simd1_ci))

# Merge all data based on the 'index'
simd1_merged_data <- simd1_adj_long %>%
  left_join(simd1_bootstrap, by = "index") %>%
  left_join(simd1_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd1_merged_data)[colnames(simd1_merged_data) == "eip"] <- "bootstrap"
colnames(simd1_merged_data)[colnames(simd1_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd1_merged_data)[colnames(simd1_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd1_merged_data <- simd1_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd1_valid_data <- simd1_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd1_merged_data, "processed/SIMD1_merged_data.csv", row.names = FALSE)
write.csv(simd1_valid_data, "processed/SIMD1_merged_valid.csv", row.names = FALSE)
