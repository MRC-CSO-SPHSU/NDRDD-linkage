# ==================================================
# Script: SIMD_4_5_data_transform_validate.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD_4_5 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd_4_5_adj <- read.csv("data/SIMD_4_5 weighted adjacency with obs above 20.csv", row.names = 1)
simd_4_5_bootstrap <- read.csv("data/SIMD_4_5 weighted adjacency bootstrap proportions with obs above 20.csv")
simd_4_5_ci <- read.csv("data/SIMD_4_5 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd_4_5_adj_long <- simd_4_5_adj %>%
  mutate(condition1 = row.names(simd_4_5_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd_4_5_adj_long$index <- seq_len(nrow(simd_4_5_adj_long))
simd_4_5_bootstrap$index <- seq_len(nrow(simd_4_5_bootstrap))
simd_4_5_ci$index <- seq_len(nrow(simd_4_5_ci))

# Merge all data based on the 'index'
simd_4_5_merged_data <- simd_4_5_adj_long %>%
  left_join(simd_4_5_bootstrap, by = "index") %>%
  left_join(simd_4_5_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd_4_5_merged_data)[colnames(simd_4_5_merged_data) == "eip"] <- "bootstrap"
colnames(simd_4_5_merged_data)[colnames(simd_4_5_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd_4_5_merged_data)[colnames(simd_4_5_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd_4_5_merged_data <- simd_4_5_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd_4_5_valid_data <- simd_4_5_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd_4_5_merged_data, "processed/SIMD_4_5_merged_data.csv", row.names = FALSE)
write.csv(simd_4_5_valid_data, "processed/SIMD_4_5_merged_valid.csv", row.names = FALSE)
