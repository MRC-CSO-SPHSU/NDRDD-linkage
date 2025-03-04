# ==================================================
# Script: SIMD_2_3_data_transform_validate.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD_2_3 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd_2_3_adj <- read.csv("data/SIMD_2_3 weighted adjacency with obs above 20.csv", row.names = 1)
simd_2_3_bootstrap <- read.csv("data/SIMD_2_3 weighted adjacency bootstrap proportions with obs above 20.csv")
simd_2_3_ci <- read.csv("data/SIMD_2_3 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd_2_3_adj_long <- simd_2_3_adj %>%
  mutate(condition1 = row.names(simd_2_3_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd_2_3_adj_long$index <- seq_len(nrow(simd_2_3_adj_long))
simd_2_3_bootstrap$index <- seq_len(nrow(simd_2_3_bootstrap))
simd_2_3_ci$index <- seq_len(nrow(simd_2_3_ci))

# Merge all data based on the 'index'
simd_2_3_merged_data <- simd_2_3_adj_long %>%
  left_join(simd_2_3_bootstrap, by = "index") %>%
  left_join(simd_2_3_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd_2_3_merged_data)[colnames(simd_2_3_merged_data) == "eip"] <- "bootstrap"
colnames(simd_2_3_merged_data)[colnames(simd_2_3_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd_2_3_merged_data)[colnames(simd_2_3_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd_2_3_merged_data <- simd_2_3_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd_2_3_valid_data <- simd_2_3_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd_2_3_merged_data, "processed/SIMD_2_3_merged_data.csv", row.names = FALSE)
write.csv(simd_2_3_valid_data, "processed/SIMD_2_3_merged_valid.csv", row.names = FALSE)
