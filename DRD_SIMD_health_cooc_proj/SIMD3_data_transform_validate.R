# ==================================================
# Script: SIMD3_data_transform_validate.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD3 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd3_adj <- read.csv("data/SIMD 3 weighted adjacency with obs above 20.csv", row.names = 1)
simd3_bootstrap <- read.csv("data/SIMD 3 weighted adjacency bootstrap proportions with obs above 20.csv")
simd3_ci <- read.csv("data/SIMD 3 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd3_adj_long <- simd3_adj %>%
  mutate(condition1 = row.names(simd3_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd3_adj_long$index <- seq_len(nrow(simd3_adj_long))
simd3_bootstrap$index <- seq_len(nrow(simd3_bootstrap))
simd3_ci$index <- seq_len(nrow(simd3_ci))

# Merge all data based on the 'index'
simd3_merged_data <- simd3_adj_long %>%
  left_join(simd3_bootstrap, by = "index") %>%
  left_join(simd3_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd3_merged_data)[colnames(simd3_merged_data) == "eip"] <- "bootstrap"
colnames(simd3_merged_data)[colnames(simd3_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd3_merged_data)[colnames(simd3_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd3_merged_data <- simd3_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd3_valid_data <- simd3_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd3_merged_data, "processed/SIMD3_merged_data.csv", row.names = FALSE)
write.csv(simd3_valid_data, "processed/SIMD3_merged_valid.csv", row.names = FALSE)
