# ==================================================
# Script: SIMD2_data_transform_validate.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD2 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd2_adj <- read.csv("data/SIMD 2 weighted adjacency with obs above 20.csv", row.names = 1)
simd2_bootstrap <- read.csv("data/SIMD 2 weighted adjacency bootstrap proportions with obs above 20.csv")
simd2_ci <- read.csv("data/SIMD 2 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd2_adj_long <- simd2_adj %>%
  mutate(condition1 = row.names(simd2_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd2_adj_long$index <- seq_len(nrow(simd2_adj_long))
simd2_bootstrap$index <- seq_len(nrow(simd2_bootstrap))
simd2_ci$index <- seq_len(nrow(simd2_ci))

# Merge all data based on the 'index'
simd2_merged_data <- simd2_adj_long %>%
  left_join(simd2_bootstrap, by = "index") %>%
  left_join(simd2_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd2_merged_data)[colnames(simd2_merged_data) == "eip"] <- "bootstrap"
colnames(simd2_merged_data)[colnames(simd2_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd2_merged_data)[colnames(simd2_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd2_merged_data <- simd2_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd2_valid_data <- simd2_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd2_merged_data, "processed/SIMD2_merged_data.csv", row.names = FALSE)
write.csv(simd2_valid_data, "processed/SIMD2_merged_valid.csv", row.names = FALSE)
