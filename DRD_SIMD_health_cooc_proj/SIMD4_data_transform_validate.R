# ==================================================
# Script: SIMD4_data_transform_validate.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD4 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd4_adj <- read.csv("data/SIMD 4 weighted adjacency with obs above 20.csv", row.names = 1)
simd4_bootstrap <- read.csv("data/SIMD 4 weighted adjacency bootstrap proportions with obs above 20.csv")
simd4_ci <- read.csv("data/SIMD 4 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd4_adj_long <- simd4_adj %>%
  mutate(condition1 = row.names(simd4_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd4_adj_long$index <- seq_len(nrow(simd4_adj_long))
simd4_bootstrap$index <- seq_len(nrow(simd4_bootstrap))
simd4_ci$index <- seq_len(nrow(simd4_ci))

# Merge all data based on the 'index'
simd4_merged_data <- simd4_adj_long %>%
  left_join(simd4_bootstrap, by = "index") %>%
  left_join(simd4_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd4_merged_data)[colnames(simd4_merged_data) == "eip"] <- "bootstrap"
colnames(simd4_merged_data)[colnames(simd4_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd4_merged_data)[colnames(simd4_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd4_merged_data <- simd4_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd4_valid_data <- simd4_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd4_merged_data, "processed/SIMD4_merged_data.csv", row.names = FALSE)
write.csv(simd4_valid_data, "processed/SIMD4_merged_valid.csv", row.names = FALSE)
