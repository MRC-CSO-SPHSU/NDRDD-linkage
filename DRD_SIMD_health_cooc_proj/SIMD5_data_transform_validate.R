# ==================================================
# Script: SIMD5_data_transform_validate.R 
# Desc: This script processes, filters, and validates
#       co-occurrence data from the SIMD5 dataset. 
# Author: Babak Mahdavi Ardestani
# ===================================================

rm(list=ls())
renv::load("../")

library(tidyverse)

# Read datasets
simd5_adj <- read.csv("data/SIMD 5 weighted adjacency with obs above 20.csv", row.names = 1)
simd5_bootstrap <- read.csv("data/SIMD 5 weighted adjacency bootstrap proportions with obs above 20.csv")
simd5_ci <- read.csv("data/SIMD 5 weighted adjacency CIs with obs above 20.csv")

# Generate condition pairs
simd5_adj_long <- simd5_adj %>%
  mutate(condition1 = row.names(simd5_adj)) %>%
  pivot_longer(-condition1, names_to = "condition2", values_to = "weight") %>%
  filter(condition1 != condition2)  
  
# Add index column
simd5_adj_long$index <- seq_len(nrow(simd5_adj_long))
simd5_bootstrap$index <- seq_len(nrow(simd5_bootstrap))
simd5_ci$index <- seq_len(nrow(simd5_ci))

# Merge all data based on the 'index'
simd5_merged_data <- simd5_adj_long %>%
  left_join(simd5_bootstrap, by = "index") %>%
  left_join(simd5_ci, by = "index") %>%
  select(-c("X.x", "X.y"))  

# Rename columns
colnames(simd5_merged_data)[colnames(simd5_merged_data) == "eip"] <- "bootstrap"
colnames(simd5_merged_data)[colnames(simd5_merged_data) == "X2.5."] <- "CI_lower"
colnames(simd5_merged_data)[colnames(simd5_merged_data) == "X97.5."] <- "CI_upper"

# Reorder columns 
simd5_merged_data <- simd5_merged_data %>%
  select(index, everything()) 

# Filter to validate data
simd5_valid_data <- simd5_merged_data %>%
  filter(CI_lower > 0, bootstrap >= 0.95, weight >0 )  

# Save results
write.csv(simd5_merged_data, "processed/SIMD5_merged_data.csv", row.names = FALSE)
write.csv(simd5_valid_data, "processed/SIMD5_merged_valid.csv", row.names = FALSE)
