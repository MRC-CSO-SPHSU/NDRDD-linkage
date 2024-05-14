rm(list = ls())
#################
#      Notes    #
#################

#Reading in GGMnonreg adjacency matrices. 

#########################
#                       #
#    Load packages      #
#                       #
#########################
library(igraph)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(readxl)

#########################
#                       #
#     Load functions    #
#                       #
#########################


#########################
#                       #
#     Main Script       #
#                       #
#########################

if (Sys.info()[4] == "DESKTOP-2CKTEEO") wd <- "C:/Users/mmc78h/OneDrive - University of Glasgow/DRD/GGMnonreg"


wd <- "T:/projects/CSO_DRD_S00359/Data"

setwd(wd)
getwd()
# full_link_df <- read.csv("full_link weighted adjacency.csv")
# full_link_df_cis <- read.csv("full_link weighted adjacency CIs.csv")
# full_link_df_bootp <- read.csv("full_link weighted adjacency bootstrap proportions.csv")

full_link_df       <- utils::read.csv("full_link weighted adjacency with obs above 20.csv")
full_link_df_cis   <- utils::read.csv("full_link weighted adjacency CIs with obs above 20.csv")
full_link_df_bootp <- utils::read.csv("full_link weighted adjacency bootstrap proportions with obs above 20.csv")

rownames(full_link_df) <- full_link_df$X
full_link_df$X <- NULL
names(full_link_df_cis) <- c("Tri_location", "lci", "uci")

###Check if confidence interval contains zero
# full_link_df_cis$contains_zero = (full_link_df_cis['lci'] <= 0) & (full_link_df_cis['uci'] >= 0)
# 
# ##Set matrix to zero where CI contains null
# upper_triangle <- upper.tri(full_link_df)
# 
# length(as.vector(as.matrix(upper_triangle)))
# 
# # Set the values in the matrix to zero based on the 'contains_zero' column
# full_link_df_zeroed_ci <- full_link_df
# full_link_df_zeroed_ci[upper_triangle] <- full_link_df[upper_triangle] * !full_link_df_cis$contains_zero
# 
# rowSums(full_link_df) == rowSums(full_link_df_zeroed_ci)

########################################################################################################
# Set the values in the matrix to zero based on proportion of included edges in bootstrap replications #
########################################################################################################

dim(full_link_df_bootp)
hist(full_link_df_bootp$eip)

upper_triangle_logical <- upper.tri(full_link_df, diag = F)
upper_triangle_vector <- full_link_df[upper_triangle_logical]


full_link_df_bootp$below_95 <- full_link_df_bootp$eip < 0.95
table(full_link_df_bootp$below_95)
table(full_link_df_bootp$below_95) / dim(full_link_df_bootp)[1]

full_link_df_zeroed_boot <- full_link_df
full_link_df_zeroed_boot[upper_triangle_logical] <- full_link_df[upper_triangle_logical] * !full_link_df_bootp$below_95

table(rowSums(full_link_df) == rowSums(full_link_df_zeroed_boot))


hist(rowSums(full_link_df))
hist(rowSums(full_link_df_zeroed_boot))

table(rowSums(full_link_df))
table(rowSums(full_link_df_zeroed_boot))

class(full_link_df)
full_link_mat <- as.matrix(full_link_df)
colnames(full_link_mat) <- colnames(full_link_df)
rownames(full_link_mat) <- colnames(full_link_df)
rawgraph <- graph_from_adjacency_matrix(as.matrix(full_link_mat), weighted = T, mode = "upper")

zeroed_boot_mat <- as.matrix(full_link_df_zeroed_boot)
colnames(zeroed_boot_mat) <- colnames(full_link_df_zeroed_boot)
rownames(zeroed_boot_mat) <- colnames(full_link_df_zeroed_boot)
zerobootgraph <- graph_from_adjacency_matrix(as.matrix(zeroed_boot_mat), weighted = T, mode = "upper")

###############
# # Convert the graph object to an edge list
# edge_list <- get.edgelist(rawgraph)
# edge_weights <- E(rawgraph)$weight
# edge_list <- cbind(edge_list, edge_weights)
# edge_list <- as.data.frame(edge_list)
# dim(edge_list)

# Use the network after removing the non replicating edges
edge_list <- get.edgelist(zerobootgraph)
edge_weights <- E(zerobootgraph)$weight
edge_list <- cbind(edge_list, edge_weights)
edge_list <- as.data.frame(edge_list)
dim(edge_list)
str(edge_list)
head(edge_list)

edge_list$V1 <- as.character(edge_list$V1)
edge_list$V2 <- as.character(edge_list$V2)

edge_list$edge_weights <- as.numeric(as.character(edge_list$edge_weight))
###Remove corr below x 

edge_list <- edge_list[edge_list$edge_weights >= 0.0,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.xlsx")
write.xlsx(edge_list, file = "full_link zeroed below 0.xlsx")

edge_list <- edge_list[edge_list$edge_weights >= 0.05,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.05.xlsx")
write.xlsx(edge_list, file = "full_link zeroed below 0.05.xlsx")

edge_list <- edge_list[edge_list$edge_weights > 0.08,]
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.08.xlsx")
write.xlsx(edge_list, file = "full_link zeroed below 0.08.xlsx")


edge_list <- edge_list[edge_list$edge_weights > 0.1,]
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.1.xlsx")
write.xlsx(edge_list, file = "full_link zeroed below 0.1.xlsx")

edge_list <- edge_list[edge_list$edge_weights > 0.20,]
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.2.xlsx")
write.xlsx(edge_list, file = "full_link zeroed below 0.2.xlsx")

