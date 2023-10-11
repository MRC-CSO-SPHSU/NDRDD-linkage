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

setwd(wd)
ndrdd_df <- read.csv("NDRDD weighted adjacency.csv")
ndrdd_df_cis <- read.csv("NDRDD weighted adjacency CIs.csv")
ndrdd_df_bootp <- read.csv("NDRDD weighted adjacency bootstrap proportions.csv")



dim(ndrdd_df)
names(ndrdd_df)

rownames(ndrdd_df) <- ndrdd_df$X
ndrdd_df$X <- NULL

dim(ndrdd_df_cis)
dim(ndrdd_df_bootp)

names(ndrdd_df_cis) <- c("Tri_location", "lci", "uci")
###Check if confidence interval contains zero
ndrdd_df_cis$contains_zero = (ndrdd_df_cis['lci'] <= 0) & (ndrdd_df_cis['uci'] >= 0)

##Set matrix to zero where CI contains null
upper_triangle <- upper.tri(ndrdd_df)
# Set the values in the matrix to zero based on the 'contains_zero' column
ndrdd_df_zeroed_ci <- ndrdd_df
ndrdd_df_zeroed_ci[upper_triangle] <- ndrdd_df[upper_triangle] * !ndrdd_df_cis$contains_zero

rowSums(ndrdd_df) == rowSums(ndrdd_df_zeroed_ci)

hist(rowSums(ndrdd_df))
hist(rowSums(ndrdd_df_zeroed_ci))

table(rowSums(ndrdd_df))
table(rowSums(ndrdd_df_zeroed_ci))

########################################################################################################
# Set the values in the matrix to zero based on proportion of included edges in bootstrap replications #
########################################################################################################

dim(ndrdd_df_bootp)
hist(ndrdd_df_bootp$eip)

ndrdd_df_bootp$below_95 <- ndrdd_df_bootp$eip < 0.95
table(ndrdd_df_bootp$below_95)
table(ndrdd_df_bootp$below_95) / dim(ndrdd_df_bootp)[1]

ndrdd_df_zeroed_boot <- ndrdd_df
ndrdd_df_zeroed_boot[upper_triangle] <- ndrdd_df[upper_triangle] * !ndrdd_df_bootp$below_95

rowSums(ndrdd_df) == rowSums(ndrdd_df_zeroed_boot)


hist(rowSums(ndrdd_df))
hist(rowSums(ndrdd_df_zeroed_boot))

table(rowSums(ndrdd_df))
table(rowSums(ndrdd_df_zeroed_boot))

#####Zeroed boot and negative 

ndrdd_df_zeroed_boot_nonegs <- ndrdd_df_zeroed_boot
ndrdd_df_zeroed_boot_nonegs[ndrdd_df_zeroed_boot_nonegs < 0] <- 0 




library("igraph")

class(ndrdd_df)
ndrdd_mat <- as.matrix(ndrdd_df)
colnames(ndrdd_mat) <- colnames(ndrdd_df)
rownames(ndrdd_mat) <- colnames(ndrdd_df)
rawgraph <- graph_from_adjacency_matrix(as.matrix(ndrdd_mat), weighted = T, mode = "upper")

V(rawgraph)$names
E(rawgraph)$weight
plot(rawgraph)
zero_ci_mat <- as.matrix(ndrdd_df_zeroed_ci)
colnames(zero_ci_mat) <- colnames(ndrdd_df_zeroed_ci)
rownames(zero_ci_mat) <- colnames(ndrdd_df_zeroed_ci)

zero_ci_graph <- graph_from_adjacency_matrix(as.matrix(zero_ci_mat), weighted = T, mode = "upper")
zero_ci_graph
########################

zero_boot_mat <- as.matrix(ndrdd_df_zeroed_boot)
colnames(zero_boot_mat) <- colnames(ndrdd_df_zeroed_boot)
rownames(zero_boot_mat) <- colnames(ndrdd_df_zeroed_boot)
zero_boot_graph <- graph_from_adjacency_matrix(as.matrix(zero_boot_mat), weighted = T, mode = "upper")
zero_boot_graph

########################
zero_boot_mat <- as.matrix(ndrdd_df_zeroed_boot)
colnames(zero_boot_mat) <- colnames(ndrdd_df_zeroed_boot)
rownames(zero_boot_mat) <- colnames(ndrdd_df_zeroed_boot)
zero_boot_graph <- graph_from_adjacency_matrix(as.matrix(zero_boot_mat), weighted = T, mode = "upper")
zero_boot_graph

########################

V(zero_ci_graph)$name
E(zero_ci_graph)$weight

edge_density(rawgraph)
edge_density(zero_ci_graph)
edge_density(zero_boot_graph)

plot(zero_boot_graph)
degree(zero_boot_graph)

V(zero_boot_graph)$node.size <- degree(zero_boot_graph)


edge_colors <- ifelse(E(zero_boot_graph)$weight > 0, "blue", "red")
E(zero_boot_graph)$weight <- abs(E(zero_boot_graph)$weight)
node_degree <- degree(zero_boot_graph)

library(scales)

# Plot the graph
min_size <- 0.5
max_size <- 1

# Scale node degrees to be between min_size and max_size
scaled_node_degree <- rescale(node_degree, center = FALSE, to = c(min_size, max_size))

plot(
  zero_boot_graph,
  edge.color=edge_colors,
  edge.width=E(zero_boot_graph)$weight,
  main="Network with Positive and Negative Edge Weights",
  vertex.size = 0,
  vertex.label.cex = scaled_node_degree
)

#############################################

zero_boot_nonegs_mat <- as.matrix(ndrdd_df_zeroed_boot_nonegs)
colnames(zero_boot_nonegs_mat) <- colnames(ndrdd_df_zeroed_boot_nonegs)
rownames(zero_boot_nonegs_mat) <- colnames(ndrdd_df_zeroed_boot_nonegs)
zero_boot_nonegs_graph <- graph_from_adjacency_matrix(as.matrix(zero_boot_nonegs_mat), weighted = T, mode = "upper")
zero_boot_nonegs_graph

########################


edge_density(rawgraph)
edge_density(zero_ci_graph)
edge_density(zero_boot_graph)
edge_density(zero_boot_nonegs_graph)

V(zero_boot_nonegs_graph)$node.size <- degree(zero_boot_nonegs_graph)

edge_colors <- ifelse(E(zero_boot_nonegs_graph)$weight > 0, "blue", "white")
E(zero_boot_nonegs_graph)$weight <- abs(E(zero_boot_nonegs_graph)$weight)
node_degree <- degree(zero_boot_nonegs_graph)

library(scales)

# Plot the graph
min_size <- 0.5
max_size <- 1

# Scale node degrees to be between min_size and max_size
scaled_node_degree <- rescale(node_degree, center = FALSE, to = c(min_size, max_size))

plot(
  zero_boot_nonegs_graph,
  edge.color=edge_colors,
  edge.width=E(zero_boot_graph)$weight,
  main="Network with Positive and Negative Edge Weights",
  vertex.size = 0,
  vertex.label.cex = scaled_node_degree
)







#install.packages("writexl")
library(writexl)
ndrdd_df_zeroed_boot$nodes <- colnames(ndrdd_df_zeroed_boot)
library(dplyr)
ndrdd_df_zeroed_boot <- ndrdd_df_zeroed_boot %>%
  select(nodes, everything())

write_xlsx(ndrdd_df_zeroed_boot, "ndrdd matrix zeroed under 95 percent.xlsx")


ndrdd_df_zeroed_boot_nonegs$nodes <- colnames(ndrdd_df_zeroed_boot_nonegs)
ndrdd_df_zeroed_boot_nonegs <- ndrdd_df_zeroed_boot_nonegs %>%
  select(nodes, everything())

write_xlsx(ndrdd_df_zeroed_boot_nonegs, "ndrdd matrix zeroed under 95 percent no negative ties.xlsx")


