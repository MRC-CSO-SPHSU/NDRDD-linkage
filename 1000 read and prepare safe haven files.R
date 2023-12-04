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
# full_link_df <- read.csv("full_link weighted adjacency.csv")
# full_link_df_cis <- read.csv("full_link weighted adjacency CIs.csv")
# full_link_df_bootp <- read.csv("full_link weighted adjacency bootstrap proportions.csv")

full_link_df <- read.csv("full_link weighted adjacency with obs above 20.csv")
full_link_df_cis <- read.csv("full_link weighted adjacency CIs with obs above 20.csv")
full_link_df_bootp <- read.csv("full_link weighted adjacency bootstrap proportions with obs above 20.csv")



dim(full_link_df)
names(full_link_df)

rownames(full_link_df) <- full_link_df$X
full_link_df$X <- NULL

dim(full_link_df_cis)
dim(full_link_df_bootp)

names(full_link_df_cis) <- c("Tri_location", "lci", "uci")
###Check if confidence interval contains zero
full_link_df_cis$contains_zero = (full_link_df_cis['lci'] <= 0) & (full_link_df_cis['uci'] >= 0)

##Set matrix to zero where CI contains null
upper_triangle <- upper.tri(full_link_df)
# Set the values in the matrix to zero based on the 'contains_zero' column
full_link_df_zeroed_ci <- full_link_df
full_link_df_zeroed_ci[upper_triangle] <- full_link_df[upper_triangle] * !full_link_df_cis$contains_zero

rowSums(full_link_df) == rowSums(full_link_df_zeroed_ci)

hist(rowSums(full_link_df))
hist(rowSums(full_link_df_zeroed_ci))

table(rowSums(full_link_df))
table(rowSums(full_link_df_zeroed_ci))

########################################################################################################
# Set the values in the matrix to zero based on proportion of included edges in bootstrap replications #
########################################################################################################

dim(full_link_df_bootp)
hist(full_link_df_bootp$eip)

upper_triangle_logical <- upper.tri(full_link_df, diag = F)
upper_triangle_vector <- full_link_df[upper_triangle_logical]

length(upper_triangle_vector)

table(upper_triangle_vector,full_link_df_bootp$eip)
reg <- lm( upper_triangle_vector ~ full_link_df_bootp$eip)


####################

library(ggplot2)
library(dplyr)

# Your data
data <- data.frame(x = upper_triangle_vector, y = full_link_df_bootp$eip)

# Specify the fraction of data to keep (adjust as needed)
sample_fraction <- 0.1

# Take a random sample of the data
sampled_data <- data %>% sample_frac(sample_fraction)

# Create the ggplot object
p <- ggplot(sampled_data, aes(x = x, y = y)) +
  geom_point() +
  labs(x = "X-axis", y = "Y-axis") +
  theme_minimal()

# Add more x-axis ticks
p + scale_x_continuous(breaks = seq(-1, 1, by = 0.1))

# Function to add red vertical lines
add_red_vertical_line_ggplot <- function(p, position) {
  p + geom_vline(xintercept = position, color = "red")
}

# Call the function with the desired positions
p_with_lines <- add_red_vertical_line_ggplot(p, -0.1)
p_with_lines <- add_red_vertical_line_ggplot(p_with_lines, 0.1)

# Display the plot
print(p_with_lines)


####################






dim(upper.tri(full_link_df))

dim(full_link_df)

full_link_df_bootp$below_95 <- full_link_df_bootp$eip < 0.95
table(full_link_df_bootp$below_95)
table(full_link_df_bootp$below_95) / dim(full_link_df_bootp)[1]

full_link_df_zeroed_boot <- full_link_df
full_link_df_zeroed_boot[upper_triangle] <- full_link_df[upper_triangle] * !full_link_df_bootp$below_95

rowSums(full_link_df) == rowSums(full_link_df_zeroed_boot)


hist(rowSums(full_link_df))
hist(rowSums(full_link_df_zeroed_boot))

table(rowSums(full_link_df))
table(rowSums(full_link_df_zeroed_boot))

#####Zeroed boot and negative 

full_link_df_zeroed_boot_nonegs <- full_link_df_zeroed_boot
full_link_df_zeroed_boot_nonegs[full_link_df_zeroed_boot_nonegs < 0] <- 0 


#####Zero low correlations 

full_link_zeroed_boot_below20 <- full_link_df

full_link_zeroed_boot_below20[abs(full_link_zeroed_boot_below20) < 0.20] <- 0 


library("igraph")

class(full_link_df)
full_link_mat <- as.matrix(full_link_df)
colnames(full_link_mat) <- colnames(full_link_df)
rownames(full_link_mat) <- colnames(full_link_df)
rawgraph <- graph_from_adjacency_matrix(as.matrix(full_link_mat), weighted = T, mode = "upper")


###############
# Convert the graph object to an edge list
edge_list <- get.edgelist(rawgraph)
edge_weights <- E(rawgraph)$weight
edge_list <- cbind(edge_list, edge_weights)
edge_list <- as.data.frame(edge_list)
dim(edge_list)
###Remove corr below x 
edge_list <- edge_list[edge_list$edge_weights > 0.04,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.04.xlsx")


edge_list <- edge_list[edge_list$edge_weights > 0.05,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.05.xlsx")

edge_list <- edge_list[edge_list$edge_weights > 0.08,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.08.xlsx")

edge_list <- edge_list[edge_list$edge_weights > 0.1,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.1.xlsx")

edge_list <- edge_list[edge_list$edge_weights > 0.20,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.2.xlsx")

hist(as.numeric(edge_list$edge_weights))
table(as.numeric(edge_list$edge_weights))

# 
# 
# 
# 
# V(rawgraph)$names
# E(rawgraph)$weight
# plot(rawgraph)
# zero_ci_mat <- as.matrix(full_link_df_zeroed_ci)
# colnames(zero_ci_mat) <- colnames(full_link_df_zeroed_ci)
# rownames(zero_ci_mat) <- colnames(full_link_df_zeroed_ci)
# 
# zero_ci_graph <- graph_from_adjacency_matrix(as.matrix(zero_ci_mat), weighted = T, mode = "upper")
# zero_ci_graph
# ########################
# 
# zero_boot_mat <- as.matrix(full_link_df_zeroed_boot)
# colnames(zero_boot_mat) <- colnames(full_link_df_zeroed_boot)
# rownames(zero_boot_mat) <- colnames(full_link_df_zeroed_boot)
# zero_boot_graph <- graph_from_adjacency_matrix(as.matrix(zero_boot_mat), weighted = T, mode = "upper")
# zero_boot_graph
# 
# ########################
# zero_boot_mat <- as.matrix(full_link_df_zeroed_boot)
# colnames(zero_boot_mat) <- colnames(full_link_df_zeroed_boot)
# rownames(zero_boot_mat) <- colnames(full_link_df_zeroed_boot)
# zero_boot_graph <- graph_from_adjacency_matrix(as.matrix(zero_boot_mat), weighted = T, mode = "upper")
# zero_boot_graph
# 
# ########################
# below20_mat <- as.matrix(full_link_zeroed_boot_below20)
# colnames(below20_mat) <- colnames(full_link_zeroed_boot_below20)
# rownames(below20_mat) <- colnames(full_link_zeroed_boot_below20)
# below20_graph <- graph_from_adjacency_matrix(as.matrix(below20_mat), weighted = T, mode = "upper")
# 
# zero_ci_mat2 <- as_adjacency_matrix(zero_ci_graph, type = "upper", sparse = F, attr = "weight")
# 
# g <- graph.adjacency(zero_ci_mat2, mode = "undirected", weighted = TRUE)
# # Convert the graph object to an edge list
# edge_list <- get.edgelist(g)
# edge_weights <- E(g)$weight
# edge_list <- cbind(edge_list, edge_weights)
# edge_list <- as.data.frame(edge_list)
# dim(edge_list)
# writexl::write_xlsx(edge_list, path = "full_link edgelist zeroed null ci.xlsx")
# ###Remove corr below 0.05
# edge_list <- edge_list[edge_list$edge_weights > 0.1,] 
# dim(edge_list)
# writexl::write_xlsx(edge_list, path = "full_link edgelist zeroed null ci and below 0.1.xlsx")
# 
# 
# 
# 
# 
# 
# 
# getwd()
# head(edge_list)
# table(edge_list[,3])
# 
# 
# hist(rowSums(zero_ci_mat))
# hist(rowSums(as.matrix(zero_ci_mat2)))
# table(rowSums(as.matrix(zero_ci_mat2)))
# 
# zero_ci_mat2 <- as.data.frame(zero_ci_mat2)
# zero_ci_mat2$node <- names(zero_ci_mat2)
# library(dplyr)
# 
# zero_ci_mat2 <- zero_ci_mat2 %>%
#   select(node, everything())
# 
# writexl::write_xlsx(zero_ci_mat2, path = "full_link matrix zeroed null ci.xlsx")
# 
# 
# getwd()
# 
# V(zero_ci_graph)$name
# E(zero_ci_graph)$weight
# 
# edge_density(rawgraph)
# edge_density(zero_ci_graph)
# edge_density(zero_boot_graph)
# edge_density(below20_graph)
# 
# 
# plot(zero_boot_graph)
# degree(zero_boot_graph)
# 
# V(zero_boot_graph)$node.size <- degree(zero_boot_graph)
# 
# 
# edge_colors <- ifelse(E(zero_boot_graph)$weight > 0, "blue", "red")
# E(zero_boot_graph)$weight <- abs(E(zero_boot_graph)$weight)
# node_degree <- degree(zero_boot_graph)
# 
# library(scales)
# 
# # Plot the graph
# min_size <- 0.5
# max_size <- 1
# 
# # Scale node degrees to be between min_size and max_size
# scaled_node_degree <- rescale(node_degree, center = FALSE, to = c(min_size, max_size))
# 
# plot(
#   zero_boot_graph,
#   edge.color=edge_colors,
#   edge.width=E(zero_boot_graph)$weight,
#   main="Network with Positive and Negative Edge Weights",
#   vertex.size = 0,
#   vertex.label.cex = scaled_node_degree
# )
# 
# #############################################
# 
# zero_boot_nonegs_mat <- as.matrix(full_link_df_zeroed_boot_nonegs)
# colnames(zero_boot_nonegs_mat) <- colnames(full_link_df_zeroed_boot_nonegs)
# rownames(zero_boot_nonegs_mat) <- colnames(full_link_df_zeroed_boot_nonegs)
# zero_boot_nonegs_graph <- graph_from_adjacency_matrix(as.matrix(zero_boot_nonegs_mat), weighted = T, mode = "upper")
# zero_boot_nonegs_graph
# 
# ########################
# 
# 
# V(below20_graph)$node.size <- degree(below20_graph)
# 
# edge_colors <- ifelse(E(below20_graph)$weight > 0, "blue", "white")
# E(below20_graph)$weight <- abs(E(below20_graph)$weight)
# node_degree <- degree(below20_graph)
# 
# library(scales)
# 
# # Plot the graph
# min_size <- 0.5
# max_size <- 1
# 
# # Scale node degrees to be between min_size and max_size
# scaled_node_degree <- rescale(node_degree, center = FALSE, to = c(min_size, max_size))
# 
# plot(
#   below20_graph,
#   edge.color=edge_colors,
#   edge.width=E(below20_graph)$weight,
#   main="Network with Positive and Negative Edge Weights",
#   vertex.size = 0,
#   vertex.label.cex = scaled_node_degree,
#   #layout_in_circle(below20_graph)
#   #layout =layout.graphopt(below20_graph)
#   #layout.drl(below20_graph)
#   #layout.circle(below20_graph)
#   )
# 
# V(zero_boot_nonegs_graph)[degree(zero_boot_nonegs_graph)==0]
# 
# 
# 
# V(zero_boot_nonegs_graph)$node.size <- degree(zero_boot_nonegs_graph)
# 
# edge_colors <- ifelse(E(zero_boot_nonegs_graph)$weight > 0, "blue", "white")
# E(zero_boot_nonegs_graph)$weight <- abs(E(zero_boot_nonegs_graph)$weight)
# node_degree <- degree(zero_boot_nonegs_graph)
# 
# library(scales)
# 
# # Plot the graph
# min_size <- 0.5
# max_size <- 1
# 
# # Scale node degrees to be between min_size and max_size
# scaled_node_degree <- rescale(node_degree, center = FALSE, to = c(min_size, max_size))
# 
# plot(
#   zero_boot_nonegs_graph,
#   edge.color=edge_colors,
#   edge.width=E(zero_boot_graph)$weight,
#   main="Network with Positive and Negative Edge Weights",
#   vertex.size = 0,
#   vertex.label.cex = scaled_node_degree
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #install.packages("writexl")
# library(writexl)
# full_link_df_zeroed_boot$nodes <- colnames(full_link_df_zeroed_boot)
# library(dplyr)
# full_link_df_zeroed_boot <- full_link_df_zeroed_boot %>%
#   select(nodes, everything())
# 
# write_xlsx(full_link_df_zeroed_boot, "full_link matrix zeroed under 95 percent.xlsx")
# 
# 
# full_link_df_zeroed_boot_nonegs$nodes <- colnames(full_link_df_zeroed_boot_nonegs)
# full_link_df_zeroed_boot_nonegs <- full_link_df_zeroed_boot_nonegs %>%
#   select(nodes, everything())
# 
# write_xlsx(full_link_df_zeroed_boot_nonegs, "full_link matrix zeroed under 95 percent no negative ties.xlsx")
# 
# 
