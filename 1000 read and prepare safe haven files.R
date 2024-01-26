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

rownames(full_link_df) <- full_link_df$X
full_link_df$X <- NULL
names(full_link_df_cis) <- c("Tri_location", "lci", "uci")
 ###Check if confidence interval contains zero
full_link_df_cis$contains_zero = (full_link_df_cis['lci'] <= 0) & (full_link_df_cis['uci'] >= 0)

##Set matrix to zero where CI contains null
upper_triangle <- upper.tri(full_link_df)

length(as.vector(as.matrix(upper_triangle)))

# Set the values in the matrix to zero based on the 'contains_zero' column
full_link_df_zeroed_ci <- full_link_df
full_link_df_zeroed_ci[upper_triangle] <- full_link_df[upper_triangle] * !full_link_df_cis$contains_zero

rowSums(full_link_df) == rowSums(full_link_df_zeroed_ci)

########################################################################################################
# Set the values in the matrix to zero based on proportion of included edges in bootstrap replications #
########################################################################################################

dim(full_link_df_bootp)
hist(full_link_df_bootp$eip)

upper_triangle_logical <- upper.tri(full_link_df, diag = F)
upper_triangle_vector <- full_link_df[upper_triangle_logical]

####################


# # Your data
# data <- data.frame(x = upper_triangle_vector, y = full_link_df_bootp$eip)
# # Specify the fraction of data to keep (adjust as needed)
# sample_fraction <- 0.1
# # Take a random sample of the data
# sampled_data <- data %>% sample_frac(sample_fraction)
# # Create the ggplot object
# p <- ggplot(sampled_data, aes(x = x, y = y)) +
#   geom_point() +
#   labs(x = "X-axis", y = "Y-axis") +
#   theme_minimal()
# # Add more x-axis ticks
# p + scale_x_continuous(breaks = seq(-1, 1, by = 0.1))
# # Function to add red vertical lines
# add_red_vertical_line_ggplot <- function(p, position) {
#   p + geom_vline(xintercept = position, color = "red")
# }
# # Call the function with the desired positions
# p_with_lines <- add_red_vertical_line_ggplot(p, -0.1)
# p_with_lines <- add_red_vertical_line_ggplot(p_with_lines, 0.1)
# # Display the plot
# print(p_with_lines)
####################

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
# edge_list <- edge_list[edge_list$edge_weights > 0.04,] 
# dim(edge_list)
# writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.04.xlsx")


edge_list <- edge_list[edge_list$edge_weights >= 0.05,] 
dim(edge_list)
writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.05.xlsx")

# edge_list <- edge_list[edge_list$edge_weights > 0.08,] 
# dim(edge_list)
# writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.08.xlsx")
# 
# edge_list <- edge_list[edge_list$edge_weights > 0.1,] 
# dim(edge_list)
# writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.1.xlsx")
# 
# edge_list <- edge_list[edge_list$edge_weights > 0.20,] 
# dim(edge_list)
# writexl::write_xlsx(edge_list, path = "full_link zeroed below 0.2.xlsx")

hist(as.numeric(edge_list$edge_weights))
table(as.numeric(edge_list$edge_weights))

head(edge_list)

zero_g <-  graph_from_data_frame(edge_list, directed = F)

#loop through increasing resolution parameters for the louvain
# algorithm. Higher resolution = more communities
for (res in seq(from = 1, to = 5, by = 0.5)) {
  set.seed(492278)
  clust <- igraph::cluster_louvain(zero_g, resolution = res)
  V(zero_g)$community <- clust$membership
  print(table(clust$membership))
  V(zero_g)$strength <- strength(zero_g)
  size <- scale(V(zero_g)$strength, center = FALSE)
  pdf(paste0("Community circle layout resolution ", res, ".pdf"))
  plot(
    zero_g,
#    vertex.label = NA ,
   vertex.label.cex = 0.1,
#    layout = layout_with_drl(zero_g),
    layout = layout_in_circle(zero_g, order = V(zero_g)[order(V(zero_g)$community,V(zero_g)$strength )]),
    vertex.size =  size * 3,
    vertex.color = membership(clust),
    edge.width = as.numeric(E(zero_g)$edge_weights) * 0.2,
    edge.alpha = as.numeric(E(zero_g)$edge_weights) 
      
 )
  dev.off()

  pdf(paste0("Community drl layout resolution ", res, ".pdf"))
  plot(
    zero_g,
   vertex.label.cex = 0.1,
    layout = layout_with_drl(zero_g),
    vertex.size =  size * 3,
    vertex.color = membership(clust),
    edge.width = as.numeric(E(zero_g)$edge_weights) * 0.2
   )
  dev.off()

###Check source of the variable
V(zero_g)$source <- NA

#Lowercase letters
V(zero_g)$source[grep("^[a-z]+"           , V(zero_g)$name)] <- "NDRDD"
#Uppercase letters
V(zero_g)$source[grep("^[A-Z]+(?=\\.|_)"          , V(zero_g)$name, perl = T)] <- "PIS"
#Starts with Letter and 3 numbers
V(zero_g)$source[grep("^[A-Z]\\d{3}", V(zero_g)$name)] <- "SMR"
###Assign a few exceptions to usual format
#Starts with E45
V(zero_g)$source[grep("^E45", V(zero_g)$name)] <- "PIS"
#Starts with Letter, 2 numbers, then X
V(zero_g)$source[grep("^[A-Z]\\d{2}X", V(zero_g)$name)] <- "SMR"

####Save descriptive table
community_data_frame <- as.data.frame(list(variable    = V(zero_g)$name,
                                           community   = V(zero_g)$community,
                                           degree      = igraph::degree(zero_g),
                                           closeness   = igraph::closeness(zero_g, mode = "all"), 
                                           betweenness = igraph::betweenness(zero_g, directed = FALSE, normalized = TRUE),
                                           strength    = igraph::strength(zero_g, mode = "all"),
                                           source      = V(zero_g)$source
                                             ))
community_data_frame <- community_data_frame %>%
  arrange(desc(community), desc(degree)) 

##Sort by strength (weighted degree)
community_data_frame <- community_data_frame %>%
  arrange(desc(community), desc(strength)) 

 write.csv(community_data_frame, 
           paste0("Community strength sorted network resolution ",res,".csv"), 
           row.names = TRUE)

#Manuscript Table 2: Descriptions of subsystems and most strongly connected factors in each subsystem,
# identified using the louvain algorithm in the co-occurence network for linked administrative data
#  relatind to *** drug deaths in Scotland **Date** to **Date 

##The linked data file is summary file from safe haven.
# Instead, create summary from final output cleared file above.
table.df <- community_data_frame

# table.df <- readxl::read_excel("Linked data with labelled communities subsystems.xlsx")
# names(table.df)[1] <- "variable"
# 
# str(community_data_frame)
# str(table.df)
# dim(community_data_frame)
# dim(table.df)
# 
# table(table.df$community)
# table(community_data_frame$community)
# 
# table.df$betweenness <- as.numeric(table.df$betweenness)
# table.df$closeness <- as.numeric(table.df$closeness)
# 
# community_data_frame$community_outside <- community_data_frame$community
# community_data_frame$community <- NULL
# table.df <- full_join(table.df, community_data_frame, by = "variable")

#table(table.df$community, table.df$community_outside)
###### Fill in columns of the table 
###First row of table - Names of subsystems

table2 <- as.data.frame((sort(table(community_data_frame$community), decreasing = T)))
table2$central_factor_1     <- NA
table2$central_factor_deg_1 <- NA
table2$central_factor_btw_1 <- NA
table2$central_factor_2     <- NA
table2$central_factor_deg_2 <- NA
table2$central_factor_btw_2 <- NA
table2$central_factor_3     <- NA
table2$central_factor_deg_3 <- NA
table2$central_factor_btw_3 <- NA

table2$central_factor_4     <- NA
table2$central_factor_deg_4 <- NA
table2$central_factor_btw_4 <- NA

table2$central_factor_5     <- NA
table2$central_factor_deg_5 <- NA
table2$central_factor_btw_5 <- NA


names(table2)
for (i in table2$Var1){
  table2$central_factor_1[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][1,1]
  table2$central_factor_deg_1[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][1,3]

  table2$central_factor_2[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][2,1]
  table2$central_factor_deg_2[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][2,3]

  table2$central_factor_3[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][3,1]
  table2$central_factor_deg_3[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][3,3]

  table2$central_factor_4[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][4,1]
  table2$central_factor_deg_4[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][4,3]

  table2$central_factor_5[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][5,1]
  table2$central_factor_deg_5[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][5,3]

  table2$central_factor_btw_1[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][1,"betweenness"],2)
  table2$central_factor_btw_2[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][2,"betweenness"],2)
  table2$central_factor_btw_3[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][3,"betweenness"],2)
  table2$central_factor_btw_4[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][4,"betweenness"],2)
  table2$central_factor_btw_5[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][5,"betweenness"],2)

  }
names(table2)[1:2] <- c("Subsystem","Number of factors")

table2$central_factor_1 <- sapply(table2$central_factor_1, function(x) paste(x, collapse = ','))
table2$central_factor_2 <- sapply(table2$central_factor_2, function(x) paste(x, collapse = ','))
table2$central_factor_3 <- sapply(table2$central_factor_3, function(x) paste(x, collapse = ','))
table2$central_factor_4 <- sapply(table2$central_factor_4, function(x) paste(x, collapse = ','))
table2$central_factor_5 <- sapply(table2$central_factor_5, function(x) paste(x, collapse = ','))
table2$central_factor_deg_1 <- sapply(table2$central_factor_deg_1, function(x) paste(x, collapse = ','))
table2$central_factor_deg_2 <- sapply(table2$central_factor_deg_2, function(x) paste(x, collapse = ','))
table2$central_factor_deg_3 <- sapply(table2$central_factor_deg_3, function(x) paste(x, collapse = ','))
table2$central_factor_deg_4 <- sapply(table2$central_factor_deg_4, function(x) paste(x, collapse = ','))
table2$central_factor_deg_5 <- sapply(table2$central_factor_deg_5, function(x) paste(x, collapse = ','))

table2$central_factor_btw_1 <- sapply(table2$central_factor_btw_1, function(x) paste(x, collapse = ','))
table2$central_factor_btw_2 <- sapply(table2$central_factor_btw_2, function(x) paste(x, collapse = ','))
table2$central_factor_btw_3 <- sapply(table2$central_factor_btw_3, function(x) paste(x, collapse = ','))
table2$central_factor_btw_4 <- sapply(table2$central_factor_btw_4, function(x) paste(x, collapse = ','))
table2$central_factor_btw_5 <- sapply(table2$central_factor_btw_5, function(x) paste(x, collapse = ','))

#write.csv(table2, file = "linked data subsystems and 5 central factors.csv")
names(table2)

####New long format
factors <-  dplyr::select(table2,
                   c("Subsystem","Number of factors",
                     "central_factor_1",     
                     "central_factor_2",  
                     "central_factor_3",
                     "central_factor_4",   
                     "central_factor_5"))
degs <-  dplyr::select(table2,
                   c("Subsystem","Number of factors",
                     "central_factor_deg_1", 
                     "central_factor_deg_2", 
                     "central_factor_deg_3",
                     "central_factor_deg_4",
                     "central_factor_deg_5")) 
btws <-  dplyr::select(table2,
                   c("Subsystem","Number of factors",
                     "central_factor_btw_1",
                     "central_factor_btw_2",
                     "central_factor_btw_3",   
                     "central_factor_btw_4",     
                     "central_factor_btw_5"))

factors <- reshape2::melt(factors, id.vars = c("Subsystem", "Number of factors"), value.name = "factor" )
degs    <- reshape2::melt(degs   , id.vars = c("Subsystem", "Number of factors"), value.name = "degree" )
btws    <- reshape2::melt(btws   , id.vars = c("Subsystem", "Number of factors"), value.name = "betweenness" )

factors$variable <- as.numeric(factors$variable)
degs$variable    <- as.numeric(degs$variable)
btws$variable    <- as.numeric(btws$variable)

tt <- dplyr::full_join(factors, degs)
tt <- dplyr::full_join(tt, btws)
table2 <- tt[order(tt$`Number of factors` , tt$Subsystem , decreasing = T),]


write.csv(table2, 
          file = paste0("Table 2 linked data subsystems and 5 factors resolution ",res,".csv"))

} # End of resolution loop 
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
