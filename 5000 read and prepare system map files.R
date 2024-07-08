# Preparation 

# This script reads in the yED co-produced map, creates and edgelist,
# detects communities, estimates degree and closeness, and creates a plot where
# communities are represented with colours.

# Clean work space
rm(list=ls())
gc(full=TRUE)

#########################
#                       #
#    Load packages      #
#                       #
#########################
library("rmarkdown")
library("tinytex")
library("knitr")
library("comato")
library("stringi")
library("igraph")
library("ggraph")
library("ggplot2")
library("stringr")
library("tidygraph")
library("psych")
library("openxlsx")

#Set working directory
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(wd,"/Data"))

#Load raw data from comato Read.Yed
raw_data <- comato::read.yEd("DRD_SystemMap.graphml") 
plot(raw_data$map)
# 98 "concepts" or nodes with 224 edges

edges_raw_data <- as.matrix(raw_data)[,1:2] [,1:2] 

# Need to tidy label names to split at first capital letter
node_names <- data.frame(edges_raw_data) 

node_names <- node_names %>%
  dplyr::rename(from = X1,
                to = X2)

node_names <- node_names %>%
  dplyr::mutate(from_start_position = stri_locate_first_regex(from, "[A-Z]+"),
                from_end_position = str_length(node_names$from),
                to_start_position = stri_locate_first_regex(to, "[A-Z]+"), 
                to_end_position = str_length(node_names$to))

node_names <- node_names %>% 
  dplyr::mutate(from_labels = substr(from, from_start_position, from_end_position)) %>% 
  dplyr::mutate(to_labels = substr(to, to_start_position, to_end_position)) %>%
  dplyr::mutate(from_labels = str_replace_all(from_labels, "([[\n]])", "")) %>%
  dplyr::mutate(to_labels = str_replace_all(to_labels, "([[\n]])", "")) %>%
  dplyr::select(from_labels, to_labels) %>%
  tibble::remove_rownames()

str(node_names)

write.csv(node_names, file = paste0("System map edgelist.csv"))

#Create Kumu file

kumu_file <- createWorkbook()

# Add the first sheet and write data
vec <- c(node_names$from_labels, node_names$to_labels)
vec <- unique(vec)
elements <- data.frame(Label = vec)

addWorksheet(wb = kumu_file, sheetName = "Elements")
writeData(wb = kumu_file, sheet = "Elements", x = elements)

# Add the second sheet and write data
names(node_names) <- c("From","To")

addWorksheet(wb = kumu_file, sheetName = "Connections")
writeData(wb = kumu_file, sheet = "Connections", x = node_names)

# Save the XLSX file
unlink("DRD_Kumu_file.xlsx")
saveWorkbook(kumu_file, file = "DRD_Kumu_file.xlsx")

# node names into igraph object for graphing
graph_raw_data <- igraph::graph_from_edgelist(as.matrix(node_names))
plot(graph_raw_data)

V(graph_raw_data)$size <- igraph::degree(graph_raw_data, 
                                         mode = "total")

V(graph_raw_data)$name

# improve visualization 

ggraph(graph_raw_data, layout = "fr") +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) + 
  geom_node_point(size = V(graph_raw_data)$size ) +
  geom_node_text(aes(label = V(graph_raw_data)$name),
                 repel = TRUE, max.overlaps = Inf, 
                 color = "black", size = 2)


# community detection and vertex attributes
# cfg <- igraph::cluster_fast_greedy(as.undirected(graph_raw_data))
set.seed(428)
cfg <- igraph::cluster_louvain(as.undirected(graph_raw_data), resolution = 1)

plot1 <- plot(cfg, as.undirected(graph_raw_data))

V(graph_raw_data)$community <- cfg$membership
V(graph_raw_data)$closeness <- igraph::closeness(graph_raw_data, 
                                                 mode = "total")

V(graph_raw_data)$in_degree <- igraph::degree(graph_raw_data, mode = "in")
V(graph_raw_data)$out_degree <- igraph::degree(graph_raw_data, mode = "out")

#colrs_vertex <- c( '#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666')

colrs_vertex <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#999999','#a65628','#f781bf' )

colr_edges <- "#808080"

E(graph_raw_data)$color <- "808080"

plot(graph_raw_data, 
     vertex.color = colrs_vertex[V(graph_raw_data)$community])

# try with ggraph 
set.seed(1212)
community_graph <- ggraph(graph_raw_data, 
                          layout = "fr") +
  geom_edge_fan(color = "grey80") +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(12, 'mm'),
                 color = "grey80") +
  geom_node_point(aes(size = V(graph_raw_data)$size), color = "white") +
  geom_node_point(aes(size = V(graph_raw_data)$size), 
                   color = colrs_vertex[V(graph_raw_data)$community], 
                   alpha = 0.7, shape = 20) +
  geom_node_text(aes(label = V(graph_raw_data)$name),
                 repel = TRUE, max.overlaps = Inf, 
                 color = "black", size = 2) +
  scale_size_continuous(range = c(2, 10)) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none")


community_graph

ggsave(paste0(wd,"/Data/Figure 1  System map Community Dectection.pdf"),
              device = "pdf", width = 13.9, height = 10.3, units = "in")

ggsave(paste0(wd,"/Data/Figure 1 System map Community Dectection.jpeg"),
       device = "jpeg", width = 13.9, height = 10.3, units = "in")

as.data.frame(V(graph_raw_data)$community)


community_data_frame <- as.data.frame(list(vertex=V(graph_raw_data), 
                                      community = V(graph_raw_data)$community, 
                                      degree = igraph::degree(graph_raw_data),
                                      in_degree = igraph::degree(graph_raw_data, mode = "in"),
                                      out_degree = igraph::degree(graph_raw_data, mode = "out"),
                                      closeness = igraph::closeness(graph_raw_data, mode = "all"), 
                                      betweeness = igraph::betweenness(graph_raw_data, directed = TRUE, normalized = TRUE), 
                                      avg_path_length = igraph::mean_distance(graph_raw_data, directed = TRUE),
                                      density = igraph::graph.density(graph_raw_data)))
write.csv(community_data_frame, file = "Full system map with community detection and degrees.csv")

######Table 1: Subsystems returned from community detection algorithms 
#  applied to the system map, and factors in each within highest degree

table1 <- as.data.frame((sort(table(V(graph_raw_data)$community), decreasing = T)))
#table(table1$Var1, as.numeric(as.character(table1$Var1)))
#Convert to numeric
table1$Var1 <- as.numeric(as.character(table1$Var1))

table1$central_factor_1     <- NA
table1$central_factor_1_deg <- NA
table1$central_factor_1_btw <- NA
table1$central_factor_2     <- NA
table1$central_factor_2_deg <- NA
table1$central_factor_2_btw <- NA
table1$central_factor_3     <- NA
table1$central_factor_3_deg <- NA
table1$central_factor_3_btw <- NA
table1$central_factor_4     <- NA
table1$central_factor_4_deg <- NA
table1$central_factor_4_btw <- NA
table1$central_factor_5     <- NA
table1$central_factor_5_deg <- NA
table1$central_factor_5_btw <- NA



for (i in table1$Var1){
  ##Store name of var
  temp_comm_df <- community_data_frame[which(community_data_frame$community == i),]
  temp_comm_df <- temp_comm_df[order(temp_comm_df$deg , decreasing = T),]
  
  table1$central_factor_1[which(table1$Var1     == i)] <- rownames(temp_comm_df)[1]
  #Store degree of var
  table1$central_factor_1_deg[which(table1$Var1 == i)] <- temp_comm_df[1,"degree"]
  table1$central_factor_1_btw[which(table1$Var1 == i)] <- round(temp_comm_df[1,"betweeness"],2)

  table1$central_factor_2[which(table1$Var1     == i)] <- rownames(temp_comm_df)[2]
  table1$central_factor_2_deg[which(table1$Var1 == i)] <- temp_comm_df[2,"degree"]
  table1$central_factor_2_btw[which(table1$Var1 == i)] <- round(temp_comm_df[2,"betweeness"],2)

  table1$central_factor_3[which(table1$Var1     == i)] <- rownames(temp_comm_df)[3]
  table1$central_factor_3_deg[which(table1$Var1 == i)] <- temp_comm_df[3,"degree"]
  table1$central_factor_3_btw[which(table1$Var1 == i)] <- round(temp_comm_df[3,"betweeness"],2)

  table1$central_factor_4[which(table1$Var1     == i)] <- rownames(temp_comm_df)[4]
  table1$central_factor_4_deg[which(table1$Var1 == i)] <- temp_comm_df[4,"degree"]
  table1$central_factor_4_btw[which(table1$Var1 == i)] <- round(temp_comm_df[4,"betweeness"],2)

  table1$central_factor_5[which(table1$Var1     == i)] <- rownames(temp_comm_df)[5]
  table1$central_factor_5_deg[which(table1$Var1 == i)] <- temp_comm_df[5,"degree"]
  table1$central_factor_5_btw[which(table1$Var1 == i)] <- round(temp_comm_df[5,"betweeness"],2)
   #  rm(temp_comm_df)
  }
names(table1)[1:2] <- c("Subsystem","Number of factors")

new_names <- sub("^(.*)(\\d+)(_.*)$", "\\1\\3\\2", names(table1))
names(table1) <- new_names


  
write.csv(table1, file = "Subsystems nodes top 5 factors wide.csv")

####New long format
factors <-  select(table1,
                   c("Subsystem","Number of factors",
                     "central_factor_1",     
                     "central_factor_2",  
                     "central_factor_3",
                     "central_factor_4",   
                     "central_factor_5"))
degs <-  select(table1,
                   c("Subsystem","Number of factors",
                     "central_factor__deg1", 
                     "central_factor__deg2", 
                     "central_factor__deg3",
                     "central_factor__deg4",
                     "central_factor__deg5")) 

btws <-  select(table1,
                   c("Subsystem","Number of factors",
                     "central_factor__btw1",
                     "central_factor__btw2",
                     "central_factor__btw3",   
                     "central_factor__btw4",     
                     "central_factor__btw5"))

factors <- reshape2::melt(factors, id.vars = c("Subsystem", "Number of factors"), value.name = "factor" )
degs    <- reshape2::melt(degs   , id.vars = c("Subsystem", "Number of factors"), value.name = "degree" )
btws    <- reshape2::melt(btws   , id.vars = c("Subsystem", "Number of factors"), value.name = "betweenness" )

factors$variable <- as.numeric(factors$variable)
degs$variable    <- as.numeric(degs$variable)
btws$variable    <- as.numeric(btws$variable)

tt <- dplyr::full_join(factors, degs)
tt <- dplyr::full_join(tt, btws)
table1 <- tt[order(tt$`Number of factors` , tt$Subsystem , decreasing = T),]

write.csv(table1, file = "Table 1 subsystems nodes deg btw.csv")


edge_list <- as.data.frame(E(graph_raw_data))


community_data_frame <- community_data_frame %>%
  dplyr::arrange(community) %>%
  tibble::rownames_to_column()

# Create summary table for measures 

in_degree <- as.data.frame(c(psych::describe(community_data_frame$in_degree))) 
in_degree <- in_degree %>%
  dplyr::mutate(variable = "in_degree")

out_degree <- as.data.frame(c(psych::describe(community_data_frame$out_degree))) 
out_degree <- out_degree %>%
  dplyr::mutate(variable = "out_degree")   

degree_centrality <- as.data.frame(c(psych::describe(community_data_frame$degree))) 
degree_centrality <- degree_centrality %>%
  dplyr::mutate(variable = "degree_centrality")   

closeness_centrality <- as.data.frame(c(psych::describe(community_data_frame$closeness))) 
closeness_centrality <- closeness_centrality %>%
  dplyr::mutate(variable = "closeness_centrality")   

betweeness_centrality <- as.data.frame(c(psych::describe(community_data_frame$betweeness))) 
betweeness_centrality <- betweeness_centrality %>%
  dplyr::mutate(variable = "betweeness_centrality")   


network_summary_measures <- rbind(in_degree, 
                                  out_degree, 
                                  degree_centrality, 
                                  closeness_centrality, 
                                  betweeness_centrality)

network_summary_measures <- network_summary_measures %>%
  dplyr::relocate(variable, .before = mean ) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  dplyr::select(variable, mean, min, max, sd )

write.csv(network_summary_measures, 
          paste0(wd,"/Data/TableNetworkSummaryMeasures.csv"), 
          row.names = TRUE)

nodes_with_maximums <- community_data_frame %>%
  filter(in_degree == max(community_data_frame$in_degree) |
           out_degree == max(community_data_frame$out_degree) |
           degree == max(community_data_frame$degree) | 
           closeness == max(community_data_frame$closeness) |
           betweeness == max(community_data_frame$betweeness)) %>%
  dplyr::mutate_if(is.numeric, round, 3) %>%
  select(community, degree, in_degree, out_degree, closeness, betweeness)


write.csv(nodes_with_maximums, 
          paste0(wd,"/Data/TableMaximumValues.csv"), 
          row.names = TRUE)


# select top 10 for degree
TableDegree <- community_data_frame %>%
  arrange(desc(degree)) 

TableDegree <- head(TableDegree, 20)

write.csv(TableDegree, 
          paste0(wd,"/Data/TableTopDegree.csv"), 
          row.names = TRUE)

# Highest degree within each community 
TableWithinCommunty <- community_data_frame %>%
  group_by(community) %>%
  filter(degree == max(degree)) %>%
  ungroup()

write.csv(TableWithinCommunty, 
          paste0(wd,"/Data/TableWithinCommunty.csv"), 
          row.names = TRUE)

base::saveRDS(community_data_frame, paste0(wd,"/CoProducedCommunities.rds"))
write.csv(community_data_frame, paste0(wd,"/CoProducedCommunities.csv"))


# ggsave(paste0(wd,"/Data/Figure 1 System map.pdf"),
#          device = "pdf", width = 25, height = 15, units = "in")

# 
# pdf(file = "up_down_stream_plot.pdf")
# 
# set.seed(3000)
# up_down_stream_plot <- plot(graph_raw_data, 
#      mode = "degree", 
#      displaylabels = TRUE,
#      boxed.labels = FALSE, 
#      suppress.axes = FALSE, 
#      label.cex = 1.2,  
#      vertex.col = 'SEM',
#      xlab = "Indegree", 
#      ylab = "Outdegree", 
#      label.col = 1, main = "",
#      edge.arrow.size = 0.1, 
#      edge.arrow.width	= 0.1,
#      edge.label.family	= "Arial",
#      vertex.color = colrs_vertex[V(graph_raw_data)$community], 
#      vertex.size = V(graph_raw_data)$size*0.5, 
#      vertex.frame.color = "gray", 
#      vertex.label.color = "black", 
#      vertex.label.cex = 0.15, 
#      vertex.label.dist = 0.01, 
#      edge.curved = 0.3, 
#      edge.width = 0.1)
# 
# dev.off()
# 
# plotsave(paste0(wd,"/CoProducedCommunityDectection.pdf"),
#        device = "pdf", width = 13.9, height = 10.3, units = "in")
# 

###########Assess resolution changes for louvain algorithm.
# 
# 
# set.seed(2121)
# cfg <- igraph::cluster_louvain(as.undirected(graph_raw_data))
# 
# plot1 <- plot(cfg, as.undirected(graph_raw_data))
# 
# zero_g <- graph_raw_data
# 
# clust   <- list()
# comm_df <- list()
# #loop through increasing resolution parameters for the louvain
# # algorithm. Higher resolution = more communities
# 
# counter <- 1
# for (res in seq(from = 1, to = 5, by = 0.5)) {
#   set.seed(492278)
#   clust[[counter]] <- igraph::cluster_louvain(as.undirected(zero_g), resolution = res)
#   V(zero_g)$community <- clust[[counter]]$membership
#   print(table(clust[[counter]]$membership))
#   V(zero_g)$strength <- strength(zero_g)
#   size <- scale(V(zero_g)$strength, center = FALSE)
#   pdf(paste0("Community circle layout resolution ", res, ".pdf"))
#   plot(
#     zero_g,
# #    vertex.label = NA ,
#    vertex.label.cex = 0.1,
# #    layout = layout_with_drl(zero_g),
#     layout = layout_in_circle(zero_g, order = V(zero_g)[order(V(zero_g)$community,V(zero_g)$strength )]),
#     vertex.frame.width = 0,                    
#     vertex.size =  size * 3,
#     vertex.color = membership(clust[[counter]]),
#     edge.width = as.numeric(E(zero_g)$edge_weights) * 0.2,
#     edge.alpha = as.numeric(E(zero_g)$edge_weights) 
#       
#  )
#   dev.off()
# 
#   pdf(paste0("Community drl layout resolution ", res, ".pdf"))
#   plot(
#     zero_g,
#    vertex.label.cex = 0.1,
#     layout = layout_with_drl(zero_g),
#     vertex.size =  size * 3,
#       vertex.frame.width = 0,                    
#    vertex.color = membership(clust[[counter]]),
#     edge.width = as.numeric(E(zero_g)$edge_weights) * 0.2
#    )
#   dev.off()
# 
# ###Check source of the variable
# V(zero_g)$source <- NA
# 
# #Lowercase letters
# V(zero_g)$source[grep("^[a-z]+"           , V(zero_g)$name)] <- "NDRDD"
# #Uppercase letters
# V(zero_g)$source[grep("^[A-Z]+(?=\\.|_)"          , V(zero_g)$name, perl = T)] <- "PIS"
# #Starts with Letter and 3 numbers
# V(zero_g)$source[grep("^[A-Z]\\d{3}", V(zero_g)$name)] <- "SMR"
# ###Assign a few exceptions to usual format
# #Starts with E45
# V(zero_g)$source[grep("^E45", V(zero_g)$name)] <- "PIS"
# #Starts with Letter, 2 numbers, then X
# V(zero_g)$source[grep("^[A-Z]\\d{2}X", V(zero_g)$name)] <- "SMR"
# 
# ####Save descriptive table
# community_data_frame <- as.data.frame(list(variable    = V(zero_g)$name,
#                                            community   = V(zero_g)$community,
#                                            degree      = igraph::degree(zero_g),
#                                            closeness   = igraph::closeness(zero_g, mode = "all"), 
#                                            betweenness = igraph::betweenness(zero_g, directed = FALSE, normalized = TRUE),
#                                            strength    = igraph::strength(zero_g, mode = "all"),
#                                            source      = V(zero_g)$source
#                                              ))
# community_data_frame <- community_data_frame %>%
#   arrange(desc(community), desc(degree)) 
# 
# ##Sort by strength (weighted degree)
# comm_df[[counter]] <- community_data_frame %>%
#   arrange(desc(community), desc(strength)) 
# 
#  write.csv(comm_df[[counter]], 
#            paste0("Community strength sorted network resolution ",res,".csv"), 
#            row.names = F)
# 
# #Manuscript Table 2: Descriptions of subsystems and most strongly connected factors in each subsystem,
# # identified using the louvain algorithm in the co-occurence network for linked administrative data
# #  relatind to *** drug deaths in Scotland **Date** to **Date 
# 
# ##The linked data file is summary file from safe haven.
# # Instead, create summary from final output cleared file above.
# table.df <- comm_df[[counter]]
# 
# # table.df <- readxl::read_excel("Linked data with labelled communities subsystems.xlsx")
# # names(table.df)[1] <- "variable"
# # 
# # str(community_data_frame)
# # str(table.df)
# # dim(community_data_frame)
# # dim(table.df)
# # 
# # table(table.df$community)
# # table(community_data_frame$community)
# # 
# # table.df$betweenness <- as.numeric(table.df$betweenness)
# # table.df$closeness <- as.numeric(table.df$closeness)
# # 
# # community_data_frame$community_outside <- community_data_frame$community
# # community_data_frame$community <- NULL
# # table.df <- full_join(table.df, community_data_frame, by = "variable")
# 
# #table(table.df$community, table.df$community_outside)
# ###### Fill in columns of the table 
# ###First row of table - Names of subsystems
# 
# table2 <- as.data.frame((sort(table(comm_df[[counter]]$community), decreasing = T)))
# table2$central_factor_1     <- NA
# table2$central_factor_deg_1 <- NA
# table2$central_factor_btw_1 <- NA
# table2$central_factor_2     <- NA
# table2$central_factor_deg_2 <- NA
# table2$central_factor_btw_2 <- NA
# table2$central_factor_3     <- NA
# table2$central_factor_deg_3 <- NA
# table2$central_factor_btw_3 <- NA
# 
# table2$central_factor_4     <- NA
# table2$central_factor_deg_4 <- NA
# table2$central_factor_btw_4 <- NA
# 
# table2$central_factor_5     <- NA
# table2$central_factor_deg_5 <- NA
# table2$central_factor_btw_5 <- NA
# 
# 
# names(table2)
# for (i in table2$Var1){
#   table2$central_factor_1[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][1,1]
#   table2$central_factor_deg_1[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][1,3]
# 
#   table2$central_factor_2[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][2,1]
#   table2$central_factor_deg_2[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][2,3]
# 
#   table2$central_factor_3[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][3,1]
#   table2$central_factor_deg_3[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][3,3]
# 
#   table2$central_factor_4[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][4,1]
#   table2$central_factor_deg_4[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][4,3]
# 
#   table2$central_factor_5[which(table2$Var1 == i)]     <- table.df[which(table.df$community == i),][5,1]
#   table2$central_factor_deg_5[which(table2$Var1 == i)] <- table.df[which(table.df$community == i),][5,3]
# 
#   table2$central_factor_btw_1[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][1,"betweenness"],2)
#   table2$central_factor_btw_2[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][2,"betweenness"],2)
#   table2$central_factor_btw_3[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][3,"betweenness"],2)
#   table2$central_factor_btw_4[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][4,"betweenness"],2)
#   table2$central_factor_btw_5[which(table2$Var1 == i)] <- round(table.df[which(table.df$community == i),][5,"betweenness"],2)
# 
#   }
# names(table2)[1:2] <- c("Subsystem","Number of factors")
# 
# table2$central_factor_1 <- sapply(table2$central_factor_1, function(x) paste(x, collapse = ','))
# table2$central_factor_2 <- sapply(table2$central_factor_2, function(x) paste(x, collapse = ','))
# table2$central_factor_3 <- sapply(table2$central_factor_3, function(x) paste(x, collapse = ','))
# table2$central_factor_4 <- sapply(table2$central_factor_4, function(x) paste(x, collapse = ','))
# table2$central_factor_5 <- sapply(table2$central_factor_5, function(x) paste(x, collapse = ','))
# table2$central_factor_deg_1 <- sapply(table2$central_factor_deg_1, function(x) paste(x, collapse = ','))
# table2$central_factor_deg_2 <- sapply(table2$central_factor_deg_2, function(x) paste(x, collapse = ','))
# table2$central_factor_deg_3 <- sapply(table2$central_factor_deg_3, function(x) paste(x, collapse = ','))
# table2$central_factor_deg_4 <- sapply(table2$central_factor_deg_4, function(x) paste(x, collapse = ','))
# table2$central_factor_deg_5 <- sapply(table2$central_factor_deg_5, function(x) paste(x, collapse = ','))
# 
# table2$central_factor_btw_1 <- sapply(table2$central_factor_btw_1, function(x) paste(x, collapse = ','))
# table2$central_factor_btw_2 <- sapply(table2$central_factor_btw_2, function(x) paste(x, collapse = ','))
# table2$central_factor_btw_3 <- sapply(table2$central_factor_btw_3, function(x) paste(x, collapse = ','))
# table2$central_factor_btw_4 <- sapply(table2$central_factor_btw_4, function(x) paste(x, collapse = ','))
# table2$central_factor_btw_5 <- sapply(table2$central_factor_btw_5, function(x) paste(x, collapse = ','))
# 
# #write.csv(table2, file = "linked data subsystems and 5 central factors.csv")
# names(table2)
# 
# ####New long format
# factors <-  dplyr::select(table2,
#                    c("Subsystem","Number of factors",
#                      "central_factor_1",     
#                      "central_factor_2",  
#                      "central_factor_3",
#                      "central_factor_4",   
#                      "central_factor_5"))
# degs <-  dplyr::select(table2,
#                    c("Subsystem","Number of factors",
#                      "central_factor_deg_1", 
#                      "central_factor_deg_2", 
#                      "central_factor_deg_3",
#                      "central_factor_deg_4",
#                      "central_factor_deg_5")) 
# btws <-  dplyr::select(table2,
#                    c("Subsystem","Number of factors",
#                      "central_factor_btw_1",
#                      "central_factor_btw_2",
#                      "central_factor_btw_3",   
#                      "central_factor_btw_4",     
#                      "central_factor_btw_5"))
# 
# factors <- reshape2::melt(factors, id.vars = c("Subsystem", "Number of factors"), value.name = "factor" )
# degs    <- reshape2::melt(degs   , id.vars = c("Subsystem", "Number of factors"), value.name = "degree" )
# btws    <- reshape2::melt(btws   , id.vars = c("Subsystem", "Number of factors"), value.name = "betweenness" )
# 
# factors$variable <- as.numeric(factors$variable)
# degs$variable    <- as.numeric(degs$variable)
# btws$variable    <- as.numeric(btws$variable)
# 
# tt <- dplyr::full_join(factors, degs)
# tt <- dplyr::full_join(tt, btws)
# table2 <- tt[order(tt$`Number of factors` , tt$Subsystem , decreasing = T),]
# 
# 
# 
# 
# write.csv(table2, 
#           file = paste0("system map subsystems and 5 factors resolution ",res,".csv"))
# 
# 
# 
# 
# clust[[counter]]$resolution   <- res
# comm_df[[counter]]$resolution <- res
# 
# counter <- counter + 1
# } # End of resolution loop 
# 


###This idea was revealed to me in a dream 
# on the night of 26th January - 27 January 2024 

# 1. Create a unique nodeID for each community and resolution 
# 2. Count how many variables are shared between first resolution and second resolution
# 3. Repeat for nth and n+1th resolutions
# 4. Create edges between each resolution node, with weight as the common variable count
# 5. Edges only appear between adjacent resolution layers


# 1. Create a unique nodeID for each community and resolution 
# 
# ##Count total number of nodes
# node_count <- 0
# node_names <- ""
# 
# for (res in 1:9) {
#   # count communities in each layer
#   print(paste0("Number of nodes in resolution layer ", res, " :", dim(table(
#     comm_df[[res]]$community))))
#   node_count <- node_count + dim(table(comm_df[[res]]$community))
#   ##create node names
#   node_names <-
#     c(node_names, paste0(res, "-", names(table(
#       comm_df[[res]]$community))))
# }
# 
# node_names <- node_names[node_names !=""]                                                                    
# layer_graph <-  igraph::graph(edges = character(0), isolates = node_names)
# V(layer_graph)$size <- NA
# 
# length(V(layer_graph))
# length(node_names)
# 
# V(layer_graph)$name[(is.na(V(layer_graph)$size))]
# 
# V(layer_graph)$size[V(layer_graph)$name =="1-1" ]
# 
# # 2. Count how many variables are shared between first resolution and second resolution
# 
# #Look at number of communities
# 
# for (res in 1:8){
# for (comm_num in as.numeric(names(table(comm_df[[res]]$community)))) {
# res2 <- res + 1
# for (comm_num2 in as.numeric(names(table(comm_df[[res2]]$community)))) {
# #Look at vars in first community
# sendvars <- comm_df[[res]]$variable[comm_df[[res]]$community==comm_num]
# #Look at vars in next layer community
# recvars <- comm_df[[res2]]$variable[comm_df[[res2]]$community==comm_num2]
# 
# if (length(intersect(sendvars, recvars)) > 0)  layer_graph <- add_edges(layer_graph, c(
#                          noquote(paste0(res,"-",comm_num)) ,
#                          noquote(paste0(res2,"-",comm_num2))),
#                         weight = length(intersect(sendvars, recvars)))
#  
# V(layer_graph)$size[V(layer_graph)$name == paste0(res,"-",comm_num)]   <- length(sendvars)
# V(layer_graph)$size[V(layer_graph)$name == paste0(res2,"-",comm_num2)]   <- length(recvars)
# 
# }
# }
# }
# 
# 
# 
# 
# min <- 1
# max <- 3
# rescaled <- scale(V(layer_graph)$size, center = F, scale = T )
# fivenum(rescaled)
# # plot(layer_graph)
# # plot(layer_graph, layout = layout_in_circle(layer_graph))
# # plot(layer_graph, layout =  layout_as_tree(layer_graph))
# isolates <- which(degree(layer_graph) == 0)
# 
# # Delete isolates from the layer graph
# layer_graph <- delete.vertices(layer_graph, isolates)
# 
# max_weight <- max(E(layer_graph)$weight)
# # Normalize the weights to range from 0 to 1
# normalized_weights <- (E(layer_graph)$weight / max_weight)
# inv_weights <- 1 - (E(layer_graph)$weight / max_weight)
# 
# layers <- rep(NA, length(V(layer_graph)$name))
# V(layer_graph)$layer <- NA
# V(layer_graph)$layer[grep("^1-", V(layer_graph)$name)] <- 1
# V(layer_graph)$layer[grep("^2-", V(layer_graph)$name)] <- 2
# V(layer_graph)$layer[grep("^3-", V(layer_graph)$name)] <- 3
# V(layer_graph)$layer[grep("^4-", V(layer_graph)$name)] <- 4
# V(layer_graph)$layer[grep("^5-", V(layer_graph)$name)] <- 5
# V(layer_graph)$layer[grep("^6-", V(layer_graph)$name)] <- 6
# V(layer_graph)$layer[grep("^7-", V(layer_graph)$name)] <- 7
# V(layer_graph)$layer[grep("^8-", V(layer_graph)$name)] <- 8
# V(layer_graph)$layer[grep("^9-", V(layer_graph)$name)] <- 9
# 
# 
# 
# scaled_weight <- (E(layer_graph)$weight - min(E(layer_graph)$weight)) / (max(E(layer_graph)$weight) - min(E(layer_graph)$weight))
# e_color <- rgb(0, (1 - scaled_weight) , 0.5)
# 
# # Get edge colors based on edge weights
# pdf("Layer plot auto position.pdf")
# plot(layer_graph, 
#      # layout = layout_as_tree(layer_graph ,
#      #                         root = V(layer_graph)$name[grepl("^1-", V(layer_graph)$name)],
#      #                         circular = F,
#      #                         mode = "all"),
#      layout = layout_with_sugiyama(layer_graph,
#                                    layers = V(layer_graph)$layer,
# # takes a few minutes                                   maxiter = 919500,
#                                    maxiter = 100500,
#                                    hgap = 610),
# 
#      #     layout = layout_with_fr(layer_graph),
#      edge.width = normalized_weights * 5 ,  # Set edge width based on weight
# #     edge.alpha = inv_weights * 0.5 ,
#      edge.label = NA,                     # Remove edge labels
# #     edge.color = "darkgray",                     # Remove edge labels
#      edge.color = e_color,                     # Remove edge labels
#      vertex.label = NA,        # Set vertex label color
#      vertex.label.color = "black",        # Set vertex label color
#      vertex.size = rescaled,                    # Set vertex size
#      vertex.frame.width = 0,                    # Set vertex size
#      edge.arrow.size = 0,
#      main = "Layered Graph without Labels and Edge Widths Based on Weight")
# dev.off()
# ###Seems to show there is one regularly detected community with the same vars, 
# ##  this appears at all resolutions. 
# 
# #  There are 2 or three similar veins of similarity. 
# 
# ##Weighting of lines is skewed because larger number of vars in the higher levels
# 
# 
# ###Try manually separating nodes 
# 
# lay_coords <- layout_with_sugiyama(layer_graph,layers = V(layer_graph)$layer)
# 
# for (layers in 1:9){
# #How many nodes in layer
# num_nodes <- length(lay_coords[lay_coords$layout[,2] == layers]) 
# #Spacing between each node.
# spacing <- 210 / num_nodes
# lay_coords$layout[which(lay_coords$layout[,2] == layers),][,1] <- seq(from = spacing , by = spacing, to = spacing * num_nodes)
# }
# 
# pdf("Layer plot manual position.pdf")
# plot(layer_graph, 
#      layout = lay_coords,
#      edge.width = normalized_weights * 5 ,  # Set edge width based on weight
# #     edge.alpha = inv_weights * 0.5 ,
# #     edge.color = "darkgray",                     # Remove edge labels
#      edge.color = e_color,                     # Remove edge labels
#      # vertex.label = NA,        # Set vertex label color
#      vertex.label.cex = 0.2,        
#      vertex.label.color = "black",        # Set vertex label color
#      vertex.size = rescaled,                    # Set vertex size
#      vertex.frame.width = 0,                    # Set vertex size
#      edge.arrow.size = 0,
#      main = "Layered Graph without Labels and Edge Widths Based on Weight")
# dev.off()
# 
# 
# 
