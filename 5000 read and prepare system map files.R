# Preparation 

# This script reads in the yED co-produced map, creates and edgelist,
# detects communities, estimates degree and closeness, and creates a plot where
# communities are represented with colours.

# Clean work space
rm(list=ls())
gc(full=TRUE)

# Library
library("rmarkdown")
library("tinytex")
library("knitr")
library("comato")
library("stringi")
library("igraph")
library("ggraph")
library("ggplot2")
library("stringr")
library("network")
library("tidygraph")
library("psych")
library("openxlsx")

# Working directory as an object 
getwd()


wd_path <- (paste0("C:/___Rosie_local/My One Drive/",
                   "OneDrive - University of Glasgow/ndrdd/ndrdd_paper/co_produced_map"))

wd_path <- "C:/Users/mmc78h/Downloads"

setwd(wd_path)
dir()

#Load raw data from comato Read.Yed
raw_data <- comato::read.yEd("DRD_SystemMap (2).graphml") 
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

write.csv(node_names, file = paste0("CoProducedEdgelist.csv"))

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

cfg <- igraph::cluster_louvain(as.undirected(graph_raw_data))

plot1 <- plot(cfg, as.undirected(graph_raw_data))

V(graph_raw_data)$community <- cfg$membership
V(graph_raw_data)$closeness <- igraph::closeness(graph_raw_data, 
                                                 mode = "total")

V(graph_raw_data)$in_degree <- igraph::degree(graph_raw_data, mode = "in")
V(graph_raw_data)$out_degree <- igraph::degree(graph_raw_data, mode = "out")

colrs_vertex <- rainbow(10)
colr_edges <- "#808080"

E(graph_raw_data)$color <- "808080"

plot(graph_raw_data, 
     vertex.color = colrs_vertex[V(graph_raw_data)$community])

# try with ggraph 
set.seed(3000)
community_graph <- ggraph(graph_raw_data, 
                          layout = "fr") +
  geom_edge_fan(color = "grey80") +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(12, 'mm'),
                 color = "grey80") +
  geom_node_point(aes(size = V(graph_raw_data)$size), color = "white") +
  geom_node_point(aes(size = V(graph_raw_data)$size), 
                   color = colrs_vertex[V(graph_raw_data)$community], 
                   alpha = 0.5, shape = 20) +
  geom_node_text(aes(label = V(graph_raw_data)$name),
                 repel = TRUE, max.overlaps = Inf, 
                 color = "black", size = 2) +
  scale_size_continuous(range = c(2, 10)) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none")


community_graph

ggsave(paste0(wd_path,"/CoProducedCommunityDectection.pdf"),
              device = "pdf", width = 13.9, height = 10.3, units = "in")

ggsave(paste0(wd_path,"/CoProducedCommunityDectection.jpeg"),
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
          paste0(wd_path,"/TableNetworkSummaryMeasures.csv"), 
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
          paste0(wd_path,"/TableMaximumValues.csv"), 
          row.names = TRUE)


# select top 10 for degree
TableDegree <- community_data_frame %>%
  arrange(desc(degree)) 

TableDegree <- head(TableDegree, 20)

write.csv(TableDegree, 
          paste0(wd_path,"/TableTopDegree.csv"), 
          row.names = TRUE)

# Highest degree within each community 
TableWithinCommunty <- community_data_frame %>%
  group_by(community) %>%
  filter(degree == max(degree)) %>%
  ungroup()

write.csv(TableWithinCommunty, 
          paste0(wd_path,"/TableWithinCommunty.csv"), 
          row.names = TRUE)





                    
base::saveRDS(community_data_frame, paste0(wd_path,"/CoProducedCommunities.rds"))
write.csv(community_data_frame, paste0(wd_path,"/CoProducedCommunities.csv"))



# Code from Mark for plotting by indegree and outdegree

# raw_network <- network(as.matrix(node_names))
# 
# 
# network_layout_degree <- function(d, layout.par){
#   id <- jitter(degree(d, cmode = "indegree") ,3)
#   od <- jitter( degree(d, cmode = "outdegree") ,3)
#   cbind(id, od) }
# 
# 
# plot(raw_network[[3]], mode = "degree", displaylabels = TRUE,
#      boxed.labels = FALSE, suppress.axes = FALSE, label.cex = 1.2,  
#      vertex.col = 'SEM',
#      xlab = "Indegree", ylab = "Outdegree", label.col = 1, main = "Market")



# upstream: causing most things but caused by few things appear in the top left 
# downstream: caused by all the other factors but not causing anything else
# appear in the bottom right. 

# igraph object as tibble 
edges  <- igraph::as_data_frame(graph_raw_data, c("edges"))
vertices <- igraph::as_data_frame(graph_raw_data, c("vertices"))


# Define layout 

# If ggraph lacks the needed layout it is always possible to 
# supply your own layout function that takes a tbl_graph object 
# and returns a data.frame of node positions, or supply the positions 
# directly by passing a matrix or data.frame to the layout argument.


tbl_graph_raw <- as_tbl_graph(graph_raw_data)


# dataframe of x and y as in and out degree

co_ordinates <- community_data_frame %>%
  select(in_degree, out_degree) %>%
  rename(x = in_degree, y = out_degree)


pdf(file = "up_down_stream_plot.pdf")
set.seed(3000)
ggraph(tbl_graph_raw, 
       layout = co_ordinates ) +
  geom_edge_fan(color = "grey80") +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(12, 'mm'),
                 color = "grey80") +
  geom_node_point(aes(size = V(graph_raw_data)$size), color = "white") +
  geom_node_point(aes(size = V(graph_raw_data)$size), 
                  color = colrs_vertex[V(graph_raw_data)$community], 
                  alpha = 0.5, shape = 20) +
  geom_node_text(aes(label = V(graph_raw_data)$name),
                 repel = TRUE, max.overlaps = Inf, 
                 color = "black", size = 2) +
  scale_size_continuous(range = c(2, 10)) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none")


ggsave(paste0(wd_path,"/FigureCoProducedMap.pdf"),
         device = "pdf", width = 25, height = 15, units = "in")

ggsave(paste0(wd_path,"/up_down_stream_plot.jpeg"),
       device = "jpeg", width = 45, height = 25, units = "in")
# 
# 
# 
# 
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
# plotsave(paste0(wd_path,"/CoProducedCommunityDectection.pdf"),
#        device = "pdf", width = 13.9, height = 10.3, units = "in")
# 

