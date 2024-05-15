
#################
#               #
#  For DRD      #
#               #
#################

# SL developed this script
# Aril 2024

#############
#  Purpose  #
#############

# Finding communities
# Creating supernodes

#########################
#                       #
#    Load packages      #
#                       #
#########################
###### PUT PACKAGES HERE

library(data.table)
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2) 
library(plyr)
library(purrr)
library(igraph)




#########################
#                       #
#     Load functions    #
#                       #
#########################
# none

wd <- "T:/projects/CSO_DRD_S00359/Data"

setwd(wd)
getwd()

# check igraph package bc older versions give different results
# detach(package:igraph, unload = TRUE)
# packageVersion("igraph") # '1.2.6'
# update.packages(oldPkgs = "igraph")

load("zero_g.RData")


##########################
# Community detection
#########################

# Define resolution? - not here
swg <- zero_g


V(swg)$degree <- degree(swg)
sum(V(swg)$degree==0) # 0
gorder(swg) # 1229
edge_density(swg) # 0.002
gsize(swg) # 1950

is.weighted(swg)

set.seed(3111) # 54 - Need to check how different the communities are with different resolutions and seeds
set.seed(12331) # 55
CL <- igraph::cluster_louvain(swg) # add Res here if needed
CL
# the followig two steps are added to avoid some igraph vesrion related issues
# save(CL, file = "CL_1.RData") 
# 
# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# CL <- loadRData("CL_1.RData")


length(unique(CL$membership)) # 55
# assign mebership as node attribute
V(swg)$member <- CL$membership
table(CL$membership)

#plot(swg, vertex.color = V(swg)$member)

##########################
# Supernodes
#########################
alln <- V(swg)$name # nodes
coms <- unique(V(swg)$member) # communities numbers
comnn <- seq(1:length(coms)) # 

size <-c()
alln <- V(swg)$member
for(k in 1:length(coms)){
  co <-length(subset(alln, alln==coms[k]))
  size[k] <- co
}
V(swg)$member2<- plyr::mapvalues(V(swg)$member, 
                                         from = coms, 
                                         to = comnn)


# supernodes - this is for simplyfied version!!! - no weights to ties bw supernodes

g2 = contract(swg, V(swg)$member2)
g2 = simplify(g2)

# fix names

V(g2)$name # the structure that one gets from CDAs
# give communities their new names
V(g2)$supername <- comnn
# coms is new name for communities in coms order
# now change the name to supername
# NOTE: names are not properly fixed yet
V(g2)$name <- V(g2)$supername
V(g2)$true_name <- coms
V(g2)$size <- size

as_adjacency_matrix(g2)

supernodes_graph <- g2

sdf <- as.data.frame(cbind(V(swg)$name, V(swg)$member2))
colnames(sdf) <- c("node_names", "supernode")

newdata <- sdf[order(sdf$supernode),]
head(newdata, 10)
write.xlsx(newdata, "NodesAndSupernodes.xlsx")




gorder(g2)# 55
gsize(g2)# 162
edge_density(g2) # 0.11
V(g2)$degree <- degree(g2)
sum(V(g2)$degree==0) # 30 isolated communities


# adding weights
# THIS CODE IS EXTREMLY SLOW!!!
# Assuming 'swg' is your original graph and ''member2 is a vector indicating the community membership of each node
# 'supernodes_graph' is the graph where each supernode represents a community and ties exist between communities if there were any ties between nodes of each pair of communities

# Calculate the percentage of nodes in each community connected to nodes in other communities

# Precompute community sizes
community_sizes <- sapply(1:vcount(supernodes_graph), function(i) {
  length(which(V(swg)$member2 == i))
})
# Precompute node names in each community
nodes_in_community <- list()
for(i in 1:vcount(supernodes_graph)){
  nodes_in_community_i <- V(swg)[which(V(swg)$member2 == i)]
  net_i <- induced_subgraph(swg, nodes_in_community_i)
  nodes_in_community[[i]] <- V(net_i)$name
}

# the loop starts here::
start.time <- Sys.time()
# simple sum of edges
weighted_edges1 <- matrix(NA, nrow = vcount(supernodes_graph), ncol = vcount(supernodes_graph))
# taking into acount the size of node
weighted_edges2 <- matrix(NA, nrow = vcount(supernodes_graph), ncol = vcount(supernodes_graph))
for (i in 2:nrow(weighted_edges1)) {
  for (j in 1:ncol(weighted_edges1)) {
    if (i != j) {
      nodes_in_c_i <- nodes_in_community[[i]]
      com_size_i <- community_sizes[[i]]
      print(c(i,j))
      nodes_in_c_j <- nodes_in_community[[j]]
      com_size_j <- community_sizes[[j]]
      
      # nodes_in_community_i <- V(swg)[which(V(swg)$member2 == i)]
      # com_i_size <- length(V(swg)$name[nodes_in_community_i])
      # net_i <- induced_subgraph(swg, nodes_in_community_i)
      # nodes_in_community_i <- V(net_i)$name
      # 
      # nodes_in_community_j <- V(swg)[which(V(swg)$member2 == j)]
      # com_j_size <- length(V(swg)$name[nodes_in_community_j])
      # net_j <- induced_subgraph(swg, nodes_in_community_j)#
      # nodes_in_community_j <- V(net_j)$name
      # Initialize a counter for the number of ties
      num_ties <- 0
      for (z in 1:length(E(swg))) {
        edge <- E(swg)[z]
        node_names <- ends(swg, edge)
        node_names_vector <- as.character(node_names)
        if ((node_names_vector[1] %in% nodes_in_c_i & node_names_vector[2] %in% nodes_in_c_j)|
            (node_names_vector[2] %in% nodes_in_c_i & node_names_vector[1] %in% nodes_in_c_j)){
         
          num_ties <- num_ties + 1
        }
      }
     W <- round(((num_ties/com_size_i) + (num_ties/com_size_j))/2, 2)
      weighted_edges1[i, j] <- num_ties # simple sum
      weighted_edges2[i, j] <- W # taking into account the size of communities
  }
  }}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


# some tries/estimations
for (i in 1:vcount(supernodes_graph)) {
  for (j in 1:(i - 1)) {
    if (i != j) {
      print(c(i,j))}
  }}

  

# estimating how long it will take
nodes_in_community_i <- V(swg)[which(V(swg)$member2 == 1)]
com_i_size <- length(V(swg)$name[nodes_in_community_i])
net_i <- induced_subgraph(swg, nodes_in_community_i)
nodes_in_community_i <- V(net_i)$name

nodes_in_community_j <- V(swg)[which(V(swg)$member2 == 3)]
com_j_size <- length(V(swg)$name[nodes_in_community_j])
net_j <- induced_subgraph(swg, nodes_in_community_j)#
nodes_in_community_j <- V(net_j)$name
class(nodes_in_community_j)
class(nodes_in_community[[1]])
# Initialize a counter for the number of ties
start.time <- Sys.time()
num_ties <- 0
for (z in 1:length(E(swg))) {
  edge <- E(swg)[z]
  node_names <- ends(swg, edge)
  node_names_vector <- as.character(node_names)
  if ((node_names_vector[1] %in% nodes_in_community_i & node_names_vector[2] %in% nodes_in_community_j)|
      (node_names_vector[2] %in% nodes_in_community_i & node_names_vector[1] %in% nodes_in_community_j)){
    
    num_ties <- num_ties + 1
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken # 9 seconds per pair

toy <- matrix(NA, nrow = vcount(supernodes_graph), ncol = vcount(supernodes_graph))
for (i in 1:vcount(supernodes_graph)) {
  for (j in 1:(i - 1)) {
    if (i != j) {
      toy[i, j] <- 1}
  }
  }
dim(toy)
sum(toy, na.rm =T)# 1485
1485*9
13365/60 = 222.75 #minutes
222.75/60 = 3.7 #hours

# two
# estimating how long it will take
nodes_in_c_i <- nodes_in_community[[1]]
com_size_i <- community_sizes[[1]]
nodes_in_c_j <- nodes_in_community[[3]]
com_size_j <- community_sizes[[3]]


# Initialize a counter for the number of ties
start.time <- Sys.time()
num_ties <- 0
for (z in 1:length(E(swg))) {
  edge <- E(swg)[z]
  node_names <- ends(swg, edge)
  node_names_vector <- as.character(node_names)
  if ((node_names_vector[1] %in% nodes_in_c_i & node_names_vector[2] %in% nodes_in_c_j)|
      (node_names_vector[2] %in% nodes_in_c_i & node_names_vector[1] %in% nodes_in_c_j)){
    
    num_ties <- num_ties + 1
  }
}
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken # 9 seconds per pair

### ends here


# Create a weighted supernodes graph
weighted_supernodes_graph <- graph_from_adjacency_matrix(weighted_edges, mode = "undirected", weighted = TRUE)

# Plot the weighted supernodes graph
plot(weighted_supernodes_graph, 
     layout = layout_with_fr(weighted_supernodes_graph),  # Use a layout algorithm (e.g., Fruchterman-Reingold)
     vertex.size = 5,  # Set node size
     vertex.label = V(weighted_supernodes_graph)$name,  # Label supernodes with community ID
     edge.label = E(weighted_supernodes_graph)$weight,  # Label edges with weights

     main = "Weighted Supernodes Graph based on Community Connections 1"  # Add a title
)

# adding proper supernode names

pr_names<- utils::read.csv("Labelled Community strength sorted network resolution 1.csv")
table(pr_names$community)
head(pr_names)

pr_names <- pr_names[pr_names$Label !="",]
pr_names <- dplyr::select(pr_names, c(community, Label))
pr_names$supernode <- pr_names$community
head(newdata)

pr_df <- merge(pr_names, newdata, by = "supernode", all = T)
head(pr_df, 20)
View(pr_df)

sum(is.na(pr_df$Label))
str(pr_df$Label)
pr_df$Label <- as.character(pr_df$Label)
pr_df$Label <- ifelse(pr_df$Label == "", NA, pr_df$Label)
sum(!is.na(pr_df$Label))
table(pr_df$Label)
prdf2 <- pr_df[!is.na(pr_df$Label), ]
dim(prdf2)
colnames(prdf2)
Com_names <- prdf2$Label
Supernode <- as.character(as.numeric(prdf2$supernode))
V(g2)$name <- as.character(as.numeric(V(g2)$name))

g2_names <- unlist(V(g2)$name)
try<- plyr::mapvalues(g2_names, from = Supernode,
                                           to = Com_names)

# fill in the rows
colnames(pr_df)
length(unique(pr_df$community))# 55
c_num <- unique(pr_df$community)
Labels <- unique(pr_df$Label)[-1]
pr_df$SUPERNODE_name <- plyr::mapvalues(pr_df$community, 
                                        from = c_num,
                                        to = Labels)
write.xlsx(pr_df, "sanity_check.xlsx")

V(g2)$Full_name <-try # maybe this causes error??


Isolated = which(degree(g2)==0)
G2 = delete.vertices(g2, Isolated)
gorder(G2) # 25
gsize(G2) # 162
edge_density(G2) # 0.54

save(g2, file = "supernodes_network.RData")
save(G2, file = "supernodes_network_without_iso.RData")




# simple plotting

png(filename="Communities_zero_g_all.png",
    #width = 10, height = 14)
    width = 800, height = 800, units = "px")
# Use the Fruchterman-Reingold layout with additional repulsion
#lay <- layout_with_fr(g2, niter = 500, grid = "nogrid")
set.seed(492555)
lay <- layout_nicely(g2)
plot(g2, 
     vertex.size=(V(g2)$size/18)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     vertex.label=V(G2)$Full_name , 
     #edge.arrow.size = 0.2,
     #vertex.frame.color = "none",
     vertex.label.font = 1,
     vertex.label.cex = 0.8,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width	= 1.3, 
     #vertex.color = "darkred",
     layout = lay,
     main = "",
     #rescale = TRUE,  # Adjusts plot to the entire layout area
     #xlim = c(-1, 1),  # Adjust x-axis limits to prevent truncation
     #ylim = c(-1, 1)  # Adjust y-axis limits to prevent truncation
)
dev.off()


png(filename="Communities_zero_g_non_isolates.png",
    width = 800, height = 800, units = "px")

set.seed(492555)
lay <- layout_nicely(G2)
plot(G2, 
     vertex.size=(V(G2)$size/10)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     vertex.label=V(G2)$Full_name ,
     #edge.arrow.size = 0.2,
     #vertex.frame.color = "none",
     vertex.label.font = 1,
     vertex.label.cex = 0.8,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width	= 1.3, 
     #vertex.color = "darkred",
     layout = lay,
     main = "",
     
)
dev.off()

# as_adjacency_matrix(G2)