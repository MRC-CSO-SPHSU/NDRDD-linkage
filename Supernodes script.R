
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
str(V(swg)$member2)
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


sdf <- as.data.frame(cbind(V(swg)$name, V(swg)$member2))
colnames(sdf) <- c("node_names", "supernode")
str(V(swg)$member2)

cbind(V(g2)$name, V(g2)$true_name, V(g2)$size)
str(V(swg)$member2)
sdf$supernode <- as.numeric(sdf$supernode)
newdata <- sdf[order(sdf$supernode),]
head(newdata, 10)
str(newdata)
write.xlsx(newdata, "NodesAndSupernodes.xlsx")


sdf2 <- as.data.frame(cbind(V(g2)$true_name, V(g2)$size))
colnames(sdf2) <- c("supernode_code", "N_of_nodes")
head(sdf2)

gorder(g2)# 55
gsize(g2)# 162
edge_density(g2) # 0.11
V(g2)$degree <- degree(g2)
sum(V(g2)$degree==0) # 30 isolated communities


# adding weights

# adding weights - new way - THAT DOES NOT INCLUDE ISOLATES!!!!
#  df   node - comm
colnames(newdata)
Node_names <- newdata$node_names
Supernode_ <- newdata$supernode
unique(Supernode_)

#  el:  From_Node - To_Node
el = as_data_frame(swg)

# map is better I think
el$from_Com <- plyr::mapvalues(el$from, from = Node_names, to = Supernode_)
el$to_Com <- plyr::mapvalues(el$to, from = Node_names, to = Supernode_)
el$to_Com <- as.numeric(el$to_Com)
el$from_Com <- as.numeric(el$from_Com)


mat <- as.matrix(table(el$from_Com, el$to_Com)) # includes isolates too
rownames(mat)
colnames(mat)
mat_simple <- mat
diag(mat_simple)
diag(mat) <- 0

Snodes <-unique(Supernode_) # that is the order of nodes in the matrix


community_sizes <- sapply(1:length(Snodes), function(i) {
  length(which(V(swg)$member2 == i))
})
sum(community_sizes)

sdf2 <- as.data.frame(cbind(Snodes, community_sizes, diag(mat_simple)))
colnames(sdf2) <- c("supernode_code", "N_of_nodes", "Nties_in_com")
# something is wrong!!
head(sdf2, 10)
rownames(mat_simple)

#density_com <- cbind(diag(mat), community_sizes)



mat_2 <- matrix(NA, nrow = 55, ncol = 55)
for (i in 1:nrow(mat_2)) {
  for (j in 1:ncol(mat_2)) {
    if (i != j) {
      print(c(i,j))
      com_size_i <- community_sizes[[i]]
      com_size_j <- community_sizes[[j]]
      
      W <- round(((mat[i,j]/com_size_i) + (mat[i,j]/com_size_j))/2, 2)
      mat_2[i, j] <- W  # taking into account the size of communities
    }
  }}
rownames(mat) <- Snodes
colnames(mat) <- Snodes

rownames(mat_2) <- Snodes
colnames(mat_2) <- Snodes
str(Snodes)
# sanity checks
degree(g2, v= 2)
degree(g2, v = 50)
str(V(g2)$name)
max(mat_2, na.rm = T) # 0.13 now
# bc e.g. A community can have 2 nodes, they can have ties to more than 2 nodes in another community
# that is: com size is not necesseraly bigger than num_ties between communities
hist(mat_2)
hist(mat) # just sums of links

# POSSIBLE SENSITIVITY STRATEGIES
# use N of ties in communities to normalize?
# and calculate ratio of outgoing to ingoing ties for each node
# that can be further used for weighting tie to each community
# NOTE: we can also use true weights of those node-node ties?

weighted_supernodes_graph <- graph_from_adjacency_matrix(mat_2, weighted = T, 
                                                         mode = "undirected",
                                                         diag = F)
E(weighted_supernodes_graph)$weight
diag(mat_2)

# Plot the weighted supernodes graph
set.seed(45389)
plot(weighted_supernodes_graph, 
     vertex.size = 5,
     vertex.size = 5,  # Set node size
     #vertex.size=(V(g2)$size/18)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     #vertex.label=V(G2)$Full_name , 
     #edge.arrow.size = 0.2,
     #vertex.frame.color = "none",
     vertex.label.font = 1,
     vertex.label.cex = 0.8,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width	= E(weighted_supernodes_graph)$weight*10, 
     layout = layout_with_fr(weighted_supernodes_graph),  # Use a layout algorithm (e.g., Fruchterman-Reingold)
     vertex.label = V(weighted_supernodes_graph)$name,  # Label supernodes with community ID
     #edge.label = E(weighted_supernodes_graph)$weight,  # Label edges with weights
     
     main = "Weighted Supernodes Graph based on Communities"  # Add a title
)

# adding proper supernode names

pr_names<- utils::read.csv("Labelled Community strength sorted network resolution 1.csv")
table(pr_names$community)
head(pr_names)

pr_names <- pr_names[pr_names$Label !="",]
pr_names <- dplyr::select(pr_names, c(community, Label))
pr_names$supernode <- pr_names$community
head(pr_names)
dim(pr_names)

str(pr_names$community)
pr_names$community == pr_names$supernode

# pr_df <- merge(pr_names, newdata, by = "supernode", all = T)
# head(pr_df, 20)
# View(pr_df)
# 
# sum(is.na(pr_df$Label))
# str(pr_df$Label)
# pr_df$Label <- as.character(pr_df$Label)
# pr_df$Label <- ifelse(pr_df$Label == "", NA, pr_df$Label)
# sum(!is.na(pr_df$Label))
# table(pr_df$Label)
# prdf2 <- pr_df[!is.na(pr_df$Label), ]
# dim(prdf2)
# colnames(prdf2)
# Com_names <- prdf2$Label
# Supernode <- as.character(as.numeric(prdf2$supernode))
# V(g2)$name <- as.character(as.numeric(V(g2)$name))

# g2_names <- unlist(V(g2)$name)
# try<- plyr::mapvalues(g2_names, from = Supernode,
#                       to = Com_names)

# fill in the rows
colnames(pr_df)
length(unique(pr_df$community))# 55
c_num <- unique(pr_df$community)
Labels <- unique(pr_df$Label)[-1]
pr_df$SUPERNODE_name <- plyr::mapvalues(pr_df$community, 
                                        from = c_num,
                                        to = Labels)
write.xlsx(pr_df, "sanity_check.xlsx")

Com_names <- pr_names$Label
Com_codes <- pr_names$community

try<- plyr::mapvalues(V(weighted_supernodes_graph)$name, from = Com_codes,
                      to = Com_names)
try
try_new <- gsub("Isolated component: ", "", try)

V(weighted_supernodes_graph)$Full_name <-try_new

Com_codes <- sdf2$supernode_code
Com_size <- sdf2$N_of_nodes

try2 <- plyr::mapvalues(V(weighted_supernodes_graph)$name, from = Com_codes,
                        to = Com_size)
try2 <- as.numeric(try2)

V(weighted_supernodes_graph)$size <-try2
degree(weighted_supernodes_graph)

# not working??? 
Isolated = which(degree(weighted_supernodes_graph)==0)
length(Isolated)
WSG = delete_vertices(weighted_supernodes_graph, Isolated)
gorder(WSG) # 25
gsize(WSG) # 160
edge_density(WSG) # 0.53
# 
save(weighted_supernodes_graph, file = "supernodes_network.RData")
save(WSG, file = "supernodes_network_without_iso.RData")


# simple plotting

png(filename="Communities_zero_g_all.png",
    #width = 10, height = 14)
    width = 800, height = 800, units = "px")
# Use the Fruchterman-Reingold layout with additional repulsion
#lay <- layout_with_fr(g2, niter = 500, grid = "nogrid")
set.seed(492555)
lay <- layout_nicely(weighted_supernodes_graph)
plot(weighted_supernodes_graph, 
     vertex.frame.color = "white",
     vertex.size=(V(weighted_supernodes_graph)$size/18)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     vertex.label=V(weighted_supernodes_graph)$Full_name , 
     #edge.arrow.size = 0.2,
     #vertex.frame.color = "none",
     vertex.label.font = 1,
     vertex.label.cex = 0.8,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width	= E(weighted_supernodes_graph)$weight*10, 
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

#set.seed(392505)

set.seed(192300)
lay <- layout_with_fr(WSG)
plot(WSG, 
     vertex.size=(V(WSG)$size/18)+5, 
     vertex.label.color="black", vertex.label.dist=0.1,
     vertex.label=V(WSG)$Full_name , 
     vertex.frame.color = "white",
     vertex.label.font = 1,
     vertex.label.cex = 0.7,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width	= E(WSG)$weight*10, 
     #vertex.color = "darkred",
     layout = lay,
     main = "")
dev.off()
hist(E(WSG)$weight)

# OLD CODE

# # THIS CODE IS EXTREMLY SLOW!!!
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
time.taken # 11.23 hours
write.xlsx(weighted_edges1, "weighted_edges1_oldcode.xlsx")
write.xlsx(weighted_edges2, "weighted_edges2_oldcode.xlsx")
