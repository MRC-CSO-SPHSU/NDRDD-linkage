
################################
#                              #
#  Create supernode plots      #
#  Where each node relates     # 
#  to a community of the       #
#  original network            #
################################

# SL developed this script
# April 2024

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

wd <- "T:/projects/CSO_DRD_S00359/Data"

setwd(wd)
getwd()
packageVersion("igraph")
# This analysis was run using igraph 2.0.3
# older versions give different results
# update.packages(oldPkgs = "igraph")

load("zero_g.RData")


##########################
# Community detection
#########################

# Define resolution? - not here
swg <- zero_g


V(swg)$degree <- degree(swg)
sum(V(swg)$degree==0) # 0

set.seed(12331) 
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


length(unique(CL$membership)) 
# assign mebership as node attribute
V(swg)$member <- CL$membership
table(CL$membership)

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


# supernodes - this is for simplified version!!! - no weights to ties bw supernodes

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

sdf <- as.data.frame(cbind(V(swg)$name, V(swg)$member2))
colnames(sdf) <- c("node_names", "supernode")

cbind(V(g2)$name, V(g2)$true_name, V(g2)$size)
sdf$supernode <- as.numeric(sdf$supernode)
newdata <- sdf[order(sdf$supernode),]
head(newdata, 10)
str(newdata)
write.xlsx(newdata, "NodesAndSupernodes.xlsx")


sdf2 <- as.data.frame(cbind(V(g2)$true_name, V(g2)$size))
colnames(sdf2) <- c("supernode_code", "N_of_nodes")
head(sdf2)

V(g2)$degree <- degree(g2)
sum(V(g2)$degree==0) 

# adding weights
#  df   node - comm
colnames(newdata)
Node_names <- newdata$node_names
Supernode_ <- newdata$supernode

matsize <- length(unique(Supernode_))

#  el:  From_Node - To_Node
el = igraph::as_data_frame(swg)

# map supernode from - to columns
el$from_Com <- plyr::mapvalues(el$from, from = Node_names, to = Supernode_)
el$to_Com <- plyr::mapvalues(el$to, from = Node_names, to = Supernode_)
el$to_Com <- as.numeric(el$to_Com)
el$from_Com <- as.numeric(el$from_Com)
colnames(el)
# make an edge list of communities
elc <- subset(el, select = c( from_Com, to_Com))

# via a loop - it takes around 5 minutes
the_first_mat <- matrix(NA, nrow = matsize, ncol = matsize)
for (i in 1:nrow(the_first_mat)) {
  for (j in 1:ncol(the_first_mat)) {
    num_tie <-0
    print(c(i,j))
    for(z in 1:nrow(elc)){
      if ((elc$from_Com[z] ==i & elc$to_Com[z] ==j)|(elc$from_Com[z] ==j & elc$to_Com[z] ==i)){
        num_tie <- num_tie +1
      }
    }
     
    the_first_mat[i,j] <- num_tie

  }}
#mat <- as.matrix(table(el$from_Com, el$to_Com)) # includes isolates too
mat <- the_first_mat


Snodes <-unique(Supernode_) # that is the order of nodes in the matrix
rownames(mat) <- Snodes
colnames(mat) <- Snodes
mat_simple <- mat
diag(mat_simple)
diag(mat) <- 0

community_sizes <- sapply(1:length(Snodes), function(i) {
  length(which(V(swg)$member2 == i))
})


sdf2 <- as.data.frame(cbind(Snodes, community_sizes, diag(mat_simple)))
colnames(sdf2) <- c("supernode_code", "N_of_nodes", "Nties_in_com")

head(sdf2, 20)

rownames(mat_simple)

#density_com <- cbind(diag(mat), community_sizes)



mat_2 <- matrix(NA, nrow = matsize, ncol = matsize)
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


# POSSIBLE SENSITIVITY STRATEGIES
# use N of ties in communities to normalize?
# and calculate ratio of outgoing to ingoing ties for each node
# that can be further used for weighting tie to each community
# NOTE: we can also use true weights of those node-node ties?

weighted_supernodes_graph <- graph_from_adjacency_matrix(mat_2, weighted = T, 
                                                         mode = "undirected",
                                                         diag = F)

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
     edge.width	= E(weighted_supernodes_graph)$weight*50, 
     layout = layout_with_fr(weighted_supernodes_graph),  # Use a layout algorithm (e.g., Fruchterman-Reingold)
     vertex.label = V(weighted_supernodes_graph)$name,  # Label supernodes with community ID
     #edge.label = E(weighted_supernodes_graph)$weight,  # Label edges with weights
     
     main = "Weighted Supernodes Graph based on Communities"  # Add a title
)

# adding proper supernode names

pr_names<- utils::read.csv("Labelled Community strength sorted network resolution 1.csv")
table(pr_names$community)
colnames(pr_names)
colnames(newdata)
pr_names$supernode <- NULL
newdata$variable <- newdata$node_names

head(pr_names)
head(newdata)
dim(pr_names)
dim(newdata)

pr_df <- merge(pr_names, newdata, by = "variable", all = T)
dim(pr_df)
head(pr_df)
table(pr_df$Label)

pr_df <- pr_df[pr_df$Label !="",]
head(pr_df)
pr_df <- dplyr::select(pr_df, c(community, Label, supernode))
cbind(pr_df$community, pr_df$supernode)
pr_df$community == pr_df$supernode # the same


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
# colnames(pr_df)
# 
# c_num <- unique(pr_df$community)
# Labels <- unique(pr_df$Label)[-1]
# pr_df$SUPERNODE_name <- plyr::mapvalues(pr_df$community, 
#                                         from = c_num,
#                                         to = Labels)
# write.xlsx(pr_df, "sanity_check.xlsx")

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

# Create a graph without isolates
Isolated = which(degree(weighted_supernodes_graph)==0)
length(Isolated)
WSG = delete_vertices(weighted_supernodes_graph, Isolated)


sdf2$Label <- try_new
head(sdf2)

#write.xlsx(sdf2, "Some data about supernodes_communities.xlsx")
# 
#save(weighted_supernodes_graph, file = "supernodes_network.RData")
#save(WSG, file = "supernodes_network_without_iso.RData")


# simple plotting
set.seed(321007)
V(weighted_supernodes_graph)$circle.order <- rank(degree(weighted_supernodes_graph), ties.method = "random")

groups <- cluster_louvain(weighted_supernodes_graph, resolution = 1)

#lay <- layout_in_circle(weighted_supernodes_graph, order = V(weighted_supernodes_graph)$circle.order)
lay <- layout_in_circle(weighted_supernodes_graph, order = order(membership(groups)))
color_gradient <- colorRampPalette(c("#ffffcc", "#FF7593"))
# Normalize node sizes for color mapping
node_sizes <- (V(weighted_supernodes_graph)$size / 18) + 5
normalized_sizes <- (node_sizes - min(node_sizes)) / (max(node_sizes) - min(node_sizes))
# Generate colors based on normalized sizes
vertex_colors <- color_gradient(100)[as.numeric(cut(normalized_sizes, breaks = 100))]

png(filename="Connected subsystems all.png",
    #width = 10, height = 14)
    width = 1000, height = 1000, units = "px")
# Use the Fruchterman-Reingold layout with additional repulsion
#lay <- layout_with_fr(g2, niter = 500, grid = "nogrid")
set.seed(492555)
#lay <- layout_nicely(weighted_supernodes_graph)
plot(weighted_supernodes_graph, 
     vertex.frame.color = "white",
     vertex.size=(V(weighted_supernodes_graph)$size/18)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     vertex.label=V(weighted_supernodes_graph)$Full_name , 
     #edge.arrow.size = 0.2,
     #vertex.frame.color = "none",
     vertex.label.font = 1,
     vertex.label.cex = 0.8,
     vertex.color = vertex_colors,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width	= E(weighted_supernodes_graph)$weight*50, 
     #vertex.color = "darkred",
     layout = lay,
     main = "",
     #rescale = TRUE,  # Adjusts plot to the entire layout area
     #xlim = c(-1, 1),  # Adjust x-axis limits to prevent truncation
     #ylim = c(-1, 1)  # Adjust y-axis limits to prevent truncation
)
dev.off()


#set.seed(392505)

##Wrap text

V(WSG)$Full_name <- c(
  "Mixed NDRDD \n variables",
  "Mental health \n inpatient",                 
  "Alcohol detox",
  "Benzos \n  and pain",                                   
  "Phlebitis, hepatitis, \n mental health, \n substance use",
  "Diabetes",                                          
  "Epilepsy",
  "Liver cirrhosis, \n malnourishment",                   
  "Alcohol \n gastritis","Allergy, \n asthma",                 
   "Respiratory, \n depression, \n pain",
  "Self poisoning",         
   "Assault and \n self harm",
  "Cardiac arrest",                                    
   "Pregabalin, \n Duloxetine",
  "Stomach, skin, \n antibiotics",                  
   "Respiratory \n disease",
  "Morphine, \n constipation",     
   "Methadone \n common prescriptions",
  "Skin cream",                                        
   "Heart disease",
  "Alcohol, fungal \n  stomach", 
   "Skin, \nstomach, \n sinuses",
  "Suboxone" 
)



set.seed(321007)
V(WSG)$circle.order <- rank(degree(WSG), ties.method = "random")

groups <- cluster_louvain(WSG, resolution = 1)

#lay <- layout_in_circle(WSG, order = V(WSG)$circle.order)
lay <- layout_in_circle(WSG, order = order(membership(groups)))
color_gradient <- colorRampPalette(c("#ffffcc", "#FF7593"))
# Normalize node sizes for color mapping
node_sizes <- (V(WSG)$size / 18) + 5
normalized_sizes <- (node_sizes - min(node_sizes)) / (max(node_sizes) - min(node_sizes))
# Generate colors based on normalized sizes
vertex_colors <- color_gradient(100)[as.numeric(cut(normalized_sizes, breaks = 100))]

png(filename="Figure 2 connected subsystems isolates removed.png",
    width = 2000, height = 2000, units = "px")

# Plot the graph with the specified parameters
plot(WSG, 
     vertex.size = node_sizes + 5, 
     vertex.label.color = "black", 
     vertex.label.dist = 0.1,
     vertex.label = V(WSG)$Full_name, 
     vertex.frame.color = "white",
     vertex.label.font = 1,
     vertex.label.cex = 2,
     arrow.mode = "-",
     edge.color = "gray",
     edge.width = E(WSG)$weight * 80, 
     vertex.color = vertex_colors,
     layout = lay,
     main = "",
     rescale = TRUE
     #xlim = c(-0.5, 0.5),  # Adjust x-axis limits to prevent truncation (if needed)
     #ylim = c(-0.5, 0.5)   # Adjust y-axis limits to prevent truncation (if needed)
)
    
dev.off()

