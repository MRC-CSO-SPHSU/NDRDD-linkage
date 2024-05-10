
#set working directory
setwd() # - put your path here

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

set.seed(492278)
CL <- cluster_louvain(swg) # add Res here if needed
length(unique(CL$membership)) # 51
# assign mebership as node attribute
V(swg)$member <- CL$membership

plot(swg, vertex.color = V(swg)$member)

##########################
# Supernodes
#########################
alln <- V(swg)$name
coms <- unique(V(swg)$member)
comnn <- seq(1:length(coms))
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

sdf <- as.data.frame(cbind(V(swg)$name, V(swg)$member2))
colnames(sdf) <- c("node_names", "supernode")

save(g2, file = "supernodes_network.RData")
newdata <- sdf[order(sdf$supernode),]
head(newdata, 10)
write.xlsx(newdata, "NodesAndSupernodes.xlsx")


# simple plotting

png(filename="Communities_zero_g_all.png",
    #width = 10, height = 14)
    width = 500, height = 500, units = "px")
# Use the Fruchterman-Reingold layout with additional repulsion
#lay <- layout_with_fr(g2, niter = 500, grid = "nogrid")
set.seed(492555)
lay <- layout_nicely(g2)
plot(g2, 
     vertex.size=(V(g2)$size/18)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     #vertex.label=NA, 
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




gorder(g2)# 51
gsize(g2)# 127
edge_density(g2) # 0.10
V(g2)$degree <- degree(g2)
sum(V(g2)$degree==0) # 30 isolated communities


Isolated = which(degree(g2)==0)
G2 = delete.vertices(g2, Isolated)
E(G2$weight)
gorder(G2) # 21
gsize(G2) # 127
edge_density(G2) # 0.6

save(G2, file = "supernodes_network_without_iso.RData")

png(filename="Communities_zero_g_non_isolates.png",
    width = 500, height = 500, units = "px")

set.seed(492555)
lay <- layout_nicely(G2)
plot(G2, 
     vertex.size=(V(G2)$size/10)+5, 
     vertex.label.color="black", vertex.label.dist=0.5,
     #vertex.label=NA, 
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

as_adjacency_matrix(G2)
