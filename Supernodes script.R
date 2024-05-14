
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
save(CL, file = "CL_1.RData") 

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
CL <- loadRData("CL_1.RData")


length(unique(CL$membership)) # 55
# assign mebership as node attribute
V(swg)$member <- CL$membership
table(CL$membership)

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

newdata <- sdf[order(sdf$supernode),]
head(newdata, 10)
write.xlsx(newdata, "NodesAndSupernodes.xlsx")





gorder(g2)# 55
gsize(g2)# 162
edge_density(g2) # 0.11
V(g2)$degree <- degree(g2)
sum(V(g2)$degree==0) # 30 isolated communities




# adding proper supernode names

pr_names<- utils::read.csv("Labelled Community strength sorted network resolution 1.csv")

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