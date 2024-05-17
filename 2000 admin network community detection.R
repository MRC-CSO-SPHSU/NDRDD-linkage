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
library(openxlsx)
library(readxl)

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

wd <- "T:/projects/CSO_DRD_S00359/Data"


if (Sys.info()[4] == "DESKTOP-2CKTEEO") wd <- "C:/Users/mmc78h/OneDrive - University of Glasgow/DRD/GGMnonreg"
setwd(wd)

edge_list <- readxl::read_excel("full_link zeroed below 0.xlsx")
edge_list <- read.xlsx("full_link zeroed below 0.xlsx")
head(edge_list)
str(edge_list)

# edge_list <- readxl::read_excel("full_link zeroed below 0.05.xlsx")
# setwd(paste0(wd,"/Below 5"))
# 
# edge_list <- readxl::read_excel("full_link zeroed below 0.08.xlsx")
# setwd(paste0(wd,"/Below 8"))


hist(as.numeric(edge_list$edge_weights))
table(as.numeric(edge_list$edge_weights))


zero_g <-  graph_from_data_frame(edge_list, directed = F)
save(zero_g, file = "zero_g.RData")
# create R object


clust   <- list()
comm_df <- list()
#loop through increasing resolution parameters for the louvain
# algorithm. Higher resolution = more communities
counter <- 1
for (res in seq(from = 1, to = 5, by = 0.5)) {
  set.seed(12331) # 55
  clust[[counter]] <- igraph::cluster_louvain(zero_g, resolution = res)
  V(zero_g)$community <- clust[[counter]]$membership
  print(table(clust[[counter]]$membership))
  V(zero_g)$strength <- strength(zero_g)
  size <- scale(V(zero_g)$strength, center = FALSE)
  pdf(paste0("Community circle layout resolution ", res, ".pdf"))
  plot(
    zero_g,
#    vertex.label = NA ,
   vertex.label.cex = 0.1,
#    layout = layout_with_drl(zero_g),
    layout = layout_in_circle(zero_g, order = V(zero_g)[order(V(zero_g)$community,V(zero_g)$strength )]),
    vertex.frame.width = 0,                    
    vertex.size =  size * 3,
    vertex.color = membership(clust[[counter]]),
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
      vertex.frame.width = 0,                    
   vertex.color = membership(clust[[counter]]),
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
comm_df[[counter]] <- community_data_frame %>%
  arrange(desc(community), desc(strength)) 

 write.csv(comm_df[[counter]], 
           paste0("Community strength sorted network resolution ",res,".csv"), 
           row.names = F)

#Manuscript Table 2: Descriptions of subsystems and most strongly connected factors in each subsystem,
# identified using the louvain algorithm in the co-occurence network for linked administrative data
#  relatind to *** drug deaths in Scotland **Date** to **Date 

##The linked data file is summary file from safe haven.
# Instead, create summary from final output cleared file above.
table.df <- comm_df[[counter]]

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

table2 <- as.data.frame((sort(table(comm_df[[counter]]$community), decreasing = T)))
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
          file = paste0("linked data subsystems and 5 factors resolution ",res,".csv"))




clust[[counter]]$resolution   <- res
comm_df[[counter]]$resolution <- res

counter <- counter + 1
} # End of resolution loop 


length(comm_df)
head(comm_df[[9]])


###This idea was revealed to me in a dream 
# on the night of 26th January - 27 January 2024 

# 1. Create a unique nodeID for each community and resolution 
# 2. Count how many variables are shared between first resolution and second resolution
# 3. Repeat for nth and n+1th resolutions
# 4. Create edges between each resolution node, with weight as the common variable count
# 5. Edges only appear between adjacent resolution layers


# 1. Create a unique nodeID for each community and resolution 

##Count total number of nodes
node_count <- 0
node_names <- ""

for (res in 1:9) {
  # count communities in each layer
  print(paste0("Number of nodes in resolution layer ", res, " :", dim(table(
    comm_df[[res]]$community))))
  node_count <- node_count + dim(table(comm_df[[res]]$community))
  ##create node names
  node_names <-
    c(node_names, paste0(res, "-", names(table(
      comm_df[[res]]$community))))
}

node_names <- node_names[node_names !=""]                                                                    
layer_graph <-  graph(edges = character(0), isolates = node_names)
V(layer_graph)$size <- NA

length(V(layer_graph))
length(node_names)

V(layer_graph)$name[(is.na(V(layer_graph)$size))]

V(layer_graph)$size[V(layer_graph)$name =="1-1" ]

# 2. Count how many variables are shared between first resolution and second resolution

#Look at number of communities

for (res in 1:8){
for (comm_num in as.numeric(names(table(comm_df[[res]]$community)))) {
res2 <- res + 1
for (comm_num2 in as.numeric(names(table(comm_df[[res2]]$community)))) {
#Look at vars in first community
sendvars <- comm_df[[res]]$variable[comm_df[[res]]$community==comm_num]
#Look at vars in next layer community
recvars <- comm_df[[res2]]$variable[comm_df[[res2]]$community==comm_num2]

if (length(intersect(sendvars, recvars)) > 0)  layer_graph <- add_edges(layer_graph, c(
                         noquote(paste0(res,"-",comm_num)) ,
                         noquote(paste0(res2,"-",comm_num2))),
                        weight = length(intersect(sendvars, recvars)))
 
V(layer_graph)$size[V(layer_graph)$name == paste0(res,"-",comm_num)]   <- length(sendvars)
V(layer_graph)$size[V(layer_graph)$name == paste0(res2,"-",comm_num2)]   <- length(recvars)

}
}
}
V(layer_graph)["9-1"]$size
#V(layer_graph)["9-85"]$size


min <- 1
max <- 3
rescaled <- scale(V(layer_graph)$size, center = F, scale = T )
fivenum(rescaled)
# plot(layer_graph)
# plot(layer_graph, layout = layout_in_circle(layer_graph))
# plot(layer_graph, layout =  layout_as_tree(layer_graph))
isolates <- which(degree(layer_graph) == 0)
# Delete isolates from the layer graph
layer_graph <- delete.vertices(layer_graph, isolates)

max_weight <- max(E(layer_graph)$weight)
# Normalize the weights to range from 0 to 1
normalized_weights <- (E(layer_graph)$weight / max_weight)
inv_weights <- 1 - (E(layer_graph)$weight / max_weight)

layers <- rep(NA, length(V(layer_graph)$name))
V(layer_graph)$layer <- NA
V(layer_graph)$layer[grep("^1-", V(layer_graph)$name)] <- 1
V(layer_graph)$layer[grep("^2-", V(layer_graph)$name)] <- 2
V(layer_graph)$layer[grep("^3-", V(layer_graph)$name)] <- 3
V(layer_graph)$layer[grep("^4-", V(layer_graph)$name)] <- 4
V(layer_graph)$layer[grep("^5-", V(layer_graph)$name)] <- 5
V(layer_graph)$layer[grep("^6-", V(layer_graph)$name)] <- 6
V(layer_graph)$layer[grep("^7-", V(layer_graph)$name)] <- 7
V(layer_graph)$layer[grep("^8-", V(layer_graph)$name)] <- 8
V(layer_graph)$layer[grep("^9-", V(layer_graph)$name)] <- 9



scaled_weight <- (E(layer_graph)$weight - min(E(layer_graph)$weight)) / (max(E(layer_graph)$weight) - min(E(layer_graph)$weight))
e_color <- rgb(0, (1 - scaled_weight) , 0.5)

# Get edge colors based on edge weights
pdf("Layer plot auto position.pdf")
plot(layer_graph, 
     # layout = layout_as_tree(layer_graph ,
     #                         root = V(layer_graph)$name[grepl("^1-", V(layer_graph)$name)],
     #                         circular = F,
     #                         mode = "all"),
     layout = layout_with_sugiyama(layer_graph,
                                   layers = V(layer_graph)$layer,
# takes a few minutes                                   maxiter = 919500,
                                   maxiter = 100500,
                                   hgap = 610),

     #     layout = layout_with_fr(layer_graph),
     edge.width = normalized_weights * 5 ,  # Set edge width based on weight
#     edge.alpha = inv_weights * 0.5 ,
     edge.label = NA,                     # Remove edge labels
#     edge.color = "darkgray",                     # Remove edge labels
     edge.color = e_color,                     # Remove edge labels
     vertex.label = NA,        # Set vertex label color
     vertex.label.color = "black",        # Set vertex label color
     vertex.size = rescaled,                    # Set vertex size
     vertex.frame.width = 0,                    # Set vertex size
     edge.arrow.size = 0,
     main = "Layered Graph without Labels and Edge Widths Based on Weight")
dev.off()
###Seems to show there is one regularly detected community with the same vars, 
##  this appears at all resolutions. 

#  There are 2 or three similar veins of similarity. 

##Weighting of lines is skewed because larger number of vars in the higher levels


###Try manually separating nodes 

lay_coords <- layout_with_sugiyama(layer_graph,layers = V(layer_graph)$layer)

for (layers in 1:9){
#How many nodes in layer
num_nodes <- length(lay_coords[lay_coords$layout[,2] == layers]) 
#Spacing between each node.
spacing <- 210 / num_nodes
lay_coords$layout[which(lay_coords$layout[,2] == layers),][,1] <- seq(from = spacing , by = spacing, to = spacing * num_nodes)
}

pdf("Layer plot manual position.pdf")
plot(layer_graph, 
     layout = lay_coords,
     edge.width = normalized_weights * 5 ,  # Set edge width based on weight
#     edge.alpha = inv_weights * 0.5 ,
#     edge.color = "darkgray",                     # Remove edge labels
     edge.color = e_color,                     # Remove edge labels
     # vertex.label = NA,        # Set vertex label color
     vertex.label.cex = 0.2,        
     vertex.label.color = "black",        # Set vertex label color
     vertex.size = rescaled,                    # Set vertex size
     vertex.frame.width = 0,                    # Set vertex size
     edge.arrow.size = 0,
     main = "Layered Graph without Labels and Edge Widths Based on Weight")
dev.off()



