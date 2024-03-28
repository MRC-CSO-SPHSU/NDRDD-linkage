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

#setwd(paste0(wd,"/system_map"))


labels <- read.csv("Labelled Community strength sorted network resolution 1.csv")
labels <- labels[labels$Label !="",]
labels <- select(labels, c(community, Label))

subsystems <- read.csv("linked data subsystems and 5 factors resolution 1.csv")
subsystems$community <- subsystems$Subsystem
head(labels)
head(subsystems)

sort(labels$Label)



table.df <- left_join(subsystems,unique(labels) , by = "community")
dim(subsystems)
head(table.df)
dim(table.df)
table(table.df$Subsystem)
table.df$X <- NULL

write.csv(table.df, file = "Table 1 linked data subsystems and five factors.csv", row.names = F)


###########################
##Same for table 1
###########################



