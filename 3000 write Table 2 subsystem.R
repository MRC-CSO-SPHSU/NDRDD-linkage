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


wd <- "T:/projects/CSO_DRD_S00359/Data"

setwd(wd)

labels <- read.csv("Labelled Community strength sorted network resolution 1.csv")
names(labels)
labels$Group[labels$Group ==""] <- NA

table(labels$Group)
table(labels$community)



labels <- labels[labels$Label !="",]

labels <- dplyr::select(labels, c(community, Label,Group))

subsystems <- read.csv("linked data subsystems and 5 factors resolution 1.csv")
subsystems$community <- subsystems$Subsystem
head(labels, 16)
head(subsystems)

sort(labels$Label)


table.df <- left_join(subsystems,unique(labels) , by = "community")
dim(subsystems)
head(table.df)
dim(table.df)
table(table.df$Subsystem)
table.df$X <- NULL

table.df$Label <- gsub("Isolated component:", "", table.df$Label)

write.csv(table.df, file = "Table 2 linked data subsystems and five factors.csv", row.names = F)


# table.df <- table.df %>%
#   arrange(Group, desc(Number.of.factors))
# 
# write.csv(table.df, file = "Table 2 linked data subsystems and five factors with three groups.csv", row.names = F)

df_filtered <- table.df %>% 
  arrange(Number.of.factors) %>% 
  distinct(Subsystem, .keep_all = TRUE)

table(table.df$Subsystem)

table(df_filtered$Subsystem)

df_filtered <- df_filtered[,c("Number.of.factors","Label","Group")]

df_filtered <- df_filtered %>%
  arrange(Group, desc(Number.of.factors))

table(df_filtered$Label, df_filtered$Group)

df_filtered <- df_filtered[,c(2,1,3)]
names(df_filtered) <- c("Subsystem","'Number of factors'","Group")


write.csv(df_filtered, file = "Table 2 Subsystems by three groups.csv")
