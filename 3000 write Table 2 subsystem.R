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

if (Sys.info()[4] == "DESKTOP-2CKTEEO") wd <- "C:/Users/mmc78h/OneDrive - University of Glasgow/DRD/GGMnonreg/Below zero"
setwd(wd)

#setwd(paste0(wd,"/system_map"))


labels <- read.csv("Finalised labelled communities.csv")

labels$Higher.classification[labels$Higher.classification ==""] <- NA

table(labels$Higher.classification)
table(labels$community)



labels <- labels[labels$Label !="",]
labels <- select(labels, c(community, Label,Higher.classification))

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

write.csv(table.df, file = "Table 2 linked data subsystems and five factors.csv", row.names = F)


table.df <- table.df %>%
  arrange(Higher.classification, desc(Number.of.factors))

write.csv(table.df, file = "Table 2 linked data subsystems and five factors with three groups.csv", row.names = F)

df_filtered <- table.df %>% 
  arrange(Number.of.factors) %>% 
  distinct(Subsystem, .keep_all = TRUE)

table(table.df$Subsystem)

table(df_filtered$Subsystem)

df_filtered <- df_filtered[,c("Number.of.factors","Label","Higher.classification")]

df_filtered <- df_filtered %>%
  arrange(Higher.classification, desc(Number.of.factors))
df_filtered$Label <- gsub("Isolated component:", "", df_filtered$Label)


table(df_filtered$Label, df_filtered$Higher.classification)

df_filtered <- df_filtered[,c(2,1,3)]
names(df_filtered) <- c("Subsystem","'Number of factors'","Group")


write.csv(df_filtered, file = "Subsystems by three groups.csv")
