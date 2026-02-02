# Preparation 

# This script reads in the yED co-produced map, creates and edgelist,
# detects communities, estimates degree and closeness, and creates a plot where
# communities are represented with colours.

# Clean work space
rm(list=ls())
gc(full=TRUE)

#########################
#                       #
#    Load packages      #
#                       #
#########################
library("rmarkdown")
library("tinytex")
library("knitr")
library("comato")
library("stringi")
library("igraph")
library("ggraph")
library("ggplot2")
library("stringr")
library("stringi")
library("tidygraph")
library("psych")
library("openxlsx")
library("graphlayouts")
library(igraph)
library(graphlayouts) 
library(ggraph)
library(threejs)
library("ggforce")
library("dplyr")
library("threejs")

library("renv")

####Network community detection and visualisation algorithms may
#   exhibit some random variation. 
#   Random number seeds are set in the code below, and the 
#       renv package used to take a snapshop of the package versions
#   as random number seeds vary with package numbers
#  R version was 4.4.1
#  R studio version was RStudio 2024.04.2+764 "Chocolate Cosmos" 

#renv::snapshot() # this command took a snapshpot of the package and R versions

#To replicate the analysis on your own, run the restore command below

#renv::restore()

#Set working directory
wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(paste0(wd,"/Data"))

#Load raw data from comato Read.Yed
raw_data <- comato::read.yEd("DRD_SystemMap.graphml") 

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

write.csv(node_names, file = paste0("System map edgelist.csv"))

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
unlink("DRD_Kumu_file.xlsx")
saveWorkbook(kumu_file, file = "DRD_Kumu_file.xlsx")

# node names into igraph object for graphing
graph_raw_data <- igraph::graph_from_edgelist(as.matrix(node_names))

V(graph_raw_data)$size <- igraph::degree(graph_raw_data, 
                                         mode = "total")

V(graph_raw_data)$name


# The analysis below shows all the ways in which Criminalisation relates to death within the system map
# crim_to_death_paths <- all_simple_paths(
#   graph_raw_data,
#   from = which(V(graph_raw_data)$name == "Criminalisationof drugs") ,
#   to = which(V(graph_raw_data)$name == "Drug related death"),
#   mode = c("out"),
#   cutoff = -1
# )

# The analysis below shows all the ways in which Criminalisation relates to death within the system map
# stigma_to_death_paths <- all_simple_paths(
#   graph_raw_data,
#   from = which(V(graph_raw_data)$name == "Prevalence of stigmatisingnorms around drugs") ,
#   to = which(V(graph_raw_data)$name == "Drug related death"),
#   mode = c("out"),
#   cutoff = -1
# )
# crim_to_death_paths <- all_simple_paths(
#   graph_raw_data,
#   from = which(V(graph_raw_data)$name == "Prevalence of stigmatisingnorms around drugs") ,
#   to = which(V(graph_raw_data)$name == "Harm reductioninterventions"),
#   mode = c("out"),
#   cutoff = -1
# )
##And this shows the total number of possible paths 
#length(stigma_to_death_paths)
#length(crim_to_death_paths)
##You can inspect each path
# crim_to_death_paths[[1]]
# crim_to_death_paths[[2]]
# crim_to_death_paths[[3]]
# stigma_to_death_paths[[1]]
# plot(induced_subgraph(graph_raw_data , stigma_to_death_paths[[1]]),
#      edge.color = "black",  
#      edge.width = 1,        
#      edge.arrow.size = 0.05)
# plot(induced_subgraph(graph_raw_data , crim_to_death_paths[[2]]),
#      edge.color = "black",  
#      edge.width = 1,        
#      edge.arrow.size = 0.05)
# 
# plot(induced_subgraph(graph_raw_data , crim_to_death_paths[[73]]),
#      edge.color = "black",  
#      edge.width = 1,        
#      edge.arrow.size = 0.05)
# 
# 
# plot(induced_subgraph(graph_raw_data , crim_to_death_paths[[2773]]),
#      edge.color = "black",  
#      edge.width = 1,        
#      edge.arrow.size = 0.05)
# 
# 
# plot(induced_subgraph(graph_raw_data , crim_to_death_paths[[45]]))
# 

###Add multilayer visualisation to unpack upstream-downstream notion of a single
#    ultimate cause versus several upstream-downstream causal subsystems
set.seed(428)
cfg <- igraph::cluster_louvain(as_undirected(graph_raw_data), resolution = 1)
V(graph_raw_data)$grp <- cfg$membership

V(graph_raw_data)$grp <- factor(V(graph_raw_data)$grp,levels = 1:8,labels = c("Stigma",
"Service experience",
"Life experiences",
"Public perspectives",
"Community",
"Proximal causes of death",
"Social influences",
"Safe environments"
))

pre.att.comms <- table(V(graph_raw_data)$grp)

write.csv(V(graph_raw_data)$name, file = "system map node labels.csv")


##Read in manually labelled nodes
se_levels <- read.csv("system map node labels and socio eco class.csv")
se_levels$X <- NULL

##Read in second coder's labels 
se_levels2 <- read.csv("system map node labels and socio eco class KS.csv")
se_levels2$X <- NULL

dim(se_levels)
dim(se_levels2)
names(se_levels2)[3] <- "classKS" 
names(se_levels)[2]  <- "classMMcC"


table(se_levels$classMMcC)
table(se_levels2$classKS)

table(se_levels$classMMcC,
      se_levels2$classKS)

levels.df <- full_join(se_levels, se_levels2)
names(levels.df)
levels.df <- levels.df[,c(1,3,2,4)] 
write.csv(levels.df, file = "system map with se_level coding MMcC KS.csv")

tab <- table(levels.df$classMMcC, levels.df$classKS)
write.csv(tab, file = "se_level crosstab MMcC KS.csv")

se_levels <- read.csv("system map node labels and socio eco class consensus.csv")
se_levels$X <- NULL


##Graphlayouts. Create node attribute with level id. 
V(graph_raw_data)$class <- se_levels$class
table(V(graph_raw_data)$class)

V(graph_raw_data)$lvl <- factor(
  V(graph_raw_data)$class,
  levels = c(
    "death",
    "physical", 
    "behavioural", 
    "cognitive - emotional", 
    "interpersonal", 
    "community",
    "service interaction", 
    "organisational", 
    "policy",
    "wider context" 
  )
)


levels(V(graph_raw_data)$lvl)   # Text labels in the correct order
as.numeric(V(graph_raw_data)$lvl) # Numeric representation of the levels

table(V(graph_raw_data)$lvl, 
as.numeric(V(graph_raw_data)$lvl))


##Wrapped text labels 
V(graph_raw_data)$name <-  c(
  "Workforce \n development", 
  "Collaboration \nbetween services", 
  "Internalised \nstigma", 
  "Positive self\npresentation of PWUD", 
  "Health vs \n criminal justice", 
  "Criminalisation \nof drugs", 
  "Poor treatment \nof PWUD", 
  "Contact with criminal\njustice system", 
  "Stigmatising\nlanguage and behaviour", 
  "Value of professional\nversus peer evidence", 
  "International \nevidence", 
  "Lived experience \nrepresentation in policy", 
  "Poverty", 
  "Social Security \nPolicy", 
  "Employment \nopportunities", 
  "Stigmatising\nnorms around drugs", 
  "Community\n relationships", 
  "Knowledge of drugs and risk\namong PWUD", 
  "Peer workers as\nlower status", 
  "Negative public \n perceptions", 
  "Knowledge of drugs \namong public", 
  "Drug education\nin schools", 
  "Positive \n aspirations", 
  "Family\n relationships", 
  "Peer \nrelationships", 
  "Self worth", 
  "Quality of life", 
  "Trauma", 
  "Mental health", 
  "Initiation of\n drug use", 
  "Control and regularity\nof substance use", 
  "Harm reduction\n interventions", 
  "Exploitation", 
  "Feeling safe", 
  "Availability \nof drugs", 
  "Cost of \ndrugs", 
  "Housing \nPolicy", 
  "Profit in \ndrugs trade", 
  "Social inclusion", 
  "Hiding substance use\nfrom others", 
  "Exposure to drugs", 
  "Drug as \ncoping mechanism", 
  "Community hubs", 
  "Recovery cafes", 
  "Warm spaces", 
  "Community\n activities", 
  "Drug type", 
  "Continued use\n of drugs", 
  "Treatment expectations", 
  "Physical health", 
  "Age", 
  "Medication", 
  "Pain", 
  "Drug use as \n self-medication", 
  "Retention \nin services", 
  "ADP funding", 
  "Availability\n of services", 
  "Pressure \non services", 
  "Attending \nservices", 
  "Hoops to get help\nCriteria/bureaucracy", 
  "Accessibility\n of services", 
  "Quality of treatment planning", 
  "Education on\n harms", 
  "Value placed on\n peer workers", 
  "Representation of \n lived experience", 
  "Peer worker \npay differential", 
  "Stigmatising reporting\nin the media", 
  "Level of local\n health need", 
  "Drug tolerance", 
  "Drug toxicity", 
  "Access to \nsafe supply", 
  "Dependence \non drugs", 
  "Drug use for \nrecreation", 
  "Recovery capital", 
  "Attitude to risk", 
  "Lone versus group\nsubstance use", 
  "Risk of harm occurring\nwhile using drugs", 
  "Poly drug use", 
  "Alarm raising", 
  "Public versus private\ndrug taking", 
  "Life-saving activity", 
  "Drug poisoning", 
  "Heart & \nlung health", 
  "Sedatives", 
  "Practical support", 
  "Quality of patient/service\nrelationship", 
  "Homelessness", 
  "Frailty", 
  "Ease of transitions\nbetween services", 
  "Gender", 
  "Near fatal\n overdose", 
  "Organised crime\n groups", 
  "Quality of \nhousing", 
  "Risk of crime\nvictimisation", 
  "Missed appointments", 
  "Service Quality", 
  "Amount of Public discourse\non drug issues", 
  "Drug related death"
)
  
V(graph_raw_data)$size <- scales::rescale(V(graph_raw_data)$size, to = c(1, 5))

##2 dimensional layout
xy <- graphlayouts::layout_as_multilevel(graph_raw_data,
                           type = "all",
                           ignore_iso = F,
                           alpha = 25,
                           beta = 15
                           )

##Add some vertical jitter
set.seed(2343) 
jitter_amount <- 0.15 # Set jitter amount
xy[, 1] <- xy[, 1] + runif(nrow(xy), -jitter_amount, jitter_amount) # Add jitter to x
xy[, 2] <- xy[, 2] + runif(nrow(xy), -jitter_amount, jitter_amount) # Add jitter to y


level_plot <- ggraph(graph_raw_data, layout = "manual", x = xy[, 1], y = xy[, 2]) +
   geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F, 
                 #repel = T,
                 cex = 1.5) +
  geom_edge_link0(
    aes(filter = (node1.lvl == node2.lvl)),
    edge_colour = "goldenrod3",
    edge_width = 0.3,
  ) + 
  geom_edge_link0(
    aes(filter = (node1.lvl != node2.lvl)),
    alpha = 0.3,
    edge_width = 0.1,
    edge_colour = "black"
  ) +
  geom_node_point(aes(shape =  as.factor(lvl)), fill =  c("#660000",
                                                          "#7f0000", 
  "#b30000",   "#d7301f", 
  "#ef6548",   "#fc8d59",   "#fdbb84",   "#fdd49e", 
  "#fee8c8",   "#fff7ec")[as.numeric(V(graph_raw_data)$lvl)],
  size = V(graph_raw_data)$size
  ) +
  scale_shape_manual(values = rep(21,10)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none")

level_plot 

ggsave(paste0(wd,"/Data/System map social ecological levels plot.pdf"),
              device = "pdf", width = 13.9, height = 10.3, units = "in")

ggsave(paste0(wd,"/Data/System map social ecological levels plot.jpeg"),
       device = "jpeg", width = 13.9, height = 10.3, units = "in")


####With subsystems
set.seed(428)
cfg <- igraph::cluster_louvain(as.undirected(graph_raw_data), resolution = 1)
V(graph_raw_data)$grp <- cfg$membership
#V(graph_raw_data)$name[as.numeric(V(graph_raw_data)$grp) ==1]
V(graph_raw_data)$grp <- factor(V(graph_raw_data)$grp,levels = 1:8,labels = c("Stigma",
"Service experience",
"Public perspectives",
"Life experiences",
"Community",
"Proximal causes of death",
"Social influences",
"Safe environments"
))

post.att.comms <- table(V(graph_raw_data)$grp)
colrs_vertex <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#999999','#a65628','#f781bf' )


subsystem_level_plot <- ggraph(graph_raw_data, layout = "manual", x = xy[, 1], y = xy[, 2]) +
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F, 
                 #repel = T,
                 cex = 1.5) +
  geom_edge_link0(
    aes(filter = (node1.lvl == node2.lvl)),
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  geom_edge_link0(
    aes(filter = (node1.lvl != node2.lvl)),
    alpha = 0.3,
    edge_width = 0.1,
    edge_colour = "black"
  ) +
  geom_node_point(aes(shape =  as.factor(lvl)), fill =  colrs_vertex[V(graph_raw_data)$grp],
  size = V(graph_raw_data)$size
  ) +
  scale_shape_manual(values = rep(21,10)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") 


subsystem_level_plot

lev_labels <- tools::toTitleCase(levels(V(graph_raw_data)$lvl))

finplot <- subsystem_level_plot + 
  annotate("text", x = 1.6, y = 2.5,
           label = lev_labels[2], hjust = -0.1) + 
  annotate("text", x = 1.6, y = 3.3,
           label = lev_labels[3], hjust = -0.1) + 
  annotate("text", x = 1.6, y = 4.3,
           label = lev_labels[4], hjust = -0.1) + 
  annotate("text", x = 1.6, y = 5.2,
           label = lev_labels[5], hjust = -0.1) + 
  annotate("text", x = 1.6, y = 6.2,
           label = lev_labels[6], hjust = -0.1) + 
  annotate("text", x = 1.6, y = 7.2,
           label = lev_labels[7], hjust = -0.1) +
  annotate("text", x = 1.6, y = 7.8,
           label = lev_labels[8], hjust = -0.1) +
  annotate("text", x = 1.6, y = 8.8,
           label = lev_labels[9], hjust = -0.1) +
  annotate("text", x = 1.6, y = 9.8,
           label = lev_labels[10], hjust = -0.1)

finplot
unlink(paste0(wd,"/Data/Figure 1 System map social ecological levels plot with subsystems.pdf"))
       
ggsave(paste0(wd,"/Data/part of Figure 1 System map social ecological levels plot with subsystems.pdf"),
       device = "pdf", width = 13.9, height = 10.3, units = "in")

ggsave(paste0(wd,"/Data/part of Figure 1 System map social ecological levels plot with subsystems.jpeg"),
       device = "jpeg", width = 13.9, height = 10.3, units = "in")

table(round(xy[,2],1))



#######Draw with convex hulls around communities
subsystem_level_plot <- ggraph(graph_raw_data, layout = "manual", x = xy[, 1], y = xy[, 2]) +
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F, 
                 #repel = T,
                 cex = 1.5) +
  geom_edge_link0(
    aes(filter = (node1.lvl == node2.lvl)),
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  geom_edge_link0(
    aes(filter = (node1.lvl != node2.lvl)),
    alpha = 0.3,
    edge_width = 0.1,
    edge_colour = "black"
  ) +
  geom_node_point(aes(shape =  as.factor(lvl)), fill =  colrs_vertex[V(graph_raw_data)$grp],
  size = V(graph_raw_data)$size
  ) +
  scale_shape_manual(values = rep(21,10)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  geom_mark_hull(
        aes(x, y, group = grp, fill = grp, label = grp),
        concavity = 4,
        expand = unit(2, "mm"),
        alpha = 0.25)

subsystem_level_plot 

ggsave(paste0(wd,"/Data/other part of fig 1 System map social ecological levels plot with subsystems and hull.pdf"),
              device = "pdf", width = 13.9, height = 10.3, units = "in")

ggsave(paste0(wd,"/Data/other part of fig 1 System map social ecological levels plot with subsystems and hull.jpeg"),
       device = "jpeg", width = 13.9, height = 10.3, units = "in")

###Combine main plot and inset

#######Draw with convex hulls around communities - remove labels
subsystem_level_plot <- ggraph(graph_raw_data, layout = "manual", x = xy[, 1], y = xy[, 2]) +
  # geom_node_text(aes(label = name), nudge_y = -0.1,
  #                check_overlap = F, 
  #                #repel = T,
  #                cex = 1.5) 
    geom_edge_link0(
    aes(filter = (node1.lvl == node2.lvl)),
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  geom_edge_link0(
    aes(filter = (node1.lvl != node2.lvl)),
    alpha = 0.3,
    edge_width = 0.1,
    edge_colour = "black"
  ) +
  geom_node_point(aes(shape =  as.factor(lvl)), fill =  colrs_vertex[V(graph_raw_data)$grp],
  size = V(graph_raw_data)$size
  ) +
  scale_shape_manual(values = rep(21,10)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  geom_mark_hull(
        aes(x, y, group = grp, fill = grp, label = grp),
        concavity = 4,
        expand = unit(2, "mm"),
        alpha = 0.25)



# install.packages("cowplot") # if needed
library(cowplot)

sub_inset <- subsystem_level_plot +
  theme(legend.position = "none",
        plot.margin = margin(1, 1, 1, 1),
        text = element_text(size = 8))

p <- ggdraw(finplot) +
  draw_plot(
    sub_inset,
    x = 0.03, y = 0.03,   # bottom-left anchor position
    width  = 0.25,        # width as fraction of main canvas
    height = 0.25,
    hjust = 0, vjust = 0
  )

ggsave(
  "Figure 1 System map levels with subset of systems.pdf",
  plot = p,
  width = 13, height = 6, units = "in",
  device = cairo_pdf
)




#######Draw one community at a time with fixed layout
V(graph_raw_data)$grp <- cfg$membership

#######################
#Group 1
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 1)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 1, ]

comm1 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Stigma")

####################

#######################
#Group 2
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 2)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 2, ]

comm2 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Service experience")

####################
#######################
#Group 3
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 3)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 3, ]

comm3 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Life experience")

####################
#######################
#Group 4
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 4)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 4, ]

comm4 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Public perspectives")

####################
#######################
#Group 5
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 5)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 5, ]

comm5 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Community")

####################
#######################
#Group 6
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 6)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 6, ]

comm6 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Proximal causes")

####################
#######################
#Group 7
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 7)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 7, ]

comm7 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill =  colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Social influences")

#######################
#Group 8
####################

graph_filtered <- graph_raw_data %>%
  induced_subgraph(V(graph_raw_data)$grp == 8)

# Manually set coordinates (if needed) for the filtered graph
xy_filtered <- xy[V(graph_raw_data)$grp == 8, ]

comm8 <- ggraph(graph_filtered, layout = "manual", x = xy_filtered[, 1], y = xy_filtered[, 2]) +
  # Draw node labels for filtered nodes
  geom_node_text(aes(label = name), nudge_y = -0.1,
                 check_overlap = F,
                 cex = 1.5) +
  # Draw edges between the filtered nodes
  geom_edge_link0(
    edge_colour = "goldenrod3",
    edge_width = 0.3
  ) + 
  # Draw node points for filtered nodes
  geom_node_point(aes(shape = as.factor(lvl)),
                  fill = colrs_vertex[V(graph_filtered)$lvl],
                  size = V(graph_filtered)$size) +
  scale_shape_manual(values = rep(21, 9)) +
  theme_graph() +
  coord_cartesian(clip = "off", expand = TRUE) +
  theme(legend.position = "none") +
  ggtitle("Safe environments")


install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")  
fonts()

pdf(file = "Multilayer plots per community.pdf")
comm1
comm2
comm3
comm4
comm5
comm6
comm7
comm8
dev.off()

##3 dimensional layout
xyz <- layout_as_multilevel(
  graph_raw_data,
  type = "all",
  alpha = 25,beta = 45,
  project2D = F
)
graph_raw_data$layout <- xyz

# V(graph_raw_data)$color <- c("#660000","#7f0000", 
#   "#b30000", 
#   "#d7301f", 
#   "#ef6548", 
#   "#fc8d59", 
#   "#fdbb84", 
#   "#fdd49e", 
#   "#fee8c8", 
#   "#fff7ec")[as.numeric(V(graph_raw_data)$lvl)]

V(graph_raw_data)$color <- colrs_vertex[V(graph_raw_data)$grp]


V(graph_raw_data)$vertex.label <- V(graph_raw_data)$name

graphjs(graph_raw_data, bg = "black", vertex.shape = "sphere",
         vertex.label = V(graph_raw_data)$vertex.label,
        vertex.size = V(graph_raw_data)$size )



set.seed(428)
cfg <- igraph::cluster_louvain(as.undirected(graph_raw_data), resolution = 1)

V(graph_raw_data)$community <- cfg$membership
V(graph_raw_data)$closeness <- igraph::closeness(graph_raw_data, 
                                                 mode = "total")

V(graph_raw_data)$in_degree <- igraph::degree(graph_raw_data, mode = "in")
V(graph_raw_data)$out_degree <- igraph::degree(graph_raw_data, mode = "out")

#colrs_vertex <- c( '#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d','#666666')

colrs_vertex <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#999999','#a65628','#f781bf' )

colr_edges <- "#808080"

E(graph_raw_data)$color <- "808080"

set.seed(1212)
community_graph <- ggraph(graph_raw_data, 
                          layout = "fr") +
  geom_edge_fan(color = "grey80") +
  geom_edge_link(arrow = arrow(length = unit(2, 'mm')), 
                 end_cap = circle(12, 'mm'),
                 color = "grey80") +
  geom_node_point(aes(size = V(graph_raw_data)$size), color = "white") +
  geom_node_point(aes(size = V(graph_raw_data)$size), 
                   color = colrs_vertex[V(graph_raw_data)$community], 
                   alpha = 0.7, shape = 20) +
  geom_node_text(aes(label = V(graph_raw_data)$name),
                 repel = TRUE, max.overlaps = Inf, 
                 color = "black", size = 2) +
  scale_size_continuous(range = c(2, 10)) +
  theme_graph(base_family = "Arial") +
  theme(legend.position = "none")


community_graph

ggsave(paste0(wd,"/Data/Appendix Figure 3  System map Community Dectection.pdf"),
              device = "pdf", width = 13.9, height = 10.3, units = "in")

ggsave(paste0(wd,"/Data/Appendix Figure 3 System map Community Dectection.jpeg"),
       device = "jpeg", width = 13.9, height = 10.3, units = "in")

community_data_frame <- as.data.frame(list(vertex=V(graph_raw_data), 
                                      community = V(graph_raw_data)$community, 
                                      degree = igraph::degree(graph_raw_data),
                                      in_degree = igraph::degree(graph_raw_data, mode = "in"),
                                      out_degree = igraph::degree(graph_raw_data, mode = "out"),
                                      closeness = igraph::closeness(graph_raw_data, mode = "all"), 
                                      betweeness = igraph::betweenness(graph_raw_data, directed = TRUE, normalized = TRUE), 
                                      avg_path_length = igraph::mean_distance(graph_raw_data, directed = TRUE),
                                      density = igraph::graph.density(graph_raw_data)))
write.csv(community_data_frame, file = "Full system map with community detection and degrees.csv")

######Table 1: Subsystems returned from community detection algorithms 
#  applied to the system map, and factors in each within highest degree

table1 <- as.data.frame((sort(table(V(graph_raw_data)$community), decreasing = T)))
#table(table1$Var1, as.numeric(as.character(table1$Var1)))
#Convert to numeric
table1$Var1 <- as.numeric(as.character(table1$Var1))

table1$central_factor_1     <- NA
table1$central_factor_1_deg <- NA
table1$central_factor_1_btw <- NA
table1$central_factor_2     <- NA
table1$central_factor_2_deg <- NA
table1$central_factor_2_btw <- NA
table1$central_factor_3     <- NA
table1$central_factor_3_deg <- NA
table1$central_factor_3_btw <- NA
table1$central_factor_4     <- NA
table1$central_factor_4_deg <- NA
table1$central_factor_4_btw <- NA
table1$central_factor_5     <- NA
table1$central_factor_5_deg <- NA
table1$central_factor_5_btw <- NA



for (i in table1$Var1){
  ##Store name of var
  temp_comm_df <- community_data_frame[which(community_data_frame$community == i),]
  temp_comm_df <- temp_comm_df[order(temp_comm_df$deg , decreasing = T),]
  
  table1$central_factor_1[which(table1$Var1     == i)] <- rownames(temp_comm_df)[1]
  #Store degree of var
  table1$central_factor_1_deg[which(table1$Var1 == i)] <- temp_comm_df[1,"degree"]
  table1$central_factor_1_btw[which(table1$Var1 == i)] <- round(temp_comm_df[1,"betweeness"],2)

  table1$central_factor_2[which(table1$Var1     == i)] <- rownames(temp_comm_df)[2]
  table1$central_factor_2_deg[which(table1$Var1 == i)] <- temp_comm_df[2,"degree"]
  table1$central_factor_2_btw[which(table1$Var1 == i)] <- round(temp_comm_df[2,"betweeness"],2)

  table1$central_factor_3[which(table1$Var1     == i)] <- rownames(temp_comm_df)[3]
  table1$central_factor_3_deg[which(table1$Var1 == i)] <- temp_comm_df[3,"degree"]
  table1$central_factor_3_btw[which(table1$Var1 == i)] <- round(temp_comm_df[3,"betweeness"],2)

  table1$central_factor_4[which(table1$Var1     == i)] <- rownames(temp_comm_df)[4]
  table1$central_factor_4_deg[which(table1$Var1 == i)] <- temp_comm_df[4,"degree"]
  table1$central_factor_4_btw[which(table1$Var1 == i)] <- round(temp_comm_df[4,"betweeness"],2)

  table1$central_factor_5[which(table1$Var1     == i)] <- rownames(temp_comm_df)[5]
  table1$central_factor_5_deg[which(table1$Var1 == i)] <- temp_comm_df[5,"degree"]
  table1$central_factor_5_btw[which(table1$Var1 == i)] <- round(temp_comm_df[5,"betweeness"],2)
   #  rm(temp_comm_df)
  }
names(table1)[1:2] <- c("Subsystem","Number of factors")

new_names <- sub("^(.*)(\\d+)(_.*)$", "\\1\\3\\2", names(table1))
names(table1) <- new_names


  
write.csv(table1, file = "Subsystems nodes top 5 factors wide.csv")

####New long format
factors <-  select(table1,
                   c("Subsystem","Number of factors",
                     "central_factor_1",     
                     "central_factor_2",  
                     "central_factor_3",
                     "central_factor_4",   
                     "central_factor_5"))
degs <-  select(table1,
                   c("Subsystem","Number of factors",
                     "central_factor__deg1", 
                     "central_factor__deg2", 
                     "central_factor__deg3",
                     "central_factor__deg4",
                     "central_factor__deg5")) 

btws <-  select(table1,
                   c("Subsystem","Number of factors",
                     "central_factor__btw1",
                     "central_factor__btw2",
                     "central_factor__btw3",   
                     "central_factor__btw4",     
                     "central_factor__btw5"))

factors <- reshape2::melt(factors, id.vars = c("Subsystem", "Number of factors"), value.name = "factor" )
degs    <- reshape2::melt(degs   , id.vars = c("Subsystem", "Number of factors"), value.name = "degree" )
btws    <- reshape2::melt(btws   , id.vars = c("Subsystem", "Number of factors"), value.name = "betweenness" )

factors$variable <- as.numeric(factors$variable)
degs$variable    <- as.numeric(degs$variable)
btws$variable    <- as.numeric(btws$variable)

tt <- dplyr::full_join(factors, degs)
tt <- dplyr::full_join(tt, btws)
table1 <- tt[order(tt$`Number of factors` , tt$Subsystem , decreasing = T),]

write.csv(table1, file = "Table 1 subsystems nodes deg btw.csv")


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
          paste0(wd,"/Data/TableNetworkSummaryMeasures.csv"), 
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
          paste0(wd,"/Data/TableMaximumValues.csv"), 
          row.names = TRUE)

# select top 10 for degree
TableDegree <- community_data_frame %>%
  arrange(desc(degree)) 

TableDegree <- head(TableDegree, 20)

write.csv(TableDegree, 
          paste0(wd,"/Data/TableTopDegree.csv"), 
          row.names = TRUE)

# Highest degree within each community 
TableWithinCommunty <- community_data_frame %>%
  group_by(community) %>%
  filter(degree == max(degree)) %>%
  ungroup()

write.csv(TableWithinCommunty, 
          paste0(wd,"/Data/TableWithinCommunty.csv"), 
          row.names = TRUE)

base::saveRDS(community_data_frame, paste0(wd,"/CoProducedCommunities.rds"))
write.csv(community_data_frame, paste0(wd,"/CoProducedCommunities.csv"))


######################################################
## Analysis of community by Social ecological layer ##
######################################################

V(graph_raw_data)$se_level <- V(graph_raw_data)$class

V(graph_raw_data)$se_level[V(graph_raw_data)$se_level == "death"] <- "physical"

V(graph_raw_data)$se_level <- factor(
  V(graph_raw_data)$class,
  levels = c(
     "death",
    "physical", 
    "behavioural", 
    "cognitive - emotional", 
    "interpersonal", 
    "community",
    "service interaction", 
    "organisational", 
    "policy",
    "wider context" 
 )
)

table(V(graph_raw_data)$se_level)
table(V(graph_raw_data)$grp)

V(graph_raw_data)$grp <- factor(
  V(graph_raw_data)$community,
  levels = 1:8,
  labels = c(
    "Stigma",
    "Service \nexperience",
    "Public \nperspectives",
    "Life \nexperiences",
    "Community",
    "Proximal causes\n of death",
    "Social \ninfluences",
    "Safe \nenvironments"
  )
)

level_by_comm <- table(V(graph_raw_data)$se_level,V(graph_raw_data)$grp)
level_by_comm <- table(V(graph_raw_data)$grp,V(graph_raw_data)$se_level)

class(level_by_comm)

chisq.test(level_by_comm)
chisq.test(level_by_comm)$expected
fisher.test(level_by_comm, simulate.p.value = T, B = 200)
# fisher.test(level_by_comm, simulate.p.value = T, B = 1000000)

levels_var <- c("Stigma",
    "Service \nexperience",
    "Public \nperspectives",
    "Life \nexperiences",
    "Community",
    "Proximal causes\n of death",
    "Social \ninfluences",
    "Safe \nenvironments")

colours_longvert <- setNames(colrs_vertex, levels_var)

# Add the colors to your data frame
level_by_comm_df <- as.data.frame(as.table(level_by_comm))
level_by_comm_df$colrs_vertex <- colours_longvert[level_by_comm_df$Var1]

max_freq <- max(level_by_comm_df$Freq, na.rm = TRUE)

# Create bubble plot with frequency of nodes by level and subsystem
bubble <- ggplot(level_by_comm_df,
       aes(
         x = Var1,y = Var2,
         size = Freq,color = colrs_vertex
       )) +
  geom_point() +
  scale_size_continuous(
    range = c(1, 15),
    limits = c(0, max_freq),
    breaks = seq(0, max_freq, by = 3)
  ) +
  labs(x = "Subsystem", y = "Level of influence", size = "Frequency") +
  theme_minimal() +
  guides(x = guide_axis(angle = 45), color = "none")

pdf("Appendix Figure 4 factors by level and subsystem.pdf")
bubble
dev.off()
bubble
ggsave(paste0(wd,"/Data/Appendix Figure 4 factors by level and subsystem.jpeg"),
       device = "jpeg", width = 13.9, height = 10.3, units = "in")


write.csv(t(level_by_comm), file = "Sub system by soc eco level crosstab.csv")

node_level_list <- data.frame(name  = V(graph_raw_data)$name,
                              lvlnum = V(graph_raw_data)$se_level)
head(node_level_list)
node_level_list <- node_level_list[order(node_level_list$lvlnum),]
write.csv(node_level_list, file = "System map nodes and levels.csv")

