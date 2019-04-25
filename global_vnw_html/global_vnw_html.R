library(shiny)
library(rmarkdown)
library(knitr)
library(rsconnect)
library(igraph)
library(stringi)
library(tidyverse)
library(visNetwork)
library(shinythemes)

# Network visnetwork

# Import node list: contains nodes$Name, nodes$Group
# It is important to encode with UTF-8 to preserve special characters. 

nodes <- read.csv('new_Nodes.csv', 
                  sep=",", 
                  header = TRUE, 
                  col.names = c("Name","Group"),
                  encoding = "UTF-8")

# Import edge list: contains edges$Source, edges$Target, edges$Relation
edges <- read.csv('new_Edges.csv',
                  header = TRUE, 
                  sep=",",
                  col.names = c("Source","Target","Relation"),
                  encoding = "UTF-8")

# Remove leading and trailing whitespace
nodes$Name <- stri_trim(nodes$Name)
nodes$Group <- stri_trim(nodes$Group)
edges$Source <- stri_trim(edges$Source)
edges$Target <- stri_trim(edges$Target)
edges$Relation <- stri_trim(edges$Relation)

# Check if all names that appears in edges are in nodes
edges_ = c(edges$Source,edges$Target)
length(edges_)
missing = edges_ %in% nodes$Name
missing_indx = which(missing == FALSE)
edges_[missing_indx]
table(edges_ %in% nodes$Name)

# Setting seed is important so the graph is always with the same configuration when starts
set.seed(123)

# Function to rescale node degree
rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

g = graph_from_data_frame(d = edges, directed=TRUE, vertices = nodes)
directed = TRUE

# ids in nodes are es required to export to Pajek format. Just in case!
V(g)$id = V(g)$name

# ids in edges are requiered for visUpdateNodes / visRemoveNodes / visUpdateEdges / visRemoveEdges 
E(g)$id = seq(1:length(E(g)))

g_ <- subgraph.edges(g, which(E(g)$Relation == "is teacher of"))

#Set label size
min_label = 1.0
max_label = 4.0

# Scaling labels
labsize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), min_label, max_label)
V(g_)$label.cex <- labsize

# Funtion to convert igraph format to visNetwork format
data <- toVisNetworkData(g_)

# Set node size
min_node = 20.0
max_node = 40.0

# Scaling nodes
nodesize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), min_node, max_node)
data$nodes$size = nodesize

# Setting parameters straight in the data frame for visNetwork
data$nodes$color.background = case_when(
  data$nodes$Group == "Male" ~ '#FF6347',
  data$nodes$Group == "Female" ~ '#ffa500'
)

# Setting group in visnetwork format
data$nodes$group = data$nodes$Group

data$nodes$color.border = rep("#000000",length(data$nodes$color.background))

data$nodes$color.highlight = case_when(
  data$nodes$Group == "Male" ~ '#47e3ff',
  data$nodes$Group == "Female" ~ '#005aff'
)

# Setting parameters straight in the data frame for visNetwork
data$edges$color.color = case_when(
  data$edges$Relation == "is teacher of" ~ '#0000FF',
  data$edges$Relation == "is friend of" ~ '#228B22',
  data$edges$Relation == "is family of" ~ '#FF0000'
)

data$edges$color.highlight = case_when(
  data$edges$Relation == "is teacher of" ~ '#00ffff',
  data$edges$Relation == "is friend of" ~ '#568b22',
  data$edges$Relation == "is family of" ~ '#ff4000'
)

# Shows the name when hovering over the node
data$nodes$title = paste0("<b>",data$nodes$id,"</b>")

# Shows the relation when hovering over the edge
data$edges$title = paste0("<i>",data$edges$Relation,"</i>")

# nodes data.frame for legend
lnodes <- data.frame(label = c("Male", "Female"),
                     shape = c( "dot"), 
                     color = c("#FF6347", "#ffa500"),
                     id = 1:2)

# edges data.frame for legend
ledges <- data.frame(color = c("#0000FF", "#228B22", "#FF0000"),
                     label = c("is teacher of", "is friend of", "is family of"), 
                     arrows =c("to", FALSE, FALSE), 
                     font.align = "bottom")

# Visnetwork graph creation
vnw_global = visNetwork(nodes = data$nodes, edges = data$edges)%>%
  visNodes(shape = "dot") %>%
  visEdges(arrows =list(to = list(enabled = directed))) %>%
  visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.15, zoom = FALSE)%>%
  visIgraphLayout()%>%
  visOptions(highlightNearest = TRUE)

# Save graph as html
vnw_global %>% visSave(file = "vnw_global.html", background = "white", )