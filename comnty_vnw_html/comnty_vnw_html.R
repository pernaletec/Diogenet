library(shiny)
library(rmarkdown)
library(knitr)
library(rsconnect)
library(igraph)
library(stringi)
library(tidyverse)
library(visNetwork)

# Community visnetwork

# Import node list: contains nodes$Name, nodes$Group
nodes <- read.csv('new_Nodes.csv', 
                  sep=",", 
                  header = TRUE, 
                  col.names = c("Name","Group"),
                  encoding = "UTF-8")

# Import edge list: contains edges$Source, edges$Target, edges$Weight, edges$Relation
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

# Verifica consistencia vertices - ejes
edges_ = c(edges$Source,edges$Target)
length(edges_)
missing = edges_ %in% nodes$Name
missing_indx = which(missing == FALSE)
edges_[missing_indx]
table(edges_ %in% nodes$Name)

# Setting seed is important so the graph is always with the same configuration when starts
set.seed(123)
# function to rescale node degree
rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)

# ids in nodes are es required to export to Pajek format. Just in case!
V(g)$id = V(g)$name

# ids in edges are requiered for visUpdateNodes / visRemoveNodes / visUpdateEdges / visRemoveEdges 
E(g)$id = seq(1:length(E(g)))

# ...and the subgraph is finally created
g_ <- subgraph.edges(g, which(E(g)$Relation == "is teacher of"))

# Community detection
cluster = cluster_leading_eigen(g_)

#Set label size
min_label = 1.0
max_label = 4.0

# Scaling labels
labsize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), min_label, max_label)
V(g_)$label.cex <- labsize

# Assign membership ids to vertex
V(g_)$community  <- cluster$membership

data <- toVisNetworkData(g_)

# Set node size
min_node = 20.0
max_node = 40.0

# Scaling nodes
nodesize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), min_node, max_node)
data$nodes$size = nodesize

# A try for controling the colours of each group
node_colours = rainbow(summary(cluster$membership)[6])
setColour = function(x) {node_colours[x]}

# Setting parameters straight in the data frame for visNetwork
#data$nodes$color.background = sapply(data$nodes$community, FUN = setColour)
  
# Setting group in visnetwork format
data$nodes$group = data$nodes$community

data$nodes$color.border = rep("#000000",length(data$nodes$group))

#data$nodes$color.highlight = sapply(data$nodes$community, FUN = setColour)

# Setting parameters straight in the data frame for visNetwork
data$edges$color.color = case_when(
  data$edges$Relation == "is teacher of" ~ '#0000FF',
  data$edges$Relation == "is friend of" ~ '#228B22',
  data$edges$Relation == "is family of" ~ '#FF0000',
  data$edges$Relation == "studied the work of" ~ '#ff8c00'
)

data$edges$color.highlight = case_when(
  data$edges$Relation == "is teacher of" ~ '#00ffff',
  data$edges$Relation == "is friend of" ~ '#568b22',
  data$edges$Relation == "is family of" ~ '#00ff00',
  data$edges$Relation == "studied the work of" ~ '#1a00ff'
)

# # nodes data.frame for legend
# lnodes <- data.frame(label = c("Male", "Female"),
#                      shape = c( "dot"), 
#                      color = c("#FF6347", "#ffa500"),
#                      id = 1:2)
# 

# edges data.frame for legend
ledges <- data.frame(color = c("#0000FF", "#228B22", "#FF0000", "#ff8c00"),
                     label = c("is teacher of", "is friend of", "is family of", "studied the work of"),
                     arrows =c("to", FALSE, FALSE, FALSE),
                     font.align = "bottom")

# Shows the name when hovering over the node
data$nodes$title = paste0("<b>",data$nodes$id,"</b>","<br/>" , "Community N°: ", "<b>",data$nodes$community,"</b>")

# Shows the relation when hovering over the edge
data$edges$title = paste0("<i>",data$edges$Relation,"</i>")

	mod <- round(modularity(cluster),3)
	subTitle = paste0("MODULARITY: ", mod)
	
	# Visnetwork graph creation
	comnty_vnw = visNetwork(nodes = data$nodes, edges = data$edges, main=subTitle)%>%
	  visNodes(shape = "dot") %>%
	  visEdges(arrows =list(to = list(enabled = FALSE))) %>%
	  visIgraphLayout()%>%
	  visOptions(highlightNearest = TRUE)
	
	comnty_vnw %>% visSave(file = "comnty_vnw.html", background = "white")
    		