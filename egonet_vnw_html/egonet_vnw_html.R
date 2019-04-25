library(rmarkdown)
library(knitr)
library(rsconnect)
library(igraph)
library(stringi)
library(tidyverse)
library(visNetwork)

# Egograph visnetwork

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

set.seed(123)
# function to rescale node degree
rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

g = graph_from_data_frame(d = edges, directed=TRUE, vertices = nodes)
directed = TRUE

# id en nodos es necesario para exportar al formato Pajek
V(g)$id = V(g)$name

# id en ejes es necesario para visUpdateNodes / visRemoveNodes / visUpdateEdges / visRemoveEdges 
E(g)$id = seq(1:length(E(g)))

g_ <- subgraph.edges(g, which(E(g)$Relation == "is teacher of"))
    
# Egonet
d <- make_ego_graph(g_,
                    order = 2,
                    nodes = "Plato",
                    mode = c("all"),
                    mindist = 0)

#Set label size
min_label = 0.4
max_label = 0.9

labsize <- rescale(degree(d[[1]]), min(degree(d[[1]])), max(degree(d[[1]])), min_label, max_label)
V(d[[1]])$label.cex <- labsize

#subTitle = paste0("Egonet of Variable ", ego_node, ", ",subTitle)

data <- toVisNetworkData(d[[1]])

# Set node size
min_node = 8.0
max_node = 20.0

nodesize <- rescale(degree(d[[1]]), min(degree(d[[1]])), max(degree(d[[1]])), min_node, max_node)
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

# Shows the name when hovering over the node
data$nodes$title = paste0("<b>",data$nodes$id,"</b>")

# Shows the relation when hovering over the edge
data$edges$title = paste0("<i>",data$edges$Relation,"</i>")

egonet_vnw_html = visNetwork(nodes = data$nodes, edges = data$edges)%>%
  visNodes(shape = "dot") %>%
  visEdges(arrows =list(to = list(enabled = directed)),
           color = list(color = "gray",
                        highlight = "red")) %>%
  visLegend(addNodes = lnodes, useGroups = FALSE, width = 0.15, zoom = FALSE)%>%
  visIgraphLayout()%>%
  visOptions(highlightNearest = TRUE)
  	   
egonet_vnw_html %>% visSave(file = "egonet_vnw.html", background = "white")