library(stringi)
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

# Discard edges whith unknown nodes 
if (length(missing_indx)>0) {
  missing_edges = edges[unique(c(which(edges$Source %in% edges_[missing_indx]), which(edges$Target %in% edges_[missing_indx]))),]
  edges = edges[-unique(c(which(edges$Source %in% edges_[missing_indx]), which(edges$Target %in% edges_[missing_indx]))),]
  message = paste("WARNING: Check data. ", length(missing_indx)," edges removed", ": ") 
  for (k in 1:length(missing_edges$Source)) message = paste(message,",", missing_edges[k,1],"-",missing_edges[k,3],"-", missing_edges[k,2])
  } else message = ""
#print(missing_edges)

