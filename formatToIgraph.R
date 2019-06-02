# Reading data saved as .csv

edgesFile = "Code–Code Link Manager (2).csv"
nodesFile = "MasterProjectFile 05242019 - Code Manager.csv"
  
edges = read.csv(file = edgesFile, header = TRUE , encoding = "UTF-8",sep = ";", stringsAsFactors = FALSE)
nodes = read.csv(file = nodesFile, header = TRUE , encoding = "UTF-8",sep = ";", stringsAsFactors = FALSE)

# Subsetting 
edges = subset(x = edges,select = c(Source,Target,Relation))
nodes = subset(x = nodes, select = c(Name, Groups))

nodes = subset(nodes,!grepl('NN_',nodes$Name))
# Check wheather NN_ is present
# FALSE + FALSE = 0, FALSE + TRUE = 1, TRUE + TRUE = 2
aux = grepl('NN_',edges$Source)+grepl('NN_',edges$Target)
# If aux = 1 or greater convert it to 1 
aux = sapply(aux, FUN = function(x) if (x>0) return(1) else return(0), simplify = TRUE)
# as.logical convert 0 to FALSE and 1 to TRUE
aux = as.logical(aux)
edges = subset(edges,!aux)

# Removing prefixes { G_ ; N_ ; NF_ ; ND_ }  

# From edges...
edges[] <- lapply(edges, gsub, pattern="G_", replacement="")
edges[] <- lapply(edges, gsub, pattern="N_", replacement="")
edges[] <- lapply(edges, gsub, pattern="NF_", replacement="")
edges[] <- lapply(edges, gsub, pattern="ND_", replacement="")
edges[] <- lapply(edges, gsub, pattern="?", replacement="")

# From nodes...
nodes[] <- lapply(nodes, gsub, pattern="G_", replacement="")
nodes[] <- lapply(nodes, gsub, pattern="N_", replacement="")
nodes[] <- lapply(nodes, gsub, pattern="NF_", replacement="")
nodes[] <- lapply(nodes, gsub, pattern="ND_", replacement="")
nodes[] <- lapply(nodes, gsub, pattern="?", replacement="")

# Checking for duplicates
dup_nodes = duplicated(nodes$Name)
dup_edges = duplicated(edges)

if(sum(dup_nodes=="TRUE")>0)  {
  nodes_dup = paste(nodes[duplicated(nodes$Name),"Name"],
                   nodes[duplicated(nodes$Name),"Groups"],
                   collapse = "; ")
  msg = paste("Duplicated nodes were found: ", nodes_dup)
  stop(msg)
  }
if(sum(dup_edges=="TRUE")>0)  {
  edges_dup = paste(edges[duplicated(edges),"Source"],
                    edges[duplicated(edges),"Target"],
                    edges[duplicated(edges),"Relation"],
                    collapse = "; ")
  msg = paste("Duplicated edges were found: ", edges_dup)
  stop(msg)
}

# Defining new name for output file
newEdgesFile = paste("new_",edgesFile,sep="")
newNodesFile = paste("new_",nodesFile,sep="")

write.csv(edges,file = newEdgesFile, fileEncoding = "UTF-8",row.names=FALSE, quote = FALSE)
write.csv(nodes,file = newNodesFile, fileEncoding = "UTF-8",row.names=FALSE, quote = FALSE)
