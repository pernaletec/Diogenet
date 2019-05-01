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

# ui stores the user interface of the Shiny application.

ui <- fluidPage(theme = shinytheme("sandstone"),
  
  # Setting seed is important so the graph is always with the same configuration when starts
  set.seed(123),
  # Use the visnetworkOutput function for visnetwork objects
  visNetworkOutput(outputId="network", height = 550),
  
  # Horizontal line
  hr(),
 
  # Configuration 1row (fluid row) x 3columns (column)
  fluidRow(
    # Just space
    column(1,offset=1
           
    ),
    column(3, offset=1,
           # HTML title
           h4("Network Ties"),
           # Selection of the edges that will appear in the relation network 
           checkboxGroupInput("edges_select",
                              label = "Tie Type",
                              choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3),
                              selected = 1,
                              inline = FALSE)
    ),
    column(6,
           # Title of the appereance block
           h4("Appearence"),
	         # Sliders for label size and node size
           sliderInput(inputId = "label_size", label = "Label Size", min = 0.0, max = 5.0, value = c(1, 4),ticks = FALSE),
           sliderInput(inputId = "node_size", label = "Node Size", min = 10.0, max = 60.0, value = c(20, 40),ticks = FALSE),
           hr(),
           helpText("These parameters control de size range of nodes and labels. The minimum size is set to the nodes with the lowest degree, while the maximum size is set to nodes with the highest degree. The same applies for its labels")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
	 # Function to rescale node degree
	rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
	# network stores an object rendered by renderVisnetwork 
	output$network = renderVisNetwork({
	# First validate if there are edges (Ties) selected
	  validate(
	    need(try(!is.null(input$edges_select)), "Please select at least one Tie Type")
	  )
	  # If there is only one edge selected and this edge is the number 1, use directed edges
	  if(length(input$edges_select) == 1 & input$edges_select[1] == 1) {
	    g = graph_from_data_frame(d = edges, directed=TRUE, vertices = nodes)
	    directed = TRUE
	  }else {
	    g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)
	    directed = FALSE
	  }
		
	  # ids in nodes are es required to export to Pajek format. Just in case!
	  V(g)$id = V(g)$name
	  
	  # ids in edges are requiered for visUpdateNodes / visRemoveNodes / visUpdateEdges / visRemoveEdges 
	  E(g)$id = seq(1:length(E(g)))
	  
	  # Who is the subset g_ ?
	  # Just one option selected and...
	  if (length(input$edges_select) == 1){
	    # ...just Teacher selected
	    if (input$edges_select[1] == 1){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation == "is teacher of"))
	      subTitle = "Ties: teacher-student "
	    }
	    # # ...just Friend selected 
	    if (input$edges_select[1] == 2){
	      g_ <- subgraph.edges(g,
	                    which(E(g)$Relation=="is friend of"))
	      subTitle = "Ties: friends"
	    }
 	    # Just Family selected
	    if (input$edges_select[1] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is family of"))
	      subTitle = "ties family"
	    }
	  }
	  # Now there are two options selected and the possible combinations are: 
	  if (length(input$edges_select) == 2){
	    # Teacher and Friends
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of"))
	      subTitle = "Ties: teachers and friends"
	    }
            # Friends and Family	  
	    if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
	      subTitle = "Ties: friends and family"
	    }
            # Teachers and Familiy
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is family of"))
	      subTitle = "Ties: teachers and Family"
	    }
	  }
	  # All selected
	  if (length(input$edges_select) == 3){
	    # Just check that everything is in order
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
	      subTitle = "Ties: teachers, friends, and family"
	    }
	  }
	  
	  #Set label size
	  if (is.null(input$label_size[1])) min_label = 1.0
	  else min_label = input$label_size[1]
	  
	  if (is.null(input$label_size[2])) max_label = 4.0
	  else max_label = input$label_size[2]
	  
	  # Scaling labels
	  labsize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), min_label, max_label)
	  V(g_)$label.cex <- labsize
	  
	  # Funtion to convert igraph format to visNetwork format
	  data <- toVisNetworkData(g_)
	  
	  # Set node size
	  if (is.null(input$node_size[1])) min_node = 20.0
	  else min_node = input$node_size[1]
	  
	  if (is.null(input$node_size[2])) max_node = 40.0
	  else max_node = input$node_size[2]
	  
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
	  
	  
	  # Progress indicator
	  withProgress(message = 'Creating graph', style = 'notification', value = 0.1, {
	    Sys.sleep(0.25)
	    
	    incProgress(1, detail = paste("Running visnetwork"))
	  
      # Visnetwork graph creation
	    visNetwork(nodes = data$nodes, edges = data$edges)%>%
	      visNodes(shape = "dot") %>%
	      visEdges(arrows =list(to = list(enabled = directed))) %>%
	      visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = 0.15, zoom = FALSE)%>%
	      visIgraphLayout()%>%
	      visOptions(highlightNearest = TRUE)
	  })
	})
}

shinyApp(ui = ui, server = server)
