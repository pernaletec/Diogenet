library(shiny)
library(rmarkdown)
library(knitr)
library(rsconnect)
library(igraph)
library(stringi)
library(tidyverse)
library(visNetwork)

# Network visnetwork

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

# Define ----
ui <- fluidPage(
  
  set.seed(123),
  visNetworkOutput(outputId="network", height = 550),
  
  hr(),
  
  fluidRow(
    column(1,offset=1
           
    ),
    column(3, offset=1,
           h4("Network Ties"),
           br(),
           # Selection of the edges that will appear in the relation network 
           checkboxGroupInput("edges_select",
                              label = "Tie Type",
                              choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3),
                              selected = 1,
                              inline = FALSE)
    ),
    column(6,
           # Plot Height
           h4("Appearence"),
           br(),
           sliderInput(inputId = "label_size", label = "Label Size", min = 0.0, max = 5.0, value = c(1, 4),ticks = FALSE),
           sliderInput(inputId = "node_size", label = "Node Size", min = 10.0, max = 60.0, value = c(20, 40),ticks = FALSE)
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
	 # function to rescale node degree
	rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}

	output$network = renderVisNetwork({
	  
	  validate(
	    need(try(!is.null(input$edges_select)), "Please select at least one Tie Type")
	  )

	  if(length(input$edges_select) == 1 & input$edges_select[1] == 1) {
	    g = graph_from_data_frame(d = edges, directed=TRUE, vertices = nodes)
	    directed = TRUE
	  }else {
	    g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)
	    directed = FALSE
	  }
		
	  # id en nodos es necesario para exportar al formato Pajek
	  V(g)$id = V(g)$name
	  
	  # id en ejes es necesario para visUpdateNodes / visRemoveNodes / visUpdateEdges / visRemoveEdges 
	  E(g)$id = seq(1:length(E(g)))
	  
	  # Who is the subset g_ ?

	  if (length(input$edges_select) == 1){
	    if (input$edges_select[1] == 1){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation == "is teacher of"))
	      subTitle = "Ties: teacher-student "
	    }
	    if (input$edges_select[1] == 2){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is friend of"))
	      subTitle = "Ties: friends"
	    }
	    if (input$edges_select[1] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is family of"))
	      subTitle = "ties family"
	    }
	  }
	  if (length(input$edges_select) == 2){
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of"))
	      subTitle = "Ties: teachers and friends"
	    }
	    if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
	      subTitle = "Ties: friends and family"
	    }
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is family of"))
	      subTitle = "Ties: teachers and Family"
	    }
	  }
	  if (length(input$edges_select) == 3){
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
	      g_ <- subgraph.edges(g,
	                           which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
	      subTitle = "Ties: teachers, friends, and family"
	    }
	  }
	  
	  
	  #Set label size
	  if (is.null(input$label_size[1])) min_label = 0.0
	  else min_label = input$label_size[1]
	  
	  if (is.null(input$label_size[2])) max_label = 5.0
	  else max_label = input$label_size[2]
	  
	  # Scaling labels
	  labsize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), input$label_size[1], input$label_size[2])
	  V(g_)$label.cex <- labsize
	  
	  data <- toVisNetworkData(g_)
	  
	  # Set node size
	  if (is.null(input$node_size[1])) min_node = 10.0
	  else min_node = input$node_size[1]
	  
	  if (is.null(input$node_size[2])) max_node = 50.0
	  else max_node = input$node_size[2]
	  
	  nodesize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), min_node, max_node)
	  data$nodes$size = nodesize
	  
	  data$nodes$color.background = case_when(
	    data$nodes$Group == "Male" ~ '#FF6347',
	    data$nodes$Group == "Female" ~ '#ffa500'
	  )
	  
	  data$nodes$color.border = rep("#000000",length(data$nodes$color.background))
	  
	  data$nodes$color.highlight = case_when(
	    data$nodes$Group == "Male" ~ '#47e3ff',
	    data$nodes$Group == "Female" ~ '#005aff'
	  )
	  
	  withProgress(message = 'Creating graph', style = 'notification', value = 0.1, {
	    Sys.sleep(0.25)
	    
	    incProgress(1, detail = paste("Running visnetwork"))
	  
  	  visNetwork(nodes = data$nodes, edges = data$edges)%>%
  	    visNodes(shape = "dot") %>%
  	    visEdges(arrows =list(to = list(enabled = directed)),
  	             color = list(color = "gray",
  	                          highlight = "red")) %>%
  	    visLegend(enabled = TRUE)%>%
  	    visIgraphLayout()%>%
  	    visOptions(highlightNearest = TRUE)
  	  })
  })
}

shinyApp(ui = ui, server = server)
