library(shiny)
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


# Define ----
ui <- fluidPage(
  
  set.seed(123),
  visNetworkOutput(outputId="network", height = 550),
  
  hr(),
  
  fluidRow(
    column(3,offset = 1,
           h4("Network Ties"),
           br(),
           # Selection of the edges that will appear in the relation network 
           checkboxGroupInput("edges_select",
                              label = "Tie Type:",
                              choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3),
                              selected = 1,
                              inline = FALSE)
    ),
    column(4,
           # Egonet configuration
           h4("Egos"),
           br(),
           # Node selection
           uiOutput(outputId = "node_sel"),
           # Order
           sliderInput(inputId = "order", label = "Order", 1, 4, 2, step = 1)
    ),
    column(4,
           # Node and Label Size
           h4("Appearance"),
           br(),
           sliderInput(inputId = "label_size", label = "Label Size", min = 0.3, max = 1.0, value = c(0.4, 0.9),ticks = FALSE),
           br(),
           sliderInput(inputId = "node_size", label = "Node Size", min = 3.0, max = 30.0, value = c(8.0, 20.0),ticks = FALSE),
           hr(),
           helpText("These sliders set the range for node sizes and label sizes according to the degree of each node")
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
	  
	  if (!is.null(input$node_sel) && !is.null(input$order)) {
	    
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
	    
	    # Egonet
	    d <- make_ego_graph(g_,
	                        order = input$order,
	                        nodes = input$node_sel,
	                        mode = c("all"),
	                        mindist = 0)
	    
	    #Set label size
	    if (is.null(input$label_size[1])) min_label = 0.3
	    else min_label = input$label_size[1]
	    
	    if (is.null(input$label_size[2])) max_label = 1.0
	    else max_label = input$label_size[2]
	    
	    labsize <- rescale(degree(d[[1]]), min(degree(d[[1]])), max(degree(d[[1]])), min_label, max_label)
	    V(d[[1]])$label.cex <- labsize
	    
	    #subTitle = paste0("Egonet of Variable ", ego_node, ", ",subTitle)
	    
	    data <- toVisNetworkData(d[[1]])
	    
	    # Set node size
	    if (is.null(input$node_size[1])) min_node = 3.0
	    else min_node = input$node_size[1]
	    
	    if (is.null(input$node_size[2])) max_node = 30.0
	    else max_node = input$node_size[2]
	    
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
	                         arrows =c("to", FALSE, FALSE))
	    
	    withProgress(message = 'Creating graph', style = 'notification', value = 0.1, {
	      Sys.sleep(0.25)
	      
	      incProgress(1, detail = paste("Running visnetwork"))
	    
  	    visNetwork(nodes = data$nodes, edges = data$edges)%>%
  	      visNodes(shape = "dot") %>%
  	      visEdges(arrows =list(to = list(enabled = directed)),
  	               color = list(color = "gray",
  	                            highlight = "red")) %>%
  	      visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = 0.15)%>%
  	      visIgraphLayout()%>%
  	      visOptions(highlightNearest = TRUE)
  	    })
	  }
	})
	
	# This function creates the selectInput object dynamically. 
	# The options for nodes in the list depends on the edges selected
	output$node_sel <- renderUI({
	  
	  validate(
	    need(try(!is.null(input$edges_select)), "Select at least one Tie")
	  )
	  
	  if (length(input$edges_select) == 1){
	    if (input$edges_select[1] == 1){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is teacher of")]),
	                    as.character(edges$Target[which(edges$Relation == "is teacher of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	    if (input$edges_select[1] == 2){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is friend of")]),
	                    as.character(edges$Target[which(edges$Relation == "is friend of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	    if (input$edges_select[1] == 3){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is family of")]),
	                    as.character(edges$Target[which(edges$Relation == "is family of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	  }
	  if (length(input$edges_select) == 2){
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is friend of" | edges$Relation == "is teacher of")]),
	                    as.character(edges$Target[which(edges$Relation == "is friend of" | edges$Relation == "is teacher of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	    if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is friend of" | edges$Relation == "is family of")]),
	                    as.character(edges$Target[which(edges$Relation == "is friend of" | edges$Relation == "is family of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is family of" | edges$Relation == "is teacher of")]),
	                    as.character(edges$Target[which(edges$Relation == "is family of" | edges$Relation == "is teacher of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	  }
	  if (length(input$edges_select) == 3){
	    if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
	      all_nodes = c(as.character(edges$Source[which(edges$Relation == "is friend of" | edges$Relation == "is family of" | edges$Relation == "is teacher of")]),
	                    as.character(edges$Target[which(edges$Relation == "is friend of" | edges$Relation == "is family of" | edges$Relation == "is teacher of")])
	      )
	      all_nodes = sort(unique(all_nodes))
	    }
	  }
	  if (!is.null(input$node_sel)) selectInput(inputId = "node_sel", label = "Node", choices = all_nodes, selected = input$node_sel)
	  else selectInput(inputId = "node_sel", label = "Node", choices = all_nodes, selected = "Plato")
	})
}

shinyApp(ui = ui, server = server)
