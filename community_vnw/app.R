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

# Define ----
ui <- fluidPage(

  # Setting seed is important so the graph is always with the same configuration when starts
  set.seed(123),
  # Use the visnetworkOutput function for visnetwork objects
  visNetworkOutput(outputId="network", height = 550),
  
  hr(),
  
  fluidRow(
    column(3,offset = 1,
           h4("Network relations"),
           br(),
           # Selection of the edges that will appear in the relation network 
           checkboxGroupInput("edges_select",
                              label = "Select the Edges",
                              choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3, "Studied the work of" = 3),
                              selected = 1,
                              inline = FALSE)
    ),
    column(4,
           # Plot Height
           h4("Community detection"),
           br(),
           # Algorithm for community detection
           selectInput("cmnt_dtc_alg", "Algorithm:", 
                       choices=c("Cluster Leading Eigen", "Cluster Fast Greedy", "Cluster Louvain")),
           hr(),
           helpText("Different algorithms for community detection are available")
    ),
    column(4,
           # Plot Height
           h4("Appearance"),
           br(),
           # Plot Size
           sliderInput(inputId = "plot_height", label = "Plot Height (px)", 500, 1200, 600, step = 30,ticks = FALSE,post = "px"),
           # Label Size
           sliderInput(inputId = "label_size", label = "Label Size", min = 0.0, max = 5.0, value = c(1, 4),ticks = FALSE),
           # Node Size
           sliderInput(inputId = "node_size", label = "Node Size", min = 10.0, max = 60.0, value = c(20, 40),ticks = FALSE)
    )
  )
)

# Define server logic ----
server <- function(input, output) {
 
  # function to rescale node degree
  rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
	 # function to rescale node degree
	rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
  output$network = renderVisNetwork({

  	    # First validate if there are edges (Ties) selected
  	    validate(
  	      need(try(!is.null(input$edges_select)), "Please select at least one Tie Type")
  	    )

  	    g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)


  	    # ids in nodes are es required to export to Pajek format. Just in case!
        V(g)$id = V(g)$name
        
        # ids in edges are requiered for visUpdateNodes / visRemoveNodes / visUpdateEdges / visRemoveEdges 
        E(g)$id = seq(1:length(E(g)))
        
        # Who is the subset g_ ?
        # Depending on how many edges are selected...
        num_edge_sel = length(input$edges_select)
        
        for (k in 1:num_edge_sel){
          #... and in the number of each selection
          if (input$edges_select[k] == 1) edge = "is teacher of"
          if (input$edges_select[k] == 2) edge = "is friend of"
          if (input$edges_select[k] == 3) edge = "is family of"
          if (input$edges_select[k] == 4) edge = "studied the work of"
          
          #...then the subsetting rule is created dynamically
          if (num_edge_sel == 1)  edge_rule = paste0('which(E(g)$Relation == ', '\'', edge,'\'',')')
          if (num_edge_sel > 1  & k == 1) edge_rule = paste0('which(E(g)$Relation == ', '\'', edge, '\'', ' | ')
          if (num_edge_sel > 1  & k > 1 & k < num_edge_sel) edge_rule = paste0(edge_rule, 'E(g)$Relation == ', '\'', edge,'\'', ' | ')
          if (num_edge_sel > 1  & k == num_edge_sel) edge_rule = paste0(edge_rule, 'E(g)$Relation == ', '\'',edge,'\'',')')
        }
        
        # ...and the subgraph is finally created
        g_ <- subgraph.edges(g,eval(parse(text=edge_rule)))
      
    		# Community detection
    		
    		if (input$cmnt_dtc_alg == "Cluster Leading Eigen") cluster = cluster_leading_eigen(g_)
    		if (input$cmnt_dtc_alg == "Cluster Fast Greedy") cluster = cluster_fast_greedy(g_)
    		if (input$cmnt_dtc_alg == "Cluster Louvain") cluster = cluster_louvain(g_)
        
        #Set label size
        if (is.null(input$label_size[1])) min_label = 0.0
        else min_label = input$label_size[1]
        
        if (is.null(input$label_size[2])) max_label = 5.0
        else max_label = input$label_size[2]
        
        # Scaling labels
        labsize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), input$label_size[1], input$label_size[2])
        V(g_)$label.cex <- labsize
        
        # Assign membership ids to vertex
        V(g_)$community  <- cluster$membership
        
        data <- toVisNetworkData(g_)
        
        # Set node size
        if (is.null(input$node_size[1])) min_node = 10.0
        else min_node = input$node_size[1]
        
        if (is.null(input$node_size[2])) max_node = 50.0
        else max_node = input$node_size[2]
        
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
          data$edges$Relation == "studied the work of" ~ '#FF0000'
        )
        
        data$edges$color.highlight = case_when(
          data$edges$Relation == "is teacher of" ~ '#00ffff',
          data$edges$Relation == "is friend of" ~ '#568b22',
          data$edges$Relation == "is family of" ~ '#ff4000',
          data$edges$Relation == "studied the work of" ~ '#ff4000'
        )
        
        
        # # nodes data.frame for legend
        # lnodes <- data.frame(label = c("Male", "Female"),
        #                      shape = c( "dot"), 
        #                      color = c("#FF6347", "#ffa500"),
        #                      id = 1:2)
        # 
        # # edges data.frame for legend
        # ledges <- data.frame(color = c("#0000FF", "#228B22", "#FF0000"),
        #                      label = c("is teacher of", "is friend of", "is family of"), 
        #                      arrows =c("to", FALSE, FALSE), 
        #                      font.align = "bottom")
      
        withProgress(message = 'Creating graph', style = 'notification', value = 0.1, {
          Sys.sleep(0.25)
          incProgress(0.6, detail = paste("Running VisNetwork"))
      		mod <- round(modularity(cluster),3)
      		subTitle = paste0("MODULARITY: ", mod)
      		
      		# Visnetwork graph creation
      		visNetwork(nodes = data$nodes, edges = data$edges)%>%
      		  visNodes(shape = "dot") %>%
      		  visEdges(arrows =list(to = list(enabled = FALSE))) %>%
      		  #visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = 0.15, zoom = FALSE)%>%
      		  visIgraphLayout()%>%
      		  visOptions(highlightNearest = TRUE)
      		
        })
  	  })

}

shinyApp(ui = ui, server = server)
