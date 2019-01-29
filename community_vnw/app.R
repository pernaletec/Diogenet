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
  uiOutput('networkUI'),
  
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
           sliderInput(inputId = "label_size", label = "Label Size", min = 0.2, max = 2.0, value = c(0.6, 1.5),ticks = FALSE),
           # Node Size
           sliderInput(inputId = "node_size", label = "Node Size", min = 3, max = 30, value = c(8, 20),ticks = FALSE)
    )
  )
)

# Define server logic ----
server <- function(input, output) {
 
  # function to rescale node degree
  rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
	 # function to rescale node degree
	rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
	heightVal <- reactive({
	  return(as.numeric(input$plot_height))
	})
	
	network <- reactive({
	  
  	  output$network = renderPlot({

  	    validate(
  	      need(try(!is.null(input$edges_select)), "Please select at least one Tie Type")
  	    )

  	  	g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)

        # Es necesario definir una columna de id para exportar correctamente al formato Pajek
        V(g)$id = V(g)$name
  
        # Scale node label size (min deg., max deg., min size, max size)
        labsize <- rescale(degree(g), min(degree(g)), max(degree(g)), input$label_size[1], input$label_size[2])
        V(g)$label.cex <- labsize
  
        # Scale node size (min deg., max deg., min size, max size)
        deg <- rescale(degree(g), min(degree(g)), max(degree(g)), input$node_size[1], input$node_size[2])
        V(g)$size <- deg
        
        num_edge_sel = length(input$edges_select)
        
        for (k in 1:num_edge_sel){
          
          if (input$edges_select[k] == 1) edge = "is teacher of"
          if (input$edges_select[k] == 2) edge = "is friend of"
          if (input$edges_select[k] == 3) edge = "is family of"
          if (input$edges_select[k] == 4) edge = "studied the work of"
          
          if (num_edge_sel == 1)  edge_rule = str_c('which(E(g)$Relation == ', '\'', edge,'\'',')')
          if (num_edge_sel > 1  & k == 1) edge_rule = paste0('which(E(g)$Relation == ', '\'', edge, '\'', ' | ')
          if (num_edge_sel > 1  & k > 1 & k < num_edge_sel) edge_rule = paste0(edge_rule, 'E(g)$Relation == ', '\'', edge,'\'', ' | ')
          if (num_edge_sel > 1  & k == num_edge_sel) edge_rule = paste0(edge_rule, 'E(g)$Relation == ', '\'',edge,'\'',')')
        }
        
        g_ <- subgraph.edges(g,eval(parse(text=edge_rule)))
      
    		# Community detection
    		# HERE THE USER SHOULD BE ABLE TO CHOOSE BETWEEN RELATIONS
    		# AT THE TIME OF RUNNING THE COMMUNITY DETECTION ALGORITHM
    		### ALGORITHM TO BE CHOSEN BY THE USER: cluster leading eigen, cluster fast greedy, cluster louvain
    		## Teachers, friends and family
    		### Modularity should be shown in the subtitle of the graph
    		
    		if (input$cmnt_dtc_alg == "Cluster Leading Eigen") cluster = cluster_leading_eigen(g_)
    		if (input$cmnt_dtc_alg == "Cluster Fast Greedy") cluster = cluster_fast_greedy(g_)
    		if (input$cmnt_dtc_alg == "Cluster Louvain") cluster = cluster_louvain(g_)
        
        withProgress(message = 'Creating graph', style = 'notification', value = 0.1, {
          Sys.sleep(0.25)
          incProgress(0.6, detail = paste("Running igraph"))
    		
      		mod <- round(modularity(cluster),3)
      		subTitle = paste0("MODULARITY: ", mod)
        		return(plot(cluster,
        		     g_,
        		     main = subTitle))
        })
  	  })
  	  plotOutput('network', height = heightVal())
	})
	
	output$networkUI <- renderUI({
	  network()
	})

}

shinyApp(ui = ui, server = server)
