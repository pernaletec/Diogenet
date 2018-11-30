

library(shiny)
library(rmarkdown)
library(knitr)
library(rsconnect)
library(igraph)
library(stringi)
library(tidyverse)
library(visNetwork)

# Community igraph

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
                              choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3),
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
=======
	 # function to rescale node degree
	rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
	heightVal <- reactive({
	  return(as.numeric(input$plot_height))
	})
	
	network <- reactive({
	
  	  output$network = renderPlot({
  	  			 		  
	    if(is.null(input$edges_select)) stop("Please select at least 1 edge") 
  	    
  	  	g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)
	    
  		## 'Male', 'Female' y 'Geography' deben tener colores distintos
  		V(g)$color <- case_when(
  		  V(g)$Group == "Male" ~ 'red',
  		  V(g)$Group == "Female" ~ 'orange',
  		  V(g)$Group ==  "Place" ~ 'blue'
  		)

  
  heightVal <- reactive({
    return(as.numeric(input$plot_height))
  })
  
  network <- reactive({
    
    output$network = renderPlot({
      
      if(is.null(input$edges_select)) stop("Please select at least 1 edge") 
      
      g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)
      
      ## 'Male', 'Female' y 'Geography' deben tener colores distintos
      V(g)$color <- case_when(
        V(g)$Group == "Male" ~ 'red',
        V(g)$Group == "Female" ~ 'orange',
        V(g)$Group ==  "Place" ~ 'blue'
      )
      
      # Es necesario definir una columna de id para exportar correctamente al formato Pajek
      V(g)$id = V(g)$name
      
      # Scale node label size (min deg., max deg., min size, max size)
      labsize <- rescale(degree(g), min(degree(g)), max(degree(g)), input$label_size[1], input$label_size[2])
      V(g)$label.cex <- labsize
      
      # Scale node size (min deg., max deg., min size, max size)
      deg <- rescale(degree(g), min(degree(g)), max(degree(g)), input$node_size[1], input$node_size[2])
      V(g)$size <- deg
      
      if (length(input$edges_select) == 1){
        if (input$edges_select[1] == 1){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation == "is teacher of"))
          subTitle = "Type of ties: Teacher"
        }
        if (input$edges_select[1] == 2){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation=="is friend of"))
          subTitle = "Type of ties: Friends"
        }  
        if (input$edges_select[1] == 3){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation=="is family of"))
          subTitle = "Type of ties: Family"
        }
      }
      if (length(input$edges_select) == 2){
        if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of"))
          subTitle = "Type of ties: Teacher, Friend"
        }  
        if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
          subTitle = "Type of ties: Friends, Family"
        }
        if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is family of"))
          subTitle = "Type of ties: Teacher, Family"
        }  
      }
      if (length(input$edges_select) == 3){
        if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
          g_ <- subgraph.edges(g,
                               which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
          subTitle = "Type of ties: Teacher, Friends, Family"
        }  
      }
      
      # Community detection
      # HERE THE USER SHOULD BE ABLE TO CHOOSE BETWEEN RELATIONS
      # AT THE TIME OF RUNNING THE COMMUNITY DETECTION ALGORITHM
      ### ALGORITHM TO BE CHOSEN BY THE USER: cluster leading eigen, cluster fast greedy, cluster louvain
      ## Teachers, friends and family
      ### Modularity should be shown in the subtitle of the graph
      
      if (input$cmnt_dtc_alg == "Cluster Leading Eigen") cluster = cluster_leading_eigen(g_)
      if (input$cmnt_dtc_alg == "Cluster Fast Greedy") cluster = cluster_fast_greedy(g_)
      if (input$cmnt_dtc_alg == "Cluster Louvain") cluster = cluster_louvain(g_)
      
      mod <- round(modularity(cluster),3)
      subTitle = paste0("MODULARITY: ", mod)
      
      plot(cluster,
           g_,
           main = subTitle)
    })
    
    plotOutput('network', height = heightVal())
    
  })
  
  output$networkUI <- renderUI({
    network()
  })
  
  		if (length(input$edges_select) == 1){
  		  if (input$edges_select[1] == 1){
  			g_ <- subgraph.edges(g,
  							   which(E(g)$Relation == "is teacher of"))
  			subTitle = "Type of ties: Teacher"
  		  }
  		  if (input$edges_select[1] == 2){
  			g_ <- subgraph.edges(g,
  								 which(E(g)$Relation=="is friend of"))
  			subTitle = "Type of ties: Friends"
  		  }  
  		  if (input$edges_select[1] == 3){
  		  g_ <- subgraph.edges(g,
  							   which(E(g)$Relation=="is family of"))
  		  subTitle = "Type of ties: Family"
  		  }
  		}
  		if (length(input$edges_select) == 2){
  		  if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
  			g_ <- subgraph.edges(g,
  							   which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of"))
  			subTitle = "Type of ties: Teacher, Friend"
  		  }  
  		  if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
  			g_ <- subgraph.edges(g,
  								 which(E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
  			subTitle = "Type of ties: Friends, Family"
  		  }
  		  if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
  			g_ <- subgraph.edges(g,
  								 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is family of"))
  			subTitle = "Type of ties: Teacher, Family"
  		  }  
  		}
  		if (length(input$edges_select) == 3){
  		  if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
  			g_ <- subgraph.edges(g,
  								 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
  			subTitle = "Type of ties: Teacher, Friends, Family"
  		  }  
  		}
  		
  		# Community detection
  		# HERE THE USER SHOULD BE ABLE TO CHOOSE BETWEEN RELATIONS
  		# AT THE TIME OF RUNNING THE COMMUNITY DETECTION ALGORITHM
  		### ALGORITHM TO BE CHOSEN BY THE USER: cluster leading eigen, cluster fast greedy, cluster louvain
  		## Teachers, friends and family
  		### Modularity should be shown in the subtitle of the graph
  		
  		if (input$cmnt_dtc_alg == "Cluster Leading Eigen") cluster = cluster_leading_eigen(g_)
  		if (input$cmnt_dtc_alg == "Cluster Fast Greedy") cluster = cluster_fast_greedy(g_)
  		if (input$cmnt_dtc_alg == "Cluster Louvain") cluster = cluster_louvain(g_)
  		
  		mod <- round(modularity(cluster),3)
  		subTitle = paste0("MODULARITY: ", mod)
  				
  		plot(cluster,
  		     g_,
  		     main = subTitle)
  		})
  	  
  	  plotOutput('network', height = heightVal())
	
	})
	
	output$networkUI <- renderUI({
	  network()
	})

}

shinyApp(ui = ui, server = server)
