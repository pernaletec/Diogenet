
library(shiny)
library(stringi)
library(igraph)
library(dplyr)

# Egograph igraph

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
    
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h3("Configuration of the network"),
      p("Please configure the network using the parameters below:"),
	  # Selection of the edges that will appear in the relation network 
	  checkboxGroupInput("edges_select",
						 label = "Select the Edges",
						 choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3),
						 selected = 1,
						 inline = FALSE),
	  # Node selection
	  selectInput(inputId = "node_sel", label = "Node selection", choices = nodes$Name),
	  # Order
	  sliderInput(inputId = "order", label = "Order", 1, 4, 2, step = 1),
	  # Plot Size
	  sliderInput(inputId = "plot_height", label = "Plot Size (px)", 500, 800, 600, step = 30),
	  # Label Size
	  sliderInput(inputId = "label_size", label = "Label Size", min = 0.05, max = 2, value = c(0.1, 1)),
	  # Node Size
	  sliderInput(inputId = "node_size", label = "Node Size", min = 1, max = 30, value = c(3, 15))
      ),
    
    mainPanel(
      h3("Using igraph",align="left"),
      p("Here you are with this amazing network..."),
	    plotOutput(outputId="network",height = 550)
      )
  )
)

# Define server logic ----
server <- function(input, output) {
  
	 # function to rescale node degree
	rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
	
	heightVal <- reactive({
	  return(as.numeric(input$plot_height))
	})

	output$network = renderPlot({
	  			 		  
		if(is.null(input$edges_select)) stop("Please select at least 1 edge") 
		  
		if(length(input$edges_select) == 1 & input$edges_select[1] == 1) {
		  g = graph_from_data_frame(d = edges, directed=TRUE, vertices = nodes)
		}else {
		  g = graph_from_data_frame(d = edges, directed=FALSE, vertices = nodes)
		  }

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
			subTitle = "Ties: Teacher"
		  }
		  if (input$edges_select[1] == 2){
			g_ <- subgraph.edges(g,
								 which(E(g)$Relation=="is friend of"))
			subTitle = "Ties: Friends"
		  }  
		  if (input$edges_select[1] == 3){
		  g_ <- subgraph.edges(g,
							   which(E(g)$Relation=="is family of"))
		  subTitle = "Ties: Family"
		  }
		}
		if (length(input$edges_select) == 2){
		  if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
			g_ <- subgraph.edges(g,
							   which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of"))
			subTitle = "Ties: Teacher, Friend"
		  }  
		  if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
			g_ <- subgraph.edges(g,
								 which(E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
			subTitle = "Ties: Friends, Family"
		  }
		  if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
			g_ <- subgraph.edges(g,
								 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is family of"))
			subTitle = "Ties: Teacher, Family"
		  }  
		}
		if (length(input$edges_select) == 3){
		  if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
			g_ <- subgraph.edges(g,
								 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
			subTitle = "Ties: Teacher, Friends, Family"
		  }  
		}
		
		# Egonets. USERS IN SHINY SHOULD BE ABLE TO CREATE AN EGOCENTRIC NETWORK.
		# VARIABLES TO CHOOSE WOULD BE ORDER AND NODE. ONLY ONE NODE IS ACCEPTABLE. 
		# IN THIS EXAMPLE ORDER =2, NODE = ARISTOTLE. 
		# ORDER SHOULD NOT BE GREATER THAN 4. MINIMUM VALUE OF ORDER IS 1
		
		node_selected = toString(input$node_sel)
		
		d <- make_ego_graph(g_,
		                    input$order, 
		                    "Aristotle", 
		                    mode = c("all"), 
		                    mindist = 0)
		
		subTitle = paste0("Egonet of Variable ", input$node_sel, ", ",subTitle)
		
		plot(d[[1]], 
		     main = subTitle)
		
		},height = heightVal)

}

shinyApp(ui = ui, server = server)