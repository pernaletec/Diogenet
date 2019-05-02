library(shiny)
library(rmarkdown)
library(knitr)
library(rsconnect)
library(igraph)
library(stringi)
library(tidyverse)
library(visNetwork)
library(shinythemes)

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
ui <- fluidPage(theme = shinytheme("sandstone"),
                
  visNetworkOutput(outputId="local_network", height = 550),
  
  hr(),
  
  fluidRow(
    column(3,offset = 1,
           h4("Network Ties"),
           # Selection of the edges that will appear in the relation network 
           checkboxGroupInput("edges_select_local",
                              label = "Tie Type",
                              choices = list("Is teacher of" = 1, 
                                             "Is friend of" = 2, 
                                             "Is family of" = 3, 
                                             "Studied the work of" = 4, 
                                             "Sent letters to" = 5),
                              selected = 1,
                              inline = FALSE)
    ),
    column(4,
           # Egonet configuration
           h4("Egos"),
           # Node selection
           uiOutput(outputId = "node_sel"),
           # Order
           sliderInput(inputId = "order", label = "Order", 1, 4, 2, step = 1)
    ),
    column(4,
           # Node and Label Size
           h4("Appearance"),
           sliderInput(inputId = "label_size", label = "Label Size", min = 0.3, max = 1.0, value = c(0.4, 0.9),ticks = FALSE),
           sliderInput(inputId = "node_size", label = "Node Size", min = 3.0, max = 30.0, value = c(8.0, 20.0),ticks = FALSE),
           hr(),
           helpText("These parameters control de size range of nodes and labels. The minimum size is set to the nodes with the lowest degree, while the maximum size is set to nodes with the highest degree. The same applies for its labels")
           
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # function to rescale node degree
  rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
  
  subset_graph  = function(edges_selected, g){
    
    # Who is the subset g_ ?
    # Depending on how many edges are selected...
    num_edge_sel = length(edges_selected)
    
    for (k in 1:num_edge_sel){
      #... and in the number of each selection
      if (edges_selected[k] == 1) edge = "is teacher of"
      if (edges_selected[k] == 2) edge = "is friend of"
      if (edges_selected[k] == 3) edge = "is family of"
      if (edges_selected[k] == 4) edge = "studied the work of"
      if (edges_selected[k] == 5) edge = "sent letters to"
      
      #...then the subsetting rule is created dynamically
      if (num_edge_sel == 1)  edge_rule = paste0('which(E(g)$Relation == ', '\'', edge,'\'',')')
      if (num_edge_sel > 1  & k == 1) edge_rule = paste0('which(E(g)$Relation == ', '\'', edge, '\'', ' | ')
      if (num_edge_sel > 1  & k > 1 & k < num_edge_sel) edge_rule = paste0(edge_rule, 'E(g)$Relation == ', '\'', edge,'\'', ' | ')
      if (num_edge_sel > 1  & k == num_edge_sel) edge_rule = paste0(edge_rule, 'E(g)$Relation == ', '\'',edge,'\'',')')
    }
    
    # ...and the subgraph is finally created
    # If there is only one edge selected and this edge is the number 1, keep directed edges
    
    g_ <- subgraph.edges(g,eval(parse(text=edge_rule)))
    
    return(g_)
  } 
  
  output$local_network = renderVisNetwork({
    
    validate(
      need(try(!is.null(input$edges_select_local)), "Please select at least one Tie Type")
    )
    
    if (!is.null(input$node_sel) && !is.null(input$order)) {
      
      if(length(input$edges_select_local) == 1 & input$edges_select_local[1] == 1) {
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
      
      # Subsetting according to edges selected
      g_ = subset_graph(input$edges_select_local, g)
      
      # Egonet
      d <- make_ego_graph(g_,
                          order = input$order,
                          nodes = input$node_sel,
                          mode = c("all"),
                          mindist = 0)
      
      #Set label size
      if (is.null(input$label_size[1])) min_label = 0.4
      else min_label = input$label_size[1]
      
      if (is.null(input$label_size[2])) max_label = 0.9
      else max_label = input$label_size[2]
      
      labsize <- rescale(degree(d[[1]]), min(degree(d[[1]])), max(degree(d[[1]])), min_label, max_label)
      V(d[[1]])$label.cex <- labsize
      
      #subTitle = paste0("Egonet of Variable ", ego_node, ", ",subTitle)
      
      data <- toVisNetworkData(d[[1]])
      
      # Set node size
      if (is.null(input$node_size[1])) min_node = 8.0
      else min_node = input$node_size[1]
      
      if (is.null(input$node_size[2])) max_node = 20.0
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
        data$edges$Relation == "is family of" ~ '#FF0000', 
        data$edges$Relation == "studied the work of" ~ '#ff01d9',
        data$edges$Relation == "sent letters to" ~ '#ffbf00'
      )
      
      data$edges$color.highlight = case_when(
        data$edges$Relation == "is teacher of" ~ '#00ffff',
        data$edges$Relation == "is friend of" ~ '#568b22',
        data$edges$Relation == "is family of" ~ '#ff4000',
        data$edges$Relation == "studied the work of" ~ '#d9ff01',
        data$edges$Relation == "sent letters to" ~ '#8400ff'
      )
      
      # nodes data.frame for legend
      lnodes <- data.frame(label = c("Male", "Female"),
                           shape = c( "dot"), 
                           color = c("#FF6347", "#ffa500"),
                           id = 1:2)
      
      if (directed) arrow = as.character("to") else arrow = as.logical(FALSE)
      
      # edges data.frame for legend
      ledges <- data.frame(color = c("#0000FF", "#228B22", "#FF0000", '#ff01d9','#ffbf00'),
                           label = c("is teacher of", "is friend of", "is family of", "studied the work of", "sent letters to"), 
                           arrows =c("to", FALSE, FALSE, FALSE, FALSE), 
                           font.align = "bottom")
      
      # Shows the name when hovering over the node
      data$nodes$title = paste0("<b>",data$nodes$id,"</b>")
      
      # Shows the relation when hovering over the edge
      data$edges$title = paste0("<i>",data$edges$Relation,"</i>")
      
      set.seed(123)
      
      withProgress(message = 'Creating graph', style = 'notification', value = 0.1, {
        Sys.sleep(0.25)
        
        incProgress(1, detail = paste("Running visnetwork"))
        
        visNetwork(nodes = data$nodes, edges = data$edges)%>%
          visNodes(shape = "dot") %>%
          visEdges(arrows =list(to = list(enabled = directed)),
                   color = list(color = "gray",
                                highlight = "red")) %>%
          visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = 0.15, zoom = FALSE)%>%
          visIgraphLayout()%>%
          visOptions(highlightNearest = TRUE)
      })
    }
  })
  
  # This function creates the selectInput object dynamically. 
  # The options for nodes in the list depends on the edges selected
  output$node_sel <- renderUI({
    
    validate(
      need(try(!is.null(input$edges_select_local)), "Select at least one Tie")
    )
    
    all_nodes = c()
    
    if (1 %in% input$edges_select_local){
      all_nodes = c(all_nodes, 
                    as.character(edges$Source[which(edges$Relation == "is teacher of")]),
                    as.character(edges$Target[which(edges$Relation == "is teacher of")]))
    }
    
    if (2 %in% input$edges_select_local){
      #browser()
      all_nodes = c(all_nodes, 
                    as.character(edges$Source[which(edges$Relation == "is friend of")]),
                    as.character(edges$Target[which(edges$Relation == "is friend of")]))
    }
    
    if (3 %in% input$edges_select_local){
      all_nodes = c(all_nodes, 
                    as.character(edges$Source[which(edges$Relation == "is family of")]),
                    as.character(edges$Target[which(edges$Relation == "is family of")]))
    }
    
    if (4 %in% input$edges_select_local){
      all_nodes = c(all_nodes, 
                    as.character(edges$Source[which(edges$Relation == "studied the work of")]),
                    as.character(edges$Target[which(edges$Relation == "studied the work of")]))
    }
    
    if (5 %in% input$edges_select_local){
      all_nodes = c(all_nodes, 
                    as.character(edges$Source[which(edges$Relation == "sent letters to")]),
                    as.character(edges$Target[which(edges$Relation == "sent letters to")]))
    }
    
    all_nodes = sort(unique(all_nodes))
    
    if (!is.null(input$node_sel)) selectInput(inputId = "node_sel", label = "Node", choices = all_nodes, selected = input$node_sel)
    else selectInput(inputId = "node_sel", label = "Node", choices = all_nodes, selected = "Plato")
  })
}

shinyApp(ui = ui, server = server)