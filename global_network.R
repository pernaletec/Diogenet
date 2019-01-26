# Import node list: contains nodes$Name, nodes$Group
source("source_data.R")

# Modularized funtion that implements the User Interface (UI)
networkUI = function(id){
  
  # Function that creates an "id" namespace
  # Enables to call xxxUI functions many times with no conflict
  ns  = NS(id)
  
  #Object returned by xxxUI function
  
  fluidPage(
    
    sidebarLayout(sidebarPanel(

      selectInput(inputId = ns("pck_sel"), label = "Package selection", choices = c("visNetwork" = "visNetwork","igraph" = "igraph"), selected = "visNetwork"),
      h4("Network Ties"),
      br(),
      # Selection of the edges that will appear in the relation network 
      checkboxGroupInput(ns("edges_select"),
                         label = "Tie Type",
                         choices = list("Is teacher of" = 1, "Is friend of" = 2, "Is family of" = 3),
                         selected = 1,
                         inline = FALSE),
      
      # Plot Height
      h4("Appearence"),
      br(),
      # Nodes/Label sizes
      uiOutput(outputId=ns("nodeUI_vnw")),
      uiOutput(outputId=ns("nodeUI_igp")),
      uiOutput(outputId=ns("labelUI_vnw")),
      uiOutput(outputId=ns("labelUI_igp")),
      # Plot height (igraph case)
      sliderInput(inputId = ns("heightUI"), label = "Plot Height (px)",min =  600,max =  1500,value =  900, step = 30,ticks = FALSE,post = "px")
    
    ),
    mainPanel(
      uiOutput(outputId=ns("graph_vnw")),
      uiOutput(outputId=ns("graph_igp"))
    )
    )
  )
}

# Server logic is modularized in this  function
# The function must include the session parameter
# Forconvenienmce function is called xxx and the user interface function xxxUI
network <- function(input, output, session) {
  
  # function to rescale node degree
  rescale <- function(x,a,b,c,d){c + (x-a)/(b-a)*(d-c)}
  
  heightVal <- reactive({
    return(as.numeric(input$heightUI))
  })
  
  network_vnw = reactive({
    
    ns = session$ns
    
    if  (input$pck_sel == "visNetwork"){
      
      output$network_vnw = renderVisNetwork({
        
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
        
        
        # Scaling labels
        labsize <- rescale(degree(g_), min(degree(g_)), max(degree(g_)), input$labelUI_vnw[1], input$labelUI_vnw[2])
        V(g_)$label.cex <- labsize
        
        data <- toVisNetworkData(g_)
        
        data$nodes$color.background = case_when(
          data$nodes$Group == "Male" ~ '#FF6347',
          data$nodes$Group == "Female" ~ '#ffa500'
        )
        
        data$nodes$color.border = rep("#000000",length(data$nodes$color.background))
        
        data$nodes$color.highlight = case_when(
          data$nodes$Group == "Male" ~ '#47e3ff',
          data$nodes$Group == "Female" ~ '#005aff'
        )
        
        visNetwork(nodes = data$nodes, edges = data$edges)%>%
          visNodes(shape = "dot") %>%
          visEdges(arrows =list(to = list(enabled = directed)),
                   color = list(color = "gray",
                                highlight = "red")) %>%
          visLegend(enabled = TRUE)%>%
          visIgraphLayout()%>%
          visOptions(highlightNearest = TRUE)
      })
      set.seed(123)
      visNetworkOutput(outputId=ns("network_vnw"), height = heightVal())
      
    }
  })
  
  network_igp = reactive({
    
    ns = session$ns
    
    if  (input$pck_sel == "igraph"){
      
      output$network_igp = renderPlot({
        
        validate(
          need(try(!is.null(input$edges_select)), "Please select at least one Tie Type")
        )
        
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
        labsize <- rescale(degree(g), min(degree(g)), max(degree(g)), input$labelUI_igp[1], input$labelUI_igp[2])
        V(g)$label.cex <- labsize
        
        # Scale node size (min deg., max deg., min size, max size)
        deg <- rescale(degree(g), min(degree(g)), max(degree(g)), input$nodeUI_igp[1], input$nodeUI_igp[2])
        V(g)$size <- deg
        
        if (length(input$edges_select) == 1){
          if (input$edges_select[1] == 1){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation == "is teacher of"))
            #subTitle = "Type of ties: Teacher"
          }
          if (input$edges_select[1] == 2){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation=="is friend of"))
            #subTitle = "Type of ties: Friends"
          }  
          if (input$edges_select[1] == 3){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation=="is family of"))
            #subTitle = "Type of ties: Family"
          }
        }
        if (length(input$edges_select) == 2){
          if (input$edges_select[1] == 1 & input$edges_select[2] == 2){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of"))
            #subTitle = "Type of ties: Teacher, Friend"
          }  
          if (input$edges_select[1] == 2 & input$edges_select[2] == 3){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
            #subTitle = "Type of ties: Friends, Family"
          }
          if (input$edges_select[1] == 1 & input$edges_select[2] == 3){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is family of"))
            #subTitle = "Type of ties: Teacher, Family"
          }  
        }
        if (length(input$edges_select) == 3){
          if (input$edges_select[1] == 1 & input$edges_select[2] == 2 & input$edges_select[3] == 3){
            g_ <- subgraph.edges(g,
                                 which(E(g)$Relation=="is teacher of" | E(g)$Relation=="is friend of" | E(g)$Relation=="is family of"))
            #subTitle = "Type of ties: Teacher, Friends, Family"
          }  
        }
        set.seed(123)
        plot(g_)
      })
      plotOutput(ns("network_igp"), height = heightVal())
    }
  })
  
  output$labelUI_vnw <- renderUI({
    ns = session$ns
    if (input$pck_sel == "visNetwork") {
      if (!is.null(input$labelUI_vnw)) sliderInput(inputId = ns("labelUI_vnw"), label = "Label Size", min = 0.0, max = 5.0, value = c(input$labelUI_vnw[1], input$labelUI_vnw[2]),ticks = FALSE)
      else sliderInput(inputId = ns("labelUI_vnw"), label = "Label Size", min = 0.0, max = 5.0, value = c(1.0, 3.0),ticks = FALSE)
    } 
  })
  
  output$nodeUI_vnw <- renderUI({
    ns = session$ns
    if (input$pck_sel == "visNetwork") {
      if (!is.null(input$nodeUI_vnw)) sliderInput(inputId = ns("nodeUI_vnw"), label = "Node Size", min = 10, max = 60, value = c(input$nodeUI_vnw[1], input$nodeUI_vnw[2]),ticks = FALSE)
      else sliderInput(inputId = ns("nodeUI_vnw"), label = "Node Size", min = 10, max = 60, value = c(20, 40),ticks = FALSE)
    }
  })
  
  output$labelUI_igp <- renderUI({
    ns = session$ns
    if (input$pck_sel == "igraph") {
      if (!is.null(input$labelUI_igp)) sliderInput(inputId = ns("labelUI_igp"), label = "Label Size", min = 0.0, max = 3.0, value = c(input$labelUI_igp[1], input$labelUI_igp[2]),ticks = FALSE)
      else sliderInput(inputId = ns("labelUI_igp"), label = "Label Size", min = 0.0, max = 3.0, value = c(1.0, 2.0),ticks = FALSE)
    } 
  })
  
  
  output$nodeUI_igp <- renderUI({
    ns = session$ns
    if (input$pck_sel == "igraph") {
      if (!is.null(input$nodeUI_igp)) sliderInput(inputId = ns("nodeUI_igp"), label = "Node Size", min = 1, max = 30, value = c(input$nodeUI_igp[1], input$nodeUI_igp[2]),ticks = FALSE)
      else sliderInput(inputId = ns("nodeUI_igp"), label = "Node Size", min = 1, max = 30, value = c(3, 15),ticks = FALSE)
    }
  })
  
  output$graph_vnw <- renderUI({
    network_vnw()
  })
  
  output$graph_igp <- renderUI({
    network_igp()
  })
  
}
