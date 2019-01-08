library(shinydashboard)
library(shiny)
library(ggplot2)
library(reshape2)


ui <- dashboardPage(
  dashboardHeader(title = "Game of Life"                  ),
  dashboardSidebar(
    sliderInput("side", "Field size:", 10, 100, 20),
    sliderInput("rnd", "Probability of life:", 0.01, 0.99, 0.4),
    sliderInput("ticker", "Animation dealy, ms:", 50, 2000, 500),
    actionButton("random", "Randomize field", width = '200px'),
    actionButton("start", "Start sequence", width = '200px'),
    actionButton("stop", "Stop sequence", width = '200px')
  ),
  dashboardBody(
      plotOutput("graph", width = 700, height = 700)
      )
    )

server = function(input, output, session) {
  
  store = reactiveValues(
      #default field
      field =   matrix(c(1,1,1,1,1,1,1,1,1,
                         1,0,0,0,0,0,0,0,1,
                         1,0,1,1,1,1,1,0,1,
                         1,0,1,0,0,0,1,0,1,
                         1,0,1,0,1,0,1,0,1,
                         1,0,1,0,0,0,1,0,1,
                         1,0,1,1,1,1,1,0,1,
                         1,0,0,0,0,0,0,0,1,
                         1,1,1,1,1,1,1,1,1
                         ), nrow=9, ncol=9, byrow = TRUE),
      status = NULL
    );
  
  #Randomize field
  observeEvent(input$random, {
    field <- matrix(nrow = input$side, ncol = input$side)
    field[] <- rbinom(input$side ^ 2, 1, input$rnd)
    store$field = field
  })
  
  observeEvent(input$start, {  
    store$status = 1
  })
  
  observeEvent(input$stop, {  
    store$status = NULL
  })
  
  #Shifting field functions to calculate neighbors
  east = function (k) { cbind(k[,nrow(k)] , k[,-nrow(k)]) }
  west = function (k) { cbind(k[,-1], k[,1]) }
  south = function (k) { rbind(k[nrow(k),], k[-nrow(k),]) }
  north = function (k) { rbind(k[-1,], k[1,]) }
  
  output$graph = renderPlot({
    
    #timer
    invalidateLater(input$ticker, session)
        
        #Check for start/stop buttons
        if ( !is.null(isolate(store$status)) ) { 
          
          field = isolate(store$field)
          
          neighbors = west(field) + east(field) + south(field) + north(field) + 
            south(east(field)) + north(east(field)) +
            south(west(field)) + north(west(field))
          
          # Applying rules of GoL
          newfield = field
          newfield[field==0 & neighbors==3] = 1
          newfield[field==1 & neighbors<2] = 0
          newfield[field==1 & neighbors>3] = 0
          
          #stop if all dead
          if (max(newfield != 0)) {
            store$field = newfield
          }
        }
    
    longData<-melt(isolate(store$field))
    
    ggplot(longData, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=value)) + 
      scale_fill_gradient(low="white", high="black") +
      theme_void() + guides(fill=FALSE)
  })
}


shinyApp(ui = ui, server = server)
