library(shinydashboard)
library(shiny)
library(ggplot2)
library(reshape2)


ui <- dashboardPage(
    dashboardHeader(title = "Game of Life",
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = "Click to add a living cell"
                                 ),
                                 notificationItem(
                                   text = "Double click to kill a cell"
                                 ),
                                 notificationItem(
                                   text = "Grey field means no cells left"
                                 )
                    )),
    dashboardSidebar(
      sliderInput("side", "Living space:", 10, 100, 20),
      sliderInput("pxls", "Graph size:", 200, 1000, 500, step = 50),
      sliderInput("ticker", "Animation dealy, ms:", 50, 2000, 500, step = 50),
      sliderInput("rnd", "Probability of life:", 0.01, 0.99, 0.4),
      actionButton("random", "Randomize field", width = '200px'),
      actionButton("clear", "Clear field", width = '200px'),
      br(),
      actionButton("start", "Start sequence", width = '200px'),
      actionButton("stop", "Stop sequence", width = '200px')
    ),
    dashboardBody(
      box(width = 12,
          plotOutput("graph",  height = "auto",
                     click = "plot_click",
                     dblclick = dblclickOpts(id = "plot_dblclick")
          )
      )
      
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
    field = matrix(nrow = input$side, ncol = input$side)
    field[] = rbinom(input$side ^ 2, 1, input$rnd)
    store$field = field
  })
  
  observeEvent(input$start, {  
    store$status = 1
  })
  
  observeEvent(input$stop, {  
    store$status = NULL
  })
  
  observeEvent(input$clear, {  
    field = matrix(nrow = input$side, ncol = input$side)
    field[] = 0
    store$field = field
  })
  
  
  #Shifting field functions to calculate neighbors
  east  = function (k) { cbind(k[, nrow(k)] , k[, -nrow(k)]) }
  west  = function (k) { cbind(k[, -1], k[,1]) }
  south = function (k) { rbind(k[nrow(k), ], k[-nrow(k), ]) }
  north = function (k) { rbind(k[-1, ], k[1, ]) }
  
  output$graph = renderPlot({
    
    #timer
    invalidateLater(input$ticker, session)
    
    #Check fro start/stop buttons
    if ( !is.null(isolate(store$status)) ) { 
      
      field = isolate(store$field)
      
      neighbors = west(field) + east(field) + south(field) + north(field) + 
        south(east(field)) + north(east(field)) +
        south(west(field)) + north(west(field))
      
      # Applying rules of GoL
      newfield = field
      newfield[field==0 & neighbors == 3] = 1
      newfield[field==1 & (neighbors < 2 | neighbors > 3)] = 0
      
      store$field = newfield #iterate
      
      if (max(store$field) < 1) { store$status = NULL }
      
    } 
    
    else {
      
      if ( !is.null(input$plot_click) &&
           (ceiling(input$plot_click$x - 0.5) %in% 1:nrow(store$field)) && 
           (ceiling(input$plot_click$y - 0.5) %in% 1:nrow(store$field)) 
      ) {
        store$field[ceiling(input$plot_click$y - 0.5), ceiling(input$plot_click$x - 0.5)] = 1
      }
      
      if ( !is.null(input$plot_dblclick) &&
           (ceiling(input$plot_dblclick$x - 0.5) %in% 1:nrow(store$field)) && 
           (ceiling(input$plot_dblclick$y - 0.5) %in% 1:nrow(store$field)) 
      ) {
        store$field[ceiling(input$plot_dblclick$y - 0.5), ceiling(input$plot_dblclick$x - 0.5)] = 0
      }
      
    }  
    
    longData = melt( if (is.null(store$status)) {store$field} else {isolate(store$field)} )
    
    ggplot(longData, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=value)) + 
      scale_fill_gradient(low="white", high="black") +
      theme_void() + guides(fill=FALSE) + coord_equal(ratio = 1) + theme(
        panel.background = element_rect(fill = "#ECF0F5",
                                        colour = "#ECF0F5") )
  }, height = function() {
    input$pxls
  })
  
}


shinyApp(ui = ui, server = server)