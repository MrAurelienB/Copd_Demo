#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  data <- data.frame(0,0)
  
  output$data_contents <- renderUI({
    inFile <- input$data_file
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    choices <- c()
    if(input$data_visualize == "Hosp.Rec"){
      for( i in 1:(length(data[,1])) ){choices<-c(choices,(paste("record ",i)))}
      radioButtons("patient", label="Choose a patient record",choices=choices)
    } else if (input$data_visualize == "feat"){
      for( i in 1:(length(data[1,])) ){choices<-c(choices,(colnames(data)[i]=i))}
      radioButtons("feature", label="Choose a feature",choices=choices)
    }
    
  })
  
  output$data_stat <- renderPlot({
    if( input$data_visualize == "feat" ){
      #plot( data[,c("age")] )
      #plot( data[,strtoi(input$feature)] )
      plot( data[,18] )
      #input$feature
    }
  })
  
})
