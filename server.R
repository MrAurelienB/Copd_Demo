#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  code_feat <- list()
  
  output$data_contents <- renderUI({
    inFile <<- input$data_file
    if (is.null(inFile))
      return(NULL)
    
    data <<- read.csv(inFile$datapath, header = input$header, sep = ',')
    if( length(data[1,]) == 1 )
      data <<- read.csv(inFile$datapath, header = input$header, sep = ';')
    if( length(data[1,]) == 1 )
      data <<- read.csv(inFile$datapath, header = input$header, sep = '\t')
    
    code_record <- as.list(1:(length(data[,1])))
    names(code_record) <- paste(as.list(rep("Record",length(data[1,]))),as.character(code_record))
    code_feat <<- as.list(1:(length(data[1,])))
    names(code_feat) <<- as.list(colnames(data))
    
    if(input$data_visualize == "Hosp.Rec"){
      radioButtons("patient", label="Choose a patient record",choices=code_record)
    } else if (input$data_visualize == "feat"){
      radioButtons("feature", label="Choose a feature",choices=code_feat)
    }
  })
  
  observeEvent(input$data_file, {
    output$data_stat <- renderPlot({
      index_feat <- strtoi(input$feature)
      index_event <- strtoi(input$event)
      if( input$data_visualize == "feat" ){
        if( length(unique(data[,index_feat])) > 5 ){
          #hist(data[,index_feat],col="lightblue",xlab=colnames(data)[index_feat],main=paste("Histogram of ",colnames(data)[index_feat]))
          m <- ggplot(data, aes(x=data[,index_feat]))+geom_histogram(aes(fill=..count..))
          m <- m + labs(x=colnames(data)[index_feat]) + labs(title=paste("Histogram of",colnames(data)[index_feat]))
          m + scale_fill_gradient("Count",low="blue4",high="lightblue")      
        }else{
          #barplot( table( data[,index_feat] ),col="lightblue",xlab=colnames(data[,strtoi(input$feature)]),ylab=colnames(data)[,strtoi(input$event)])
          #plot( factor(data[,index_feat]) ~ factor(data[,index_event]),col=c("cadetblue2","lightblue"),xlab=colnames(data)[index_feat],ylab=colnames(data)[index_event],main="")
          m <- qplot(factor(data[,index_event]), data=data, geom="bar", fill=factor(data[,index_feat]))
          m <- m + labs(title=paste("Count of '",colnames(data)[index_feat],"' depending on '",colnames(data)[index_event],"'"))
          m <- m + labs(x=colnames(data)[index_event]) + labs(y="count") + labs(fill=colnames(data)[index_feat])
          m
        }
      }
    })
  })
  
  observeEvent(input$data_file, {
    output$data_event_choice <- renderUI({
      selectInput("event",NULL,code_feat)
    })
  })
  
})
