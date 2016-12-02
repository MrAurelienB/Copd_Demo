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
library(survival)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  code_feat <- list()
  copd <- NULL
  
  observeEvent({
    input$data_file
    input$patientID},
    {
  
    readFile <- function(){
      copd <<- read.csv(inFile$datapath, header = input$header, sep = ',')
      if( length(copd[1,]) == 1 )
        copd <<- read.csv(inFile$datapath, header = input$header, sep = ';')
      if( length(copd[1,]) == 1 )
        copd <<- read.csv(inFile$datapath, header = input$header, sep = '\t')
      
      copd
    }
    
    output$data_contents <- renderUI({
      inFile <<- input$data_file
      if (is.null(inFile))
        return(NULL)
      
      copd <<- readFile()
      
      code_record <- as.list(1:(length(copd[,1])))
      if( input$patientID ){
        names(code_record) <- as.character(copd[,1])
        copd <<- copd[,-c(1)]
      }else{
        names(code_record) <- paste(as.list(rep("Record",length(copd[1,]))),as.character(code_record))
        copd <<- readFile()
      }
      code_feat <<- as.list(1:(length(copd[1,])))
      names(code_feat) <<- as.list(colnames(copd))
      
      if(input$data_visualize == "Hosp.Rec"){
        selectInput("patient",NULL,choices=code_record)
      } else if (input$data_visualize == "feat"){
        selectInput("feature",NULL,choices=code_feat)
      }
    })
  })
  
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$data_stat <- renderPlot({
      index_feat <- strtoi(input$feature)
      index_event <- strtoi(input$event)
      if( input$data_visualize == "feat" ){
        if( length(unique(copd[,index_feat])) > 5 ){
          m <- ggplot(copd, aes(x=copd[,index_feat]))+geom_histogram(aes(fill=..count..))
          m <- m + labs(x=colnames(copd)[index_feat]) + labs(title=paste("Histogram of",colnames(copd)[index_feat]))
          m + scale_fill_gradient("Count",low="blue4",high="lightblue")      
        }else{
          m <- qplot(as.factor(copd[,index_event]), data=copd, geom="bar", fill=as.factor(copd[,index_feat]))
          m <- m + labs(title=paste("Count of '",colnames(copd)[index_feat],"' depending on '",colnames(copd)[index_event],"'"))
          m <- m + labs(x=colnames(copd)[index_event]) + labs(y="count") + labs(fill=colnames(copd)[index_feat])
          m
        }
      }
    })
  })
  
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$data_event_choice <- renderUI({
      selectInput("event",NULL,choices=code_feat,selected=length(code_feat)-1)
    })
  })
  
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$data_stime_choice <- renderUI({
      selectInput("surv_time",NULL,choices=code_feat,selected=length(code_feat))
    })
  })
  
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$survival_curve <- renderPlot({
      index_time <- strtoi(input$surv_time)
      index_feat <- strtoi(input$feature)
      index_event <- strtoi(input$event)
      if( input$data_visualize == "feat" && length(unique(copd[,index_feat])) < 5 ){
        surv_obj <- Surv(copd[,index_time],copd[,index_event]==1)
        fit <- survfit(surv_obj~copd[,index_feat] , data = copd )
        ggsurvplot(fit,risk.table=TRUE,break.time.by=ceiling(max(copd[,index_time])/5),risk.table.y.text=FALSE,risk.table.height=0.35)
      }
    })
  })
  
})
