#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram

library(shiny)
library(shinythemes)
library(survival)
library(survminer)
library(ggplot2)
library(GGally)
library(plotly)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  #---contains the list of features
  code_feat <- list()
  
  #---contains the list of patients
  code_record <- list()
  
  #---contains the data matrix
  copd <- NULL
  
  #---contain the cox-model prediction containing the risks score
  cox_prediction <- NULL
  
  #---contains the cox-model
  mod.cox <- NULL
  
  observeEvent({
    input$data_file
    input$patientID},
    {
  
    #---read the data file
    readFile <- function(){
      copd <<- read.csv(inFile$datapath, header = input$header, sep = ',')
      if( length(copd[1,]) == 1 )
        copd <<- read.csv(inFile$datapath, header = input$header, sep = ';')
      if( length(copd[1,]) == 1 )
        copd <<- read.csv(inFile$datapath, header = input$header, sep = '\t')
      
      copd
    }
    
    #---create UI depending on data file
    output$data_contents <- renderUI({
      inFile <<- input$data_file
      if (is.null(inFile))
        return(NULL)
      
      copd <<- readFile()
      
      code_record <<- as.list(1:(length(copd[,1])))
      if( input$patientID ){
        names(code_record) <<- as.character(copd[,1])
        copd <<- copd[,-c(1)]
      }else{
        names(code_record) <<- paste(as.list(rep("Record",length(copd[1,]))),as.character(code_record))
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
  
  #---barplot to visualize data depending on 'event of interest'
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$data_stat <- renderPlotly({
      inFile <<- input$data_file
      if (is.null(inFile))
        return(NULL)
      
      index_feat <- strtoi(input$feature)
      index_event <- strtoi(input$event)
      if( input$data_visualize == "feat" ){
        if( length(unique(copd[,index_feat])) > 5 ){
          #---plot the histogram
          m <- ggplot(copd,aes(x=copd[,index_feat],fill=copd[,index_feat]))+geom_histogram( )
          m <- m + labs(x=colnames(copd)[index_feat]) + labs(title=paste("Histogram of",colnames(copd)[index_feat]))
          m <- m + scale_fill_gradient("Count",low="blue4",high="lightblue")
          ggplotly(m,tooltip=c("count"))
        }else{
          #---plot a barplot
          x <- as.factor(copd[,index_event])
          y <- as.factor(copd[,index_feat])
          count <- as.vector(table(y,x))
          xx <- c()
          for( i in 1:(length(levels(x))) ){
            xx <- c( xx , rep(levels(x)[i],length(levels(y))) )
          }
          yy <- rep(levels(y),length(levels(x)))
          dat <- data.frame(
            x = xx,
            value = count,
            y = yy
          )
          m <- ggplot(data=dat, aes(x=xx, y=count, fill=yy))
          m <- m + geom_bar(stat="identity", position=position_dodge())
          m <- m + labs(title=paste("Count of '",colnames(copd)[index_feat],"' depending on '",colnames(copd)[index_event],"'"))
          m <- m + labs(x=colnames(copd)[index_event]) + labs(y="count") + labs(fill=colnames(copd)[index_feat])
          ggplotly(m,tooltip=c("y"))
        }
      }
    })
  })
  
  #---select input to choose the event of interest among the features
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$data_event_choice <- renderUI({
      selectInput("event",NULL,choices=code_feat,selected=length(code_feat)-1)
    })
  })
  
  #---select input to choose the survival time among the features
  observeEvent({
    input$data_file
    input$patientID
    },
    {output$data_stime_choice <- renderUI({
      selectInput("surv_time",NULL,choices=code_feat,selected=length(code_feat))
    })
  })
  
  #---select input to choose a patient record
  observeEvent({
    input$data_file
    input$patientID
  },
  {output$patient_pred_output <- renderUI({
    selectInput("patient_pred",NULL,choices=code_record,selected=1)
  })
  })
  
  
  #---plot survival curve depending on selected 'event', 'time' and 'feature'
  observeEvent({
    input$data_file
    input$patientID
    input$event
    input$surv_time
    },
    {output$survival_curve <- renderPlot({
      inFile <<- input$data_file
      if (is.null(inFile))
        return(NULL)
      
      index_time <- strtoi(input$surv_time)
      index_feat <- strtoi(input$feature)
      index_event <- strtoi(input$event)
      if( input$data_visualize == "feat" && length(unique(copd[,index_feat])) < 5 ){
        surv_obj <- Surv(copd[,index_time],copd[,index_event]==1)
        fit <- survfit(surv_obj~copd[,index_feat] , data = copd )
        ggsurvplot(fit,risk.table=TRUE,break.time.by=ceiling(max(copd[,index_time])/5),risk.table.y.text=FALSE,risk.table.height=0.35,legend="none")
      }
    })
  })
  
  #---plot the coefficients for the models
  observeEvent({
    input$data_file
    input$event
    input$surv_time
  },
  {output$model_coeff <- renderPlotly({
      index_time <- strtoi(input$surv_time)
      index_event <- strtoi(input$event)
      if( input$model == "coxmodel" ){
        surv_obj <- Surv(copd[,index_time],copd[,index_event]==1)
        mod.cox <<- coxph(surv_obj~.,data=copd[,-c(index_time,index_event)])
        cox_prediction <<- predict(mod.cox,type="risk",se.fit=TRUE)
        x <- as.data.frame(mod.cox$coefficients)
        dtf <- data.frame(x = rownames(x),y = x[,1])
        n <- length(dtf$x)
        plotly_cols <- colorRampPalette(brewer.pal(9,"Blues"))(2*n)
        m <- plot_ly(dtf,x=dtf$x,y=dtf$y,text=paste("Feature: ",dtf$x),marker=list(color=plotly_cols[(n):(2*n)]),showlegend=FALSE)
        m <- layout(m,title = "Estimated Coefficients for Cox-model")
        m
      }
    })
  })
  
  #---print the risk score of a patient
  observeEvent({
    input$data_file
    input$event
    input$surv_time
    input$patient_pred
  },
  {
    output$risk_score_output <- renderText({
      if( input$model == "coxmodel" ){
        paste("Risk Score = ",round(cox_prediction$fit[input$patient_pred],4))
      }else{
        "You must select a valid model"
      }
    })
  })
  
  #---plot the survival curve for a patient
  observeEvent({
    input$data_file
    input$event
    input$surv_time
    input$patient_pred
  },
  {
    output$survival_curve_output <- renderPlotly({
      inFile <<- input$data_file
      if (is.null(inFile))
        return(NULL)
      
      if( input$model == "coxmodel" ){
        index_patient <- strtoi(input$patient_pred)
        surv.fit <- survfit(mod.cox,newdata=data.frame(copd[index_patient,]))
        # xlab="Time",ylab="Survival",main=paste("Survival curve for record",input$patient_pred),col="blue",lwd=2)
        title <- paste("Survival probability for Patient",index_patient,"with Cox-model")
        m <- ggsurv(surv.fit,plot.cens=FALSE,surv.col="blue",size.est=0.5,size.ci=0.2,xlab="Time",ylab="Survival Probability",main=title)
        ggplotly(m)
      }
      
    })
  })

  
})
