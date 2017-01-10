#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(survival)
library(survminer)
library(ggplot2)
library(GGally)
library(plotly)
library(RColorBrewer)


shinyServer(function(input, output) {
  
  #############################
  #------variables
  #############################
  
  #---dimensions
  n <- 0
  df <- 0
  
  #---contains the list of features
  listOfFeatures <- list()
  
  #---contains the list of patients
  listOfPatients <- list()
  
  #---the general event of interest 'death', 'readmission', 'both', 'time's
  isDeath <- NULL
  isReadmission <- NULL
  isBoth <- NULL
  timeDeath <- NULL
  timeReadmission <- NULL
  timeBoth <- NULL
  
  #---and the current event of interest
  currentEvent <- NULL
  currentTime <- NULL
  
  #---contains the data matrix
  copd <- NULL
  initialCOPD <- NULL
  
  #---contains model coefficients ans prediction
  mod.cox <- NULL
  pred.cox <- NULL
  
  #---parameters
  defaultCaterogicalLimit <- 5
  
  #############################
  #------functions
  #############################
  
  #---read the data file
  readFile <- function(){
    copd <<- read.csv(inFile$datapath, header = input$header, sep = ',')
    if( length(copd[1,]) == 1 )
      copd <<- read.csv(inFile$datapath, header = input$header, sep = ';')
    if( length(copd[1,]) == 1 )
      copd <<- read.csv(inFile$datapath, header = input$header, sep = '\t')
    return(copd)
  }
  
  #---sets the current event of interest and time depending of the choice
  setEventOfInterest <- function(){
    if( input$inputEventOfInterest == "Readmission" ){
      currentEvent <<- isReadmission
      currentTime <<- timeReadmission
      #print("set event to : readmission")
    }else if( input$inputEventOfInterest == "Death" ){
      currentEvent <<- isDeath
      currentTime <<- timeDeath
      #print("set event to : death")
    }else if( input$inputEventOfInterest == "Both" ){
      currentEvent <<- isBoth
      currentTime <<- timeBoth
      #print("set event to : both")
    }
  }
  
  #################################
  #------output and observeEvent
  #################################
  
  #---read the file when upload
  observeEvent(
    input$data_file,
  {
    inFile <<- input$data_file
    if(is.null(inFile))
      return(NULL)
        
    copd <<- readFile()
    initialCOPD <<- copd
    d <- length(copd[1,])
    n <<- length(copd[,1])
      
    #---check if the there is the require features
    if( d < 7 )
      return(NULL)
      
    df <<- d - 7
    
    #---save the list of patientsID (in column 1)
    listOfPatients <<- as.list(1:n)
    names(listOfPatients) <<- paste(as.list(rep("Patient",n)),as.character(as.list(copd[,1])))

    #---save event of interest feature (column d-3,d-2,d-1,d)
    isReadmission <<- copd[,d-5]
    timeReadmission <<- copd[,d-4]
    isDeath <<- copd[,d-3]
    timeDeath <<- copd[,d-2]
    isBoth <<- copd[,d-1]
    timeBoth <<- copd[,d]
      
    setEventOfInterest()

    copd <<- copd[,-c(1,(d-5):d)]

    #---save the list of remaining features
    listOfFeatures <<- as.list(1:length(copd[1,]))
    names(listOfFeatures) <<- as.list(colnames(copd))

  })
  
  #---create a ui select input for features
  observeEvent(
    input$data_file,
    {output$selectInputFeatures <- renderUI({
        listOfChoices = c("DEFAULT"="default",listOfFeatures)
        selectInput("inputFeatures",NULL,choices=listOfChoices)
      })
    })
  
  #---create a ui select input for patients
  observeEvent(
    input$data_file,
    {output$selectInputPatients <- renderUI({
        selectInput("inputPatients",NULL,choices=listOfPatients)
      })
    })
  
  #---disable / enable the selectInput
  observeEvent({
    input$dataDisplay
    input$inputPatients
    input$inputFeatures
    },
    {
      if( input$dataDisplay == "features" ){
        shinyjs::disable("inputPatients")
        shinyjs::enable("inputFeatures")
      }
      else if( input$dataDisplay == "patients" ){
        shinyjs::disable("inputFeatures")
        shinyjs::enable("inputPatients")
      }
    })
  
  #---change currentEvent in reaction to selectInput
  observeEvent({
    input$data_file
    input$inputEventOfInterest
    },
    setEventOfInterest()
  )
  
  #---print data table
  observeEvent({
    input$data_file
    input$inputEventOfInterest
  },
  {output$dataInfos <- renderDataTable({
    initialCOPD
  }, options = list(lengthMenu = c(10,25,50,n), pageLength = n))
  })
  
  #---print default informations about features
  observeEvent(
    input$inputFeatures,
    {output$featuresInfos <- renderUI({
      if( input$dataDisplay == "features" && input$inputFeatures == "default" ){
        binaryFeatures <- c()
        categoricalFeatures <- c()
        numericalFeatures <- c()
        for( i in 1:df ){
          lengthFeature <- length(unique(copd[,i]))
          if( lengthFeature == 2 ){
            binaryFeatures <- c( binaryFeatures , names(listOfFeatures[i]) )
          }else if( lengthFeature < defaultCaterogicalLimit ){
            categoricalFeatures <- c( categoricalFeatures , names(listOfFeatures[i]) )
          }else{
            numericalFeatures <- c( numericalFeatures , names(listOfFeatures[i]) )
          }
        }
        print(binaryFeatures)
        print(categoricalFeatures)
        print(numericalFeatures)
        text <- paste(strong("Number of features :"),df,"<br/>",
                      strong("Number of record :"),n,"<br/><br/>")
        maxLenth <- max(length(binaryFeatures),max(length(categoricalFeatures),length(numericalFeatures)))
        tableFeatures <- paste("<table><tr><th>",strong("Binary (0/1)"),"</th><th>",strong("Categorical <"),strong(defaultCaterogicalLimit),"</th><th>",strong("Numerical >="),strong(defaultCaterogicalLimit),"</th></tr>")
        tableFeatures <- paste(tableFeatures , "<tr><th>",strong(length(binaryFeatures)),"</th><th>",strong(length(categoricalFeatures)),"</th><th>",strong(length(numericalFeatures)),"</th></tr>")
        for( i in 1:maxLenth ){
          tableFeatures <- paste(tableFeatures,"<tr>")
          if( !is.na(binaryFeatures[i]) )
            tableFeatures <- paste(tableFeatures,"<th>",binaryFeatures[i],"</th>")
          else
            tableFeatures <- paste(tableFeatures,"<th></th>")
          if( !is.na(categoricalFeatures[i]) )
            tableFeatures <- paste(tableFeatures,"<th>",categoricalFeatures[i],"</th>")
          else
            tableFeatures <- paste(tableFeatures,"<th></th>")
          if( !is.na(numericalFeatures[i]) )
            tableFeatures <- paste(tableFeatures,"<th>",numericalFeatures[i],"</th>")
          else
            tableFeatures <- paste(tableFeatures,"<th></th>")
          tableFeatures <- paste(tableFeatures,"</tr>")
        }
        tableFeatures <- paste(tableFeatures,"</tr></table>")
        text <- paste(text,tableFeatures)
        HTML(text)
      }
      })
    })
  
  #---barplot to visualize data depending on 'event of interest'
  observeEvent({
    input$inputFeatures
  },
  {output$dataBarplot <- renderPlotly({
    resultPlot <- NULL
    if( input$dataDisplay == "features" & input$inputFeatures != "default" ){
      index_feat <- strtoi(input$inputFeatures)
      feat <- copd[,index_feat]
      if( length(unique(feat)) > defaultCaterogicalLimit ){
        #---plot the histogram
        m <- ggplot(copd,aes(x=feat,fill=feat))+geom_histogram( )
        m <- m + labs(x=colnames(copd)[index_feat]) + labs(title=paste("Histogram of",colnames(copd)[index_feat]))
        m <- m + scale_fill_gradient("Count",low="blue4",high="lightblue")
        resultPlot <- ggplotly(m,tooltip=c("count"))
      }else{
        #---plot a barplot
        x <- as.factor(currentEvent)
        y <- as.factor(feat)
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
        m <- m + labs(title=paste("Count of '",colnames(copd)[index_feat],"' depending on '",input$inputEventOfInterest,"'"))
        m <- m + labs(x=input$inputEventOfInterest) + labs(y="count") + labs(fill=colnames(copd)[index_feat])
        resultPlot <- ggplotly(m,tooltip=c("y"))
      }
    }
    resultPlot
  })
  })
  
  #---plot survival curve for the selected feature
  observeEvent({
    input$inputFeatures
  },
  {output$survivalCurveFeature <- renderPlot({
    resultPlot <- NULL
    if( input$dataDisplay == "features" & input$inputFeatures != "default" ){
      index_feat <- strtoi(input$inputFeatures)
      feat <- copd[,index_feat]
      if( length(unique(feat)) < defaultCaterogicalLimit ){
        surv_obj <- Surv(currentTime,currentEvent==1)
        fit <- survfit(surv_obj~copd[,index_feat] , data = copd )
        resultPlot <- ggsurvplot(fit,risk.table=TRUE,break.time.by=ceiling(max(currentTime)/5),risk.table.y.text=FALSE,risk.table.height=0.35,legend="none",risk.table.col="strata")
      }
    }
    resultPlot
  })
  })
  
  #---select input to choose the features to apply a model
  observeEvent(
    input$data_file,
    {output$featureSelectionForPrediction <- renderUI({
      selectInput("featuresForPrediction",NULL,choices=listOfFeatures,multiple=TRUE,selected=listOfFeatures)
    })
  })
  
  #---plot the coefficients for the models
  observeEvent({
    input$data_file
    input$featureForPrediction
    input$inputEventOfInterest
  },
  {output$modelCoeff <- renderPlotly({
    if( input$model == "coxmodel" ){
      surv_obj <- Surv(currentTime,currentEvent==1)
      mod.cox <<- coxph(surv_obj~.,data=copd)
      pred.cox <<- predict(mod.cox,type="risk",se.fit=TRUE)
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
  
  #---select input to choose a patient
  observeEvent({
    input$data_file
  },
  {output$patientSelection <- renderUI({
    selectInput("patientSelect",NULL,choices=listOfPatients,selected=1)
    })
  })
  
  #---print the risk score of a patient
  observeEvent({
    input$data_file
    input$featureForPrediction
    input$inputEventOfInterest
    input$model
    input$patientSelect
  },
  {output$riskScore <- renderText({
      if( input$model == "coxmodel" ){
        paste("Risk Score = ",round(pred.cox$fit[input$patientSelect],4))
      }else{
        "You must select a valid model"
      }
    })
  })
  
  #---plot the survival curve for a patient
  observeEvent({
    input$data_file
    input$featureForPrediction
    input$inputEventOfInterest
    input$model
    input$patientSelect
  },
  {output$survivalCurvePatient <- renderPlotly({
      inFile <<- input$data_file
      if (is.null(inFile))
        return(NULL)
      
      if( input$model == "coxmodel" ){
        index_patient <- strtoi(input$patientSelect)
        surv.fit <- survfit(mod.cox,newdata=data.frame(copd[index_patient,]))
        # xlab="Time",ylab="Survival",main=paste("Survival curve for record",input$patient_pred),col="blue",lwd=2)
        title <- paste("Survival probability for Patient",index_patient,"with Cox-model")
        m <- ggsurv(surv.fit,plot.cens=FALSE,surv.col="blue",xlab="Time",ylab="Survival Probability",main=title)
        ggplotly(m)
      }
      
    })
  })
  
})
