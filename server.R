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
  
  #---contains the list of features
  listOfFeatures <- list()
  
  #---contains the list of patients
  listOfPatients <- list()
  
  #---the general event of interest 'death', 'readmission', 'both', 'time's
  isDeath <- NULL
  isBoth <- NULL
  timeDeath <- NULL
  timeBoth <- NULL
  dataDeath <- NULL
  dataBoth <- NULL
  
  #---contains the data and the current data
  patientsID <- NULL
  initialData <- NULL
  currentData <- NULL
  currentEvent <- NULL
  currentTime <- NULL
  
  #---parameters
  defaultCaterogicalLimit <- 5
  
  #---contains model coefficients ans prediction
  mod.cox <- NULL
  pred.cox <- NULL
  
  #############################
  #------functions
  #############################
  
  #---read the data file
  readFile <- function( name ){
    mydata <- read.csv(name, header = input$header, sep = ',')
    if( length(mydata[1,]) == 1 )
      mydata <- read.csv(name, header = input$header, sep = ';')
    if( length(mydata[1,]) == 1 )
      mydata <- read.csv(name, header = input$header, sep = '\t')
    return(mydata)
  }
  
  #---set the data
  setData <- function(){
    d <- length(initialData[1,])
    n <<- length(initialData[,1])

    #---check if the there is the require features
    if( d < 9 )
      return(NULL)
    
    #---save the list of patientsID (in column 1)
    listOfPatients <<- as.list(1:n)
    names(listOfPatients) <<- paste(as.list(rep("Patient",n)),as.character(as.list(copd[,1])))
    
    #---save the interesting features
    patientsID <<- initialData[,1]
    isDeath <<- initialData[,2]
    timeDeath <<- initialData[,3]
    isBoth <<- initialData[,4]
    timeBoth <<- initialData[,5]
    
    dataDeath <<- initialData[,-c(1:5)]
    dataBoth <<- initialData[,-c(1:6)]

    #---save the list of remaining features
    listOfFeaturesDeath <<- as.list(1:length(dataDeath))
    listOfFeaturesBoth <<- as.list(1:length(dataBoth))
    names(listOfFeaturesDeath) <<- as.list(colnames(dataDeath))
    names(listOfFeaturesDeath) <<- as.list(colnames(dataDeath))
    
    setEventOfInterest()
  }
  
  #---sets the current event of interest and time depending of the choice
  setEventOfInterest <- function(){
    if( input$inputEventOfInterest == "Death" ){
      currentEvent <<- isDeath
      currentTime <<- timeDeath
      currentData <<- dataDeath
    }else if( input$inputEventOfInterest == "Both" ){
      currentEvent <<- isBoth
      currentTime <<- timeBoth
      currentData <<- dataBoth
    }
    listOfFeatures <<- as.list(1:length(currentData))
    names(listOfFeatures) <<- as.list(colnames(currentData))
  }
  
  #---create HTML text to display for featuresInfos
  getFeaturesInfos <- function(){
    binaryFeatures <- c()
    categoricalFeatures <- c()
    numericalFeatures <- c()
    featuresName <- names(listOfFeatures)
    for( i in 1:(length(currentData[1,])) ){
      lengthFeature <- length(unique(currentData[,i]))
      if( lengthFeature == 2 )
        binaryFeatures <- c( binaryFeatures , featuresName[i] )
      else if( lengthFeature < defaultCaterogicalLimit )
        categoricalFeatures <- c( categoricalFeatures , featuresName[i] )
      else
        numericalFeatures <- c( numericalFeatures , featuresName[i] )
    }
    text <- paste(strong("Number of features :"),length(currentData[1,]),br(),
                  strong("Number of record :"),n,br(),br())
    maxLenth <- max(length(binaryFeatures),max(length(categoricalFeatures),length(numericalFeatures)))
    #---construct the html table
    trFeatures <- tagList()
    trFeatures <- tagList( trFeatures , tags$tr(
                                    tags$th(strong("Binary (0/1)")),
                                    tags$th(strong("Categorical <"),strong(defaultCaterogicalLimit)),
                                    tags$th(strong("Numerical >="),strong(defaultCaterogicalLimit))
                                  )
                  )
    trFeatures <- tagList( trFeatures , tags$tr(
                                        tags$th(strong(length(binaryFeatures))),
                                        tags$th(strong(length(categoricalFeatures))),
                                        tags$th(strong(length(numericalFeatures)))
                                      )
                  )
    for( i in 1:maxLenth ){
      name1 <- ""; name2 <- ""; name3 <- ""
      if( !is.na(binaryFeatures[i]) )
        name1 <- binaryFeatures[i]
      if( !is.na(categoricalFeatures[i]) )
        name2 <- categoricalFeatures[i]
      if( !is.na(numericalFeatures[i]) )
        name3 <- numericalFeatures[i]
      trFeatures <- tagList( trFeatures , tags$tr(
                                          tags$th(name1),
                                          tags$th(name2),
                                          tags$th(name3)
                                        )
                    )
    }
    tableFeatures <- as.character(tags$table(trFeatures))
    text <- paste(text,tableFeatures)
    return(text)
  }
  
  #---create HTML text to display for patientInfos
  getPatientInfos <- function( index ){
    psex <- "M"
    if( currentData$sex[index] == 0 )psex <- "F"
    text <- paste(strong("Patient"),strong(index),":",patientsID[index],br())
    text <- paste(text,"Sex :",psex,br(),"Age :",currentData$age[index],br(),br())
    tableFeatures <- tags$table(
      tags$tr(
        tags$th(strong("1st hospitalization LoS")),
        tags$th(strong("Total COPD-Cause LoS")),
        tags$th(strong("Total all-Cause LoS"))
      ),
      tags$tr(
        tags$th(currentData$LOS_index[index]),
        tags$th(currentData$COPD_Cause_LOS[index]),
        tags$th(currentData$all_Cause_LOS[index])
      )
    )
    text <- paste(text,as.character(tableFeatures))
    return(text)
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
        
    initialData <<- readFile(inFile$datapath)
    setData()
    
  })
  
  #---change currentEvent in reaction to selectInput
  observeEvent(
    input$inputEventOfInterest,
    setEventOfInterest(),
    priority=1
  )
  
  #---load the default data
  observeEvent(
    input$defaultData, 
    {
    path <- "www/copd_demo_data_csv.csv"
    initialData <<- readFile(path)
    setData()
  })
  
  #---create a ui select input for features
  observeEvent({
    input$data_file
    input$defaultData
    input$inputEventOfInterest
    },
    {output$selectInputFeatures <- renderUI({
        listOfChoices = c("DEFAULT"="default",listOfFeatures)
        selectInput("inputFeatures",NULL,choices=listOfChoices)
      })
    },priority=0)
  
  #---create a ui select input for patients
  observeEvent({
    input$data_file
    input$defaultData
    },
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
  
  #---print data table
  observeEvent({
    input$data_file
    input$defaultData
    input$inputEventOfInterest
  },
  {output$dataInfos <- renderDataTable({
    displayData <- NULL
    if( input$inputEventOfInterest == "Both" ){
      displayData <- cbind(patientsID,isBoth,timeBoth,currentData)
    }else if( input$inputEventOfInterest == "Death" ){
      displayData <- cbind(patientsID,isDeath,timeDeath,currentData)
    }
    displayData
  }, options = list(lengthMenu = c(10,25,50,n), pageLength = n ) )
  })
  
  #---print default informations about features
  observeEvent({
    input$inputEventOfInterest
    input$inputFeatures
    },
    {output$featuresInfos <- renderUI({
      if( input$dataDisplay == "features" && input$inputFeatures == "default" ){
        text <- getFeaturesInfos()
        HTML(text)
      }
      })
    })
  
  #---print informations about patients
  observeEvent(
    input$inputPatients,
    {output$patientsInfos <- renderUI({
      if( input$dataDisplay == "patients" ){
        index <- strtoi(input$inputPatients)
        text <- getPatientInfos(index)
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
      feat <- currentData[,index_feat]
      if( length(unique(feat)) > defaultCaterogicalLimit ){
        #---plot the histogram
        m <- ggplot(currentData,aes(x=feat,fill=feat))+geom_histogram( )
        m <- m + labs(x=names(listOfFeatures)[index_feat]) + labs(title=paste("Histogram of",names(listOfFeatures)[index_feat]))
        m <- m + scale_fill_gradient("Count",low="blue4",high="lightblue")
        if( !is.null(m) )
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
        m <- m + labs(title=paste("Count of '",names(listOfFeatures)[index_feat],"' depending on '",input$inputEventOfInterest,"'"))
        m <- m + labs(x=input$inputEventOfInterest) + labs(y="count") + labs(fill=names(listOfFeatures)[index_feat])
        if( !is.null(m) )  
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
      feat <- currentData[,index_feat]
      if( length(unique(feat)) < defaultCaterogicalLimit ){
        surv_obj <- Surv(currentTime,currentEvent==1)
        fit <- survfit(surv_obj~currentData[,index_feat] , data = currentData )
        resultPlot <- ggsurvplot(fit,risk.table=TRUE,break.time.by=ceiling(max(currentTime)/5),risk.table.y.text=FALSE,risk.table.height=0.35,legend="none",risk.table.col="strata")
      }
    }
    resultPlot
  })
  })
  
  #---select input to choose the features to apply a model
  observeEvent({
    input$data_file
    input$defaultData
    },
    {output$featureSelectionForPrediction <- renderUI({
      selectInput("featuresForPrediction",NULL,choices=listOfFeatures,multiple=TRUE,selected=listOfFeatures)
    })
  })
  
  #---plot the coefficients for the models
  observeEvent({
    input$data_file
    input$defaultData
    input$featureForPrediction
    input$inputEventOfInterest
  },
  {output$modelCoeff <- renderPlotly({
    if( input$model == "coxmodel" ){
      surv_obj <- Surv(currentTime,currentEvent==1)
      mod.cox <<- coxph(surv_obj~.,data=currentData)
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
    input$defaultData
  },
  {output$patientSelection <- renderUI({
    selectInput("patientSelect",NULL,choices=listOfPatients,selected=1)
    })
  })
  
  #---print the risk score of a patient
  observeEvent({
    input$data_file
    input$defaultData
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
    input$defaultData
    input$featureForPrediction
    input$inputEventOfInterest
    input$model
    input$patientSelect
  },
  {output$survivalCurvePatient <- renderPlotly({
      if( input$model == "coxmodel" ){
        index_patient <- strtoi(input$patientSelect)
        surv.fit <- survfit(mod.cox,newdata=data.frame(currentData[index_patient,]))
        # xlab="Time",ylab="Survival",main=paste("Survival curve for record",input$patient_pred),col="blue",lwd=2)
        title <- paste("Survival probability for Patient",index_patient,"with Cox-model")
        m <- ggsurv(surv.fit,plot.cens=FALSE,surv.col="blue",xlab="Time",ylab="Survival Probability",main=title)
        ggplotly(m)
      }
      
    })
  })
  
})
