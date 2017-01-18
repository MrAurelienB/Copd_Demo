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


shinyServer(function(input, output, session) {
  
  #############################
  #------variables
  #############################
  
  dataLoaded <- FALSE
  
  #---list element that contains the training set objects
  train <- list(
    "n" = 0,
    "listOfFeatures" = NULL,
    "listOfPatients" = NULL,
    "isDeath" = NULL,
    "isBoth" = NULL,
    "timeDeath" = NULL,
    "timeBoth" = NULL,
    "dataDeath" = NULL,
    "dataBoth" = NULL,
    "patientsID" = NULL,
    "initialData" = NULL,
    "currentData" = NULL,
    "currentEvent" = NULL,
    "currentTime" = NULL,
    "admission" = NULL,
    "discharge" = NULL,
    "disease" = NULL
  )
  
  #---list element that contains the test set objects
  test <- list(
    "n" = 0,
    "listOfFeatures" = NULL,
    "listOfPatients" = NULL,
    "isDeath" = NULL,
    "isBoth" = NULL,
    "timeDeath" = NULL,
    "timeBoth" = NULL,
    "dataDeath" = NULL,
    "dataBoth" = NULL,
    "patientsID" = NULL,
    "initialData" = NULL,
    "currentData" = NULL,
    "currentEvent" = NULL,
    "currentTime" = NULL,
    "admission" = NULL,
    "discharge" = NULL,
    "disease" = NULL
  )

  #---parameters
  defaultCaterogicalLimit <- 5
  
  #---contains model coefficients ans prediction
  model.cox <- list(
    "mod" <- NULL,
    "pred" <- NULL,
    "ci" <- NULL,
    "auc" <- NULL,
    "mse" <- NULL
  )
  
  #############################
  #------functions
  #############################
  
  #---read the data file
  readDataFile <- function( name ){
    mydata <- read.csv(name, header = input$headerDataFile, sep = ';')
    return(mydata)
  }
  
  #---read the test file
  readTestFile <- function( name ){
    mydata <- read.csv(name, header = input$headerTestFile, sep = ';')
    return(mydata)
  }
  
  #---set the training data
  setTrainingData <- function(){
    d <- length(train$initialData[1,])
    n <- length(train$initialData[,1])
    train$n <<- n

    #---check if the there is the require features
    if( d < 9 )
      return(NULL)
    
    #---save the interesting features
    train$patientsID <<- as.character(train$initialData[,1])
    train$isDeath <<- train$initialData[,2]
    train$timeDeath <<- train$initialData[,3]
    train$isBoth <<- train$initialData[,4]
    train$timeBoth <<- train$initialData[,5]
    
    train$admission <<- train$initialData[,6]
    train$discharge <<- train$initialData[,7]
    train$disease <<- train$initialData[,8]
    
    train$dataDeath <<- train$initialData[,-c(1:8)]
    train$dataBoth <<- train$initialData[,-c(1:8)]

    #---save the list of patientsID (in column 1)
    train$listOfPatients <<- as.list(1:n)
    names(train$listOfPatients) <<- paste(as.list(rep("Patient",n)),as.character(as.list(1:n)),":",as.character(train$patientsID))
    
    setEventOfInterest()
    dataLoaded <<- TRUE
  }
  
  #---set the test data
  setTestData <- function(){
    d <- length(test$initialData[1,])
    n <- length(test$initialData[,1])
    test$n <<- n
    
    #---check if the there is the require features
    if( d < 9 )
      return(NULL)
    
    #---save the interesting features
    test$patientsID <<- as.character(test$initialData[,1])
    test$isDeath <<- test$initialData[,2]
    test$timeDeath <<- test$initialData[,3]
    test$isBoth <<- test$initialData[,4]
    test$timeBoth <<- test$initialData[,5]
    
    test$admission <<- test$initialData[,6]
    test$discharge <<- test$initialData[,7]
    test$disease <<- test$initialData[,8]
    
    test$dataDeath <<- test$initialData[,-c(1:8)]
    test$dataBoth <<- test$initialData[,-c(1:8)]
    
    #---save the list of patientsID (in column 1)
    test$listOfPatients <<- as.list(1:n)
    names(test$listOfPatients) <<- paste(as.list(rep("Patient",n)),as.character(as.list(1:n)),":",as.character(test$patientsID))

  }
  
  #---sets the current event of interest and time depending of the choice
  setEventOfInterest <- function(){
    if( input$inputEventOfInterest == "Death" ){
      train$currentEvent <<- train$isDeath
      train$currentTime <<- train$timeDeath
      train$currentData <<- train$dataDeath
      test$currentData <<- test$dataBoth
    }else if( input$inputEventOfInterest == "Both" ){
      train$currentEvent <<- train$isBoth
      train$currentTime <<- train$timeBoth
      train$currentData <<- train$dataBoth
      test$currentData <<- test$dataBoth
    }
    train$listOfFeatures <<- as.list(1:length(train$currentData))
    names(train$listOfFeatures) <<- as.list(colnames(train$currentData))
    test$listOfFeatures <<- as.list(1:length(test$currentData))
    names(test$listOfFeatures) <<- as.list(colnames(test$currentData))
  }
  
  #---create HTML text to display for featuresInfos
  getFeaturesInfos <- function(){
    binaryFeatures <- c()
    categoricalFeatures <- c()
    numericalFeatures <- c()
    featuresName <- names(train$listOfFeatures)
    for( i in 1:(length(train$currentData[1,])) ){
      lengthFeature <- length(unique(train$currentData[,i]))
      if( lengthFeature == 2 )
        binaryFeatures <- c( binaryFeatures , featuresName[i] )
      else if( lengthFeature < defaultCaterogicalLimit )
        categoricalFeatures <- c( categoricalFeatures , featuresName[i] )
      else
        numericalFeatures <- c( numericalFeatures , featuresName[i] )
    }
    text <- paste(strong("Number of features :"),length(train$currentData[1,]),br(),
                  strong("Number of record :"),train$n,br(),br())
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
      if( !is.null(binaryFeatures) ){
        if( !is.na(binaryFeatures[i]) )
          name1 <- binaryFeatures[i]
      }
      if( !is.null(categoricalFeatures) ){
        if( !is.na(categoricalFeatures[i]) )
          name2 <- categoricalFeatures[i]
      }
      if( !is.null(numericalFeatures) ){
        if( !is.na(numericalFeatures[i]) )
          name3 <- numericalFeatures[i]
      }
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
    if( train$currentData$sex[index] == 0 )psex <- "F"
    text <- paste(strong("Patient"),strong(index),":",train$patientsID[index],br())
    text <- paste(text,"Sex :",psex,br(),"Age :",train$currentData$age[index],br(),br())
    tableFeatures <- tags$table(
      tags$tr(
        tags$th(strong("Admission")),
        tags$th(strong("Discharge")),
        tags$th(strong("Disease")),
        tags$th(strong("1st hospitalization LoS")),
        tags$th(strong("Number of Readmission")),
        tags$th(strong("Total COPD-Cause LoS")),
        tags$th(strong("Total all-Cause LoS"))
      ),
      tags$tr(
        tags$th(train$admission[index]),
        tags$th(train$discharge[index]),
        tags$th(train$disease[index]),
        tags$th(train$currentData$LOS_index[index]),
        tags$th(train$initialData$number_of_readmission_before_death[index]),
        tags$th(train$currentData$COPD_Cause_LOS[index]),
        tags$th(train$currentData$all_Cause_LOS[index])
      )
    )
    text <- paste(text,as.character(tableFeatures))
    return(text)
  }
  
  #################################
  #------output and observeEvent
  #################################

  #---read the training file when upload
  observeEvent(
    input$data_file,
  {
    inFile <<- input$data_file
    if(is.null(inFile))
      return(NULL)
        
    train$initialData <<- readDataFile(inFile$datapath)
    setTrainingData()
    
  })
  
  #---read the test file when upload
  observeEvent(
    input$test_file,
    {
      inFile <<- input$test_file
      if(is.null(inFile))
        return(NULL)
      
      test$initialData <<- readDataFile(inFile$datapath)
      setTestData()
      setTrainingData()
    })
  
  #---change currentEvent in reaction to selectInput
  observeEvent(
    input$inputEventOfInterest,
    setEventOfInterest()
  )
  
  #---load the default data
  observeEvent(
    input$defaultData, 
    {
    path_trainingSet <- "www/copd_demo_data_csv.csv"
    train$initialData <<- readDataFile(path_trainingSet)
    path_testSet <- "www/copd_demo_test_csv.csv"
    test$initialData <<- readDataFile(path_testSet)
    setTestData()
    setTrainingData()
  })
  
  #---create a ui select input for features
  observeEvent({
    input$inputEventOfInterest
    input$defaultData
    input$data_file
    input$dataDisplay
    },
    {output$selectInputFeatures <- renderUI({
      print("called")
        listOfChoices = c("DEFAULT"="default",train$listOfFeatures)
        selectInput("inputFeatures",NULL,choices=listOfChoices,width='100%')
      })
    })
  
  #---create a ui select input for patients
  observeEvent({
    input$defaultData
    input$data_file
    input$dataDisplay
    },
    {output$selectInputPatients <- renderUI({
        listOfChoices = as.list(test$listOfPatients)
        selectInput("inputPatients",NULL,choices=test$listOfPatients,width='100%')
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
    input$test_file
    input$defaultData
    input$inputEventOfInterest
  },
  {output$dataInfos <- renderDataTable({
    displayData <- NULL
    if( test$n > 0 ){
      displayData <- cbind(index=1:test$n,ID=test$patientsID)
    }
    if( input$inputEventOfInterest == "Both" ){
      displayData <- cbind(
        displayData,
        isEvent=test$isBoth,
        timeEvent=test$timeBoth
      )
    }else if( input$inputEventOfInterest == "Death" ){
      displayData <- cbind(
        displayData,
        isDeath=test$isDeath,
        timeDeath=test$timeDeath
      )
    }
    displayData <- cbind(
      displayData,
      admission=test$admission,
      discharge=test$discharge,
      disease=test$disease,
      test$currentData
    )
    displayData
  }, options = list(lengthMenu = c(10,25,50,test$n), pageLength = test$n ) )
  })
  
  #---print default informations about features
  observeEvent({
    input$inputEventOfInterest
    input$inputFeatures
    },
    {output$featuresInfos <- renderUI({
      if( !is.null(train$currentData) & input$dataDisplay == "features" && input$inputFeatures == "default" ){
        text <- getFeaturesInfos()
        HTML(text)
      }
      })
    })
  
  #---print informations about patients
  observeEvent(
    input$inputPatients,
    {output$patientsInfos <- renderUI({
      if( !is.null(train$currentData) ){
        if( input$dataDisplay == "patients" ){
          index <- strtoi(input$inputPatients)
          text <- getPatientInfos(index)
          HTML(text)
        }
      }
      })
    })
  
  #---barplot or histogram to visualize data depending on 'event of interest'
  observeEvent({
    input$inputFeatures
  },
  {output$dataBarplot <- renderPlotly({
    resultPlot <- NULL
    if( !is.null(train$currentData) & input$dataDisplay == "features" & input$inputFeatures != "default" ){
      index_feat <- strtoi(input$inputFeatures)
      feat <- train$currentData[,index_feat]
      if( length(unique(feat)) > defaultCaterogicalLimit ){
        #---plot the histogram
        m <- ggplot(train$currentData,aes(x=feat,fill=feat))+geom_histogram(aes(fill=..count..))
        m <- m + labs(x=names(train$listOfFeatures)[index_feat]) + labs(title=paste("Histogram of",names(train$listOfFeatures)[index_feat]))
        #m <- m + scale_fill_gradient("Count",low="blue4",high="lightblue")
        if( !is.null(m) )
          resultPlot <- ggplotly(m,tooltip=c("count"))
      }else{
        #---plot a barplot
        x <- as.factor(train$currentEvent)
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
        m <- m + labs(title=paste("Count of '",names(train$listOfFeatures)[index_feat],"' depending on '",input$inputEventOfInterest,"'"))
        m <- m + labs(x=input$inputEventOfInterest) + labs(y="count") + labs(fill=names(train$listOfFeatures)[index_feat])
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
    if( !is.null(train$currentData) ){
      resultPlot <- NULL
      if( input$dataDisplay == "features" & input$inputFeatures != "default" ){
        index_feat <- strtoi(input$inputFeatures)
        feat <- train$currentData[,index_feat]
        if( length(unique(feat)) < defaultCaterogicalLimit ){
          surv_obj <- Surv(train$currentTime,train$currentEvent==1)
          fit <- survfit(surv_obj~train$currentData[,index_feat] , data = train$currentData )
          resultPlot <- ggsurvplot(fit,risk.table=TRUE,break.time.by=ceiling(max(train$currentTime)/5),risk.table.y.text=FALSE,risk.table.height=0.35,legend="none",risk.table.col="strata",risk.table.title = "Number of patient at risk (i.e. alive) by time")
        }
      }
      resultPlot
    }
  })
  })
  
  #---select input to choose the features to apply a model
  observeEvent({
    input$data_file
    input$defaultData
    input$model
    },
    {output$featuresForModel <- renderUI({
      if( !is.null(train$currentData) ){
        listOfChoices <- as.list(train$listOfFeatures)
        checkboxGroupInput("featuresForPrediction",label=NULL,
                           choices=listOfChoices,selected=listOfChoices,width='100%')
      }
    })
  })
  
  #---select all/none checkbox for the features
  observeEvent({
   input$selectAllNone
  },
  {
    listOfChoices <- NULL
    listOfSelected <- character(0)
    if( !is.null(train$listOfFeatures) & dataLoaded ){
      listOfChoices <- train$listOfFeatures
      if( input$selectAllNone ){
        listOfSelected <- train$listOfFeatures
      }
      updateCheckboxGroupInput(session,inputId="featuresForPrediction",label="All/None",choices=listOfChoices,selected=listOfSelected)
    }
  })
  
  #---plot the coefficients for the models
  observeEvent({
    input$featuresForPrediction
    input$inputEventOfInterest
    input$model
  },
  {output$modelCoeff <- renderPlotly({
    if( length(input$featuresForPrediction) > 0 & !is.null(train$currentData) & !is.null(test$currentData) ){
      if( input$model == "coxmodel" ){
        index_feats <- strtoi(input$featuresForPrediction)
        surv_obj <- Surv(train$currentTime,train$currentEvent==1)
        model.cox$mod <<- coxph(surv_obj~.,data=data.frame(train$currentData[,index_feats]))
        newdata <- data.frame(test$currentData[,index_feats])
        model.cox$pred <<- predict(model.cox$mod,newdata=newdata,type="risk",se.fit=TRUE)
        x <- as.data.frame(model.cox$mod$coefficients)
        dtf <- data.frame(x = rownames(x),y = x[,1])
        n <- length(dtf$x)
        plotly_cols <- colorRampPalette(brewer.pal(9,"Blues"))(2*n)
        m <- plot_ly(dtf,x=dtf$x,y=dtf$y,type="bar",text=paste("Feature: ",dtf$x),marker=list(color=plotly_cols[(n):(2*n)]),showlegend=FALSE)
        m <- layout(m,title = "Estimated Coefficients by Cox-model")
        m
      }
    }
  })
  },priority=1)
  
  #---select input to choose a patient
  observeEvent({
    input$test_file
    input$defaultData
    input$model
  },
  {output$patientSelection <- renderUI({
    if( !is.null(test$currentData) ){
      listOfChoices <- as.list(test$listOfPatients)
      selectInput("patientSelect",NULL,choices=listOfChoices,selected=1,width='100%')
    }
    })
  })
  
  #---print the risk score of a patient
  observeEvent({
    input$featuresForPrediction
    input$inputEventOfInterest
    input$model
    input$patientSelect
  },
  {output$riskScore <- renderText({
    if( length(input$featuresForPrediction) > 0 & !is.null(train$currentData) & !is.null(test$currentData) ){
      if( input$model == "coxmodel" ){
        paste("Risk Score = ",round(model.cox$pred$fit[input$patientSelect],4))
      }else{
        "You must select a valid model"
      }
    }
    })
  })
  
  #---plot the survival curve for a patient
  observeEvent({
    input$featuresForPrediction
    input$inputEventOfInterest
    input$model
    input$patientSelect
  },
  {output$survivalCurvePatient <- renderPlotly({
    if( length(input$featuresForPrediction) > 0 & !is.null(train$currentData) & !is.null(test$currentData) ){
      if( input$model == "coxmodel" ){
        index_patient <- strtoi(input$patientSelect)
        index_feats <- strtoi(input$featuresForPrediction)
        surv.fit <- survfit(model.cox$mod,newdata=data.frame(test$currentData[index_patient,index_feats]))
        title <- paste("Survival probability for Patient",index_patient,"with Cox-model")
        m <- ggsurv(surv.fit,plot.cens=FALSE,surv.col="blue",xlab="Time",ylab="Survival Probability",main=title)
        ggplotly(m)
      }
    }
    })
  },priority=0)
  
})
