#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


# display parameters
width <- 12
leftWidth <- 3
rightWidth <- 9


errorStyle <- tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }")
htmlTableStyle <- tags$style(type="text/css",
                             "table {border-collapse: collapse;}",
                              "td, th {border-bottom: 1px solid #ddd; width: 180px; text-align: center; font-weight: normal;}")

#######################
###---HOME---PANEL
#######################
tabPanel_Home <- tabPanel(
  strong("Home"),
  value = "home",
  icon = icon("home"),
  errorStyle,
  fluidRow(
    column(width,
           h1("Web Application for COPD prediction", align = "center"),
           hr(),
           h6(align="center",
              "Universite de Sherbrooke (UdeS) - Faculte des Sciences - Departement Informatique",
              br(),
              HTML(' <a href="http://info.usherbrooke.ca/Prospectus" target="_blank">Laboratoire Prospectus</a> ')," - Aurelien Bach - Jianfei Zhang - Shengrui Wang",
              br()
           ),
           hr(),
           h6(align="center",
              "Centre Hospitalier Universitaire de Sherbrooke (CHUS)",
              br()
           ),
           hr(),
           h6(align="center",
              "GPL licence - 2016"
           )        
    )
  )
)

#######################
###---DATA---PANEL
#######################
tabPanel_Data <- tabPanel(
  strong("Data"),
  value = "data",
  icon = icon("upload"),
  errorStyle,
  fluidRow(
    #---MENU
    column(leftWidth,
      fileInput(inputId = "data_file", label = "Choose a Database (CSV File)",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                width='100%'
      ),
      checkboxInput(inputId = "headerDataFile", label = "Header", TRUE),
      hr(),
      actionLink(inputId = "defaultData", label = "Load Default COPD-Data"),
      hr(),
      selectInput(inputId = "inputEventOfInterest", label = "Event of interest (failure)",
                  c("Death"="Death","Combined Events (Readmission and Death)"="Both"),
                  selected="Both",width='100%'),
      hr(),
      fileInput(inputId = "test_file", label = "Choose a test file (CSV File)",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                width='100%'
      ),
      checkboxInput(inputId = "headerTestFile", label = "Header", TRUE),
      hr()
    ),
    #---PLOT
    column(rightWidth,
      dataTableOutput("dataInfos")
    )
  )
)

#######################
###---STAT---PANEL
#######################
tabPanel_Stat <- tabPanel(
  strong("Stat"),
  value = "stat",
  icon = icon("bar-chart"),
  errorStyle,
  htmlTableStyle,
  fluidRow(
    shinyjs::useShinyjs(),
    #---MENU
    column(leftWidth,
      selectInput(inputId = "dataDisplay", label = "Display",
                       c("Risk Factors" = "features","Patients" = "patients"),
                       selected="features",width='100%'),
      uiOutput("selectInputFeatures"),
      uiOutput("selectInputPatients")
    ),
    #---PLOT
    column(rightWidth,
      htmlOutput("featuresInfos"),
      htmlOutput("patientsInfos"),
      plotlyOutput("dataBarplot"),
      plotOutput("survivalCurveFeature")
    )
  )
)

############################
###---PREDICTION---PANEL
############################
listThreshold <- seq(5,95,5)
names(listThreshold) <- paste(seq(5,95,5),"%",sep="")
############################

#---select model and features
columnL <- column(leftWidth,
                  selectInput(inputId = "model", label = "Select a Model",
                              c("Cox Model"="coxmodel","..."="..."),
                              selected="coxmodel",width='100%'),
                  strong("Factors for Selection"),
                  checkboxInput(inputId = "selectAllNone", label = "All/None",value = TRUE),
                  wellPanel(
                    uiOutput("featuresForModel"),
                    style = "overflow-y:scroll; max-height: 300px"
                  ),
                  actionButton(inputId = "modelCompute", label = "Compute the model", icon = icon("chevron-right")),
                  hr(),
                  uiOutput("patientSelection"),
                  actionButton(inputId = "patientCompute", label = "Compute prediction for patient", icon = icon("chevron-right"))
)

panelR1 <- tabPanel("Coefficients",
  plotlyOutput("modelCoeff",width='100%')
)

panelR2 <- tabPanel("Cumulative Baseline Hazard",
                    fluidRow(
                      column(8,
                        plotlyOutput("cumulativeBaselineHazard",width='100%') 
                      ),
                      column(4,
                        dataTableOutput("cbhDataTable")
                      )
                    )
)

panelR3 <- tabPanel("Survival Curve",
                      column(8,
                             plotlyOutput("survivalCurvePatient",width='100%'),
                             selectInput(inputId = "thresholdSurvivalCurve",label = "Inverse search",
                                         choices = listThreshold,
                                         selected = "50"),
                             textOutput("timeThreshold")
                      ),
                      column(4,
                             selectInput(inputId = "interval",label = "Prediction interval",
                                         choices = c("Daily"=1,"Weekly"=7,"Monthly"=30),
                                         selected = "monthly"),
                             dataTableOutput("survivalDataTable")
                      )
)

panelR4 <- tabPanel("Survival Details",
                      column(leftWidth,
                        strong(textOutput("riskScore"))
                        
                      ),
                      column(rightWidth
                        
                      )
)

panelR5 <- tabPanel("Model Evaluation",
                      column(leftWidth,
                        htmlOutput("modelSummary")
                      ),
                      column(rightWidth
                             
                      )
)

columnR <- column(rightWidth,
                  tabsetPanel(
                    panelR1,
                    panelR2,
                    panelR3,
                    panelR4,
                    panelR5
                  )
)


############################
tabPanel_Prediction <- tabPanel(
  strong("Prediction"),
  value = "prediction",
  icon = icon("line-chart"),
  errorStyle,
  fluidRow(
    columnL,
    columnR
  )
)

##############################
###---CLASSIFICATION---PANEL
##############################
tabPanel_Classifier <- tabPanel(
  strong("Classifier"),
  value = "classifier",
  errorStyle,
  fluidRow(
    column(leftWidth,
      selectInput(inputId = "classificationMethod", label = "Select a Classifier",
                   c("k-nn" = "knn","SVM" = "svm"),width='100%')
      ),
    column(rightWidth
      
    )
  )
)


#########################
###---ASSEMBLE PANELS
#########################
shinyUI(
    navbarPage(
      strong("COPD Demo"),
      position = "static-top",
      windowTitle = "COPD Demo",
      collapsible = TRUE,
      theme = shinytheme("cerulean"),
      tabPanel_Home,
      tabPanel_Data,
      tabPanel_Stat,
      tabPanel_Prediction,
      tabPanel_Classifier
    )
)