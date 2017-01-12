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
  errorStyle,
  fluidRow(
    column(width,
           h1("Web Application for COPD prediction", align = "center"),
           tags$hr(),
           h1("Application Web pour prediction liee a la MPOC", align = "center"),
           tags$h6(align="center",
                   "UdeS - Faculte des Sciences - Departement Informatique - ",
                   HTML(' <a href="http://info.usherbrooke.ca/Prospectus" target="_blank">Prospectus</a> '),br(),
                   "CHUS - Centre Hospitalier Universitaire de Sherbrooke",br(),
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
  errorStyle,
  fluidRow(
    #---MENU
    column(leftWidth,
      fileInput("data_file", "Choose CSV File",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
      ),
      checkboxInput('header', 'Header', TRUE),
      tags$hr(),
      actionLink("defaultData","Load Default COPD-Data"),
      tags$hr(),
      strong("Event of interest (failure)"),
      selectInput("inputEventOfInterest",NULL,
                  c("Death"="Death","Readmission And Death"="Both"),
                  selected="Both")
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
  errorStyle,
  htmlTableStyle,
  fluidRow(
    shinyjs::useShinyjs(),
    #---MENU
    column(leftWidth,
      strong("Display"),
      selectInput("dataDisplay",NULL,
                       c("Features" = "features","Hospitalization Records" = "patients"),
                       selected="features"),
      strong("Select a feature"),
      uiOutput("selectInputFeatures"),
      strong("Select a record"),
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
tabPanel_Prediction <- tabPanel(
  strong("Prediction"),
  errorStyle,
  fluidRow(
    #---Select parameter for prediction
    column(leftWidth,
      strong("Select a Model"),
      selectInput("model",NULL,
           c("Cox Model"="coxmodel","..."="..."),
           selected="coxmodel"),
      strong("Select features"),
      tableOutput("featureSelectionForPrediction")
    ),
    #---prediction plot
    column(rightWidth,
      plotlyOutput("modelCoeff") 
    )
  ),
  br(),
  fluidRow(
    #---select patient for prediction
    column(leftWidth,
      strong("Choose a patient"),
      tableOutput("patientSelection")
    ),
    #---plot patient prediction
    column(rightWidth,
      wellPanel(strong(textOutput("riskScore"))),
      plotlyOutput("survivalCurvePatient")
    )
  )
)

##############################
###---CLASSIFICATION---PANEL
##############################
tabPanel_Classifier <- tabPanel(
  strong("Classifier"),
  errorStyle,
  fluidRow(
    column(leftWidth,
      strong("Select a classifier"),
      selectInput("classificationMethod",NULL,
                   c("k-nn" = "knn","SVM" = "svm"))
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