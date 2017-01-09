#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
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

###---HOME---PANEL
tabPanel_Home <- tabPanel(
  strong("Home"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  fluidRow(
    column(width,
           h1("Web Application for COPD prediction", align = "center"),
           tags$hr(),
           h1("Application Web pour prédiction liée à la MPOC", align = "center"),
           tags$h6(align="center",
                   "UdeS - Faculté des Sciences - Département Informatique - ",
                   HTML(' <a href="http://info.usherbrooke.ca/Prospectus" target="_blank">Prospectus</a> '),br(),
                   "CHUS - Centre Hospitalier Universitaire de Sherbrooke",br(),
                   "GPL licence - 2016"
           )
    )
  )
)

###---DATA---PANEL
tabPanel_Data <- tabPanel(
  strong("Data"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  fluidRow(
    #---MENU
    column(leftWidth,
      fileInput("data_file", "Choose CSV File",
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
      ),
      checkboxInput('header', 'Header', TRUE),
      tags$hr(),
      strong("Event of interest"),
      selectInput("inputEventOfInterest",NULL,
                  c("Readmission"="Readmission","Death"="Death","Readmission OR Death"="Both"),
                  selected="Readmission"),
      strong("Feature or Hospitalization Records"),
      selectInput("dataVisualization",NULL,
                  c("Features" = "features","Hospitalization Records" = "patients"),
                  selected="features"),
      strong("Select a feature"),
      uiOutput("dataSelection")
    ),
    #---PLOT
    column(rightWidth,
           tableOutput("patientInfos"),
           plotlyOutput("dataBarplot"),
           plotOutput("survivalCurveFeature")
    )
  )
)

###---PREDICTION---PANEL
tabPanel_Prediction <- tabPanel(
  strong("Prediction"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
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

###---CLASSIFICATION---PANEL
tabPanel_Classification <- tabPanel(
  strong("Classification"),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)



###---ASSEMBLE PANELS
shinyUI(
  navbarPage(
    strong("COPD Demo"),
    position = "static-top",
    windowTitle = "COPD Demo",
    collapsible = TRUE,
    theme = shinytheme("cerulean"),
    tabPanel_Home,
    tabPanel_Data,
    tabPanel_Prediction,
    tabPanel_Classification
  )
)