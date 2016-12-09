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


shinyUI(
  navbarPage(
    strong("COPD_Demo"),
    position = "static-top",
    windowTitle = "COPD_Demo",
    collapsible = TRUE,
    theme = shinytheme("cerulean"),
    #---HOME PANEL
    tabPanel(
      strong("Home"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      fluidRow(
        column(12,
               h1("Web Application for COPD prediction", align = "center"),
               tags$hr(),
               h1("Application Web pour prédiction liée à la MPOC", align = "center"),
               tags$h6(align="center",
                       "UdeS - Faculté des Sciences - Département Informatique - ",
                       HTML(' <a href="http://info.usherbrooke.ca/Prospectus" target="_blank">Prospectus</a> '),br(),
                       "CHUS - Centre Hospitalier Universitaire de Sherbrooke - ...",br(),
                       "GPL licence - 2016"
               )
        )
      )
    ),
    #---DATA PANEL
    tabPanel(
      strong("Data"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      fluidRow(
        #---PARAMETERS SELECTION
        column(4,
          fileInput("data_file", "Choose CSV File",
                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
          ),
          checkboxInput('header', 'Header', TRUE),
          checkboxInput('patientID', 'Patient ID', FALSE),
          tags$hr(),
          #radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
          wellPanel(strong("Event of Interest"),
                    tableOutput("data_event_choice")
          ),
          wellPanel(strong("Survival Time"),
                    tableOutput("data_stime_choice")
          ),
          wellPanel(
            strong("Feature or Hospitalization Records"),
            selectInput("data_visualize",NULL,
                        c("Features" = "feat","Hospitaliuzation Records" = "Hosp.Rec"),
                        selected="feat")
          ),
          wellPanel(strong("Select one object"),
                    tableOutput("data_contents")
          )
        ),
        #---PLOT OUTPUT
        column(8,
          strong(textOutput("patient_info_output")),
          plotlyOutput("data_stat"),
          plotOutput("survival_curve")
        )
      )
    ),
    #---PREDICTION PANEL
    tabPanel(
      strong("Prediction"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      fluidRow(
        #---PARAMETERS SELECTION--1
        column(4,
            wellPanel(strong("Select a Model"),
              selectInput("model",NULL,
                          c("Cox Model"="coxmodel","..."="..."),
                          selected="coxmodel")
            )
            #wellPanel(strong("k-fold Cross-Validation"),
            #  selectInput("k-folds",NULL,1:5)
            #)
        ),
        #---PLOT OUTPUT--1
        column(8,
            plotlyOutput("model_coeff")       
        )
      ),
      br(),
      fluidRow(
        #---PARAMETERS SELECTION--2
        column(4,
            wellPanel(
              strong("Choose a patient record"),
              tableOutput("patient_pred_output")
            )
        ),
        #---PLOT OUTPUT--2
        column(8,
            wellPanel(
              strong(textOutput("risk_score_output"))
            ),
             plotlyOutput("survival_curve_output")
        )
      )
    ),
    #---CLASSFICATION PANEL
    tabPanel(strong("Classification"))
  )
)




