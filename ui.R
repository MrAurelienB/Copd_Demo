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

shinyUI(
  navbarPage(
    strong("COPD_Demo"),
    position = "static-top",
    windowTitle = "COPD_Demo",
    collapsible = TRUE,
    theme = shinytheme("cerulean"),
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
    tabPanel(
      strong("Data"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      fluidRow(
        column(4,
          fileInput("data_file", "Choose CSV File",
                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
          ),
          checkboxInput('header', 'Header', TRUE),
          checkboxInput('patientID', 'Patient ID', FALSE),
          tags$hr(),
          #radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
          wellPanel("Event of Interest",
                    tableOutput("data_event_choice")
          ),
          wellPanel("Survival Time",
                    tableOutput("data_stime_choice")
          ),
          selectInput("data_visualize",NULL,
                      c("Features" = "feat","Hospitaliuzation Records" = "Hosp.Rec"),
                      selected="feat"),
          wellPanel("Select one object",
                    tableOutput("data_contents")
          )
        ),
        column(8,
          plotOutput("data_stat"),
          plotOutput("survival_curve")
        )
      )
    ),
    tabPanel(
      strong("Prediction"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      fluidRow(
        column(4,
            wellPanel("Select a Model",
              selectInput("model",NULL,
                          c("Cox Model"="coxmodel","Our Model"="ourmodel"),
                          selected="coxmodel")
            ),
            wellPanel("k-fold Cross-Validation",
              selectInput("k-folds",NULL,1:5)
            )
        ),
        column(8)
      )
    ),
    tabPanel(strong("Classification"))
  )
)




