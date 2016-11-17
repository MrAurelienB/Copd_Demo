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
    tabPanel(strong("Home")),
    tabPanel(
      strong("Data"),
      fluidRow(
        column(4,
          fileInput("data_file", "Choose CSV File",
                    accept = c("text/csv","text/comma-separated-values,text/plain",".csv")
          ),
          tags$hr(),
          checkboxInput('header', 'Header', TRUE),
          radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
          selectInput("data_visualize",NULL,
                      c("Features" = "feat","Hospitaliuzation Records" = "Hosp.Rec")),
          tableOutput("data_contents")
        ),
        column(8,
          plotOutput("data_stat")   
        )
      )
    ),
    tabPanel(strong("Prediction")),
    tabPanel(strong("Classification"))
  )
)







