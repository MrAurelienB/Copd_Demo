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
    tabPanel(strong("Data")),
    tabPanel(strong("Prediction")),
    tabPanel(strong("Classification"))
  )
)







