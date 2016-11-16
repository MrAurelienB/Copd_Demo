#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



header <- dashboardHeader(title = strong("COPD-demo"),titleWidth = 200, disable = FALSE)




sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(id="menu",
              menuItem(strong("Home"),tabName = "home", icon = icon("home")),
              menuItem(strong("Data"), tabName = "data", icon = icon("file")),
              menuItem(strong("Charts"),tabName = "charts", icon = icon("bar-chart"))
  )
)






body <- dashboardBody(
  useShinyjs(),
  fluidRow(column(12,
    tabItems(
      
      tabItem(tabName = "home", h2("MAIN MENU") ),
      
      tabItem(tabName = "data", h2("DATA") ,
              fluidRow(column(12,
                              div(style="height: 60px;",
                                  fileInput('file_data', 'Choose CSV File',
                                            accept=c('text/csv','text/comma-separated-values,text/plain','.csv'))
                              )
              ))
              ),
      
      tabItem(tabName = "charts", h2("CHARTS") )
      
    )
  ))
)





dashboardPage(header, sidebar, body, skin = "green")


