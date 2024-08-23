#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
  
  
  
  
  
  dashboardHeader(title = sprintf("Earthquake  \n Dashboard")),
  dashboardSidebar(width = "17vw",
    
    
    uiOutput("Selectyear")
   
    
    
    
  ),
  dashboardBody(
    fluidRow(id="row1",
             column(width =6,
                    box(
                      title = "Map",width = NULL, status = "primary",height = "80vh",
                      leaflet::leafletOutput("myplot",height="70vh")
                    )    
                    ),
  column(width = 6,
         fluidRow(
           box(
             title = "Magnitude Hist",width = NULL, status = "primary",height = "40vh",
             plotly::plotlyOutput("hist",height="35vh")
           )
    
  ),
  fluidRow(
    
    box(
      title = "Depth Hist",width = NULL ,status = "primary",height = "40vh",
      plotly::plotlyOutput("hist2",height="35vh")
    )
    
  )
  )
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ) )
  
)

