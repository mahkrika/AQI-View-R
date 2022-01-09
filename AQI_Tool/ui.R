#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

orgs <- c("RX6", "RYA")
year <- c("2021", "2020")
manualOrgs <- c('England', 'North East and Yorkshire', 'North West', 'Midlands', 'East of England', 'London',
                'South East', 'South West', 'EAST MIDLANDS AMBULANCE SERVICE NHS TRUST', 'EAST OF ENGLAND AMBULANCE SERVICE NHS TRUST',
                'ISLE OF WIGHT NHS TRUST', 'LONDON AMBULANCE SERVICE NHS TRUST', 'NORTH EAST AMBULANCE SERVICE NHS FOUNDATION TRUST',
                'NORTH WEST AMBULANCE SERVICE NHS TRUST', 'SOUTH CENTRAL AMBULANCE SERVICE NHS FOUNDATION TRUST',
                'SOUTH EAST COAST AMBULANCE SERVICE NHS FOUNDATION TRUST', 'SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST',
                'WEST MIDLANDS AMBULANCE SERVICE UNIVERSITY NHS FOUNDATION TRUST', 'YORKSHIRE AMBULANCE SERVICE NHS TRUST',
                'WEST MIDLANDS AMBULANCE SERVICE NHS FOUNDATION TRUST')

ui <- dashboardPage(
  dashboardHeader(title = "AQI View-R"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Input", tabName = "input", icon = icon("clipboard-list"),
               textInput("urlIn", label = "URL of data:")
               ),
      
      menuItem("Selections", tabName = "selections", icon = icon("feather-alt"),
               selectInput("selectOrg", "Select Organisation:", manualOrgs),
               selectInput("selectYear", "Select Year:", year),
               
               div(style="display: inline-block; vertical-align:top; width: 100px;",
                   actionButton("reset", "Reset", icon("times-circle"), 
                                style="color: #FFFFFF; background-color: #005EB8; border-color: #768692")),
               
               div(style="display: inline-block; vertical-align:top; width: 100px;",
                   actionButton("submit", "Submit", icon("paper-plane"), 
                                style="color: #FFFFFF; background-color: #005EB8; border-color: #768692"))
               ),
      
      menuItem("About", tabName = "about", icon = icon("crow"),
               p(""),
               h3("Why do I do this to myself?"),
               p("Learning this is something to do. I am a glutton for punishment.")
               )
    )
  ),
  dashboardBody(
    tableOutput('testTabCnts')
  )
)
