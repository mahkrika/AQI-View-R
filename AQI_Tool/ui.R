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
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(gtools)
library(stringr)
library(scales)
library(hms)

year <- c('2021', '2020', '2019', '2018')
manualOrgs <- c('England', 'North East and Yorkshire', 'North West', 'Midlands', 'East of England', 'London',
                'South East', 'South West', 'EAST MIDLANDS AMBULANCE SERVICE NHS TRUST', 'EAST OF ENGLAND AMBULANCE SERVICE NHS TRUST',
                'ISLE OF WIGHT NHS TRUST', 'LONDON AMBULANCE SERVICE NHS TRUST', 'NORTH EAST AMBULANCE SERVICE NHS FOUNDATION TRUST',
                'NORTH WEST AMBULANCE SERVICE NHS TRUST', 'SOUTH CENTRAL AMBULANCE SERVICE NHS FOUNDATION TRUST',
                'SOUTH EAST COAST AMBULANCE SERVICE NHS FOUNDATION TRUST', 'SOUTH WESTERN AMBULANCE SERVICE NHS FOUNDATION TRUST',
                'WEST MIDLANDS AMBULANCE SERVICE UNIVERSITY NHS FOUNDATION TRUST', 'YORKSHIRE AMBULANCE SERVICE NHS TRUST',
                'WEST MIDLANDS AMBULANCE SERVICE NHS FOUNDATION TRUST')


sidebar <-   dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("crow")),
    menuItem("Selections", tabName = "selections", icon = icon("feather-alt"),
             textInput("urlIn", label = "URL of data:"),
             selectInput("menuOrg", "Select Organisation:", manualOrgs),
             selectInput("menuYear", "Select Year:", year),
             
             div(style="display: inline-block; vertical-align:top; width: 100px;",
                 actionButton("reset", "Reset", icon("times-circle"), 
                              style="color: #FFFFFF; background-color: #005EB8; border-color: #768692")),
             
             div(style="display: inline-block; vertical-align:top; width: 100px;",
                 actionButton("submit", "Submit", icon("paper-plane"), 
                              style="color: #FFFFFF; background-color: #005EB8; border-color: #768692"))
    ),
    
    #menuItem('Calls Answered - Counts', tabName = 'callAns', icon = icon('phone')),
    #menuItem('Calls Answered - Times', tabName = 'callAnsTime', icon = icon('phone')),
    menuItem('Call Answering', tabName = 'callAns', icon = icon('phone')),
    menuItem('Incidents - Counts', tabName = 'incsCounts', icon = icon('ambulance'))
    
    
  )
)

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = 'callAns',
            h2('Call Answering'),
            p('This page details elements of call answering figures:'),
            fluidRow(column(6, plotOutput('callsAns')),
                     column(6, plotOutput('callsAnsTime'))
            ),
            p(''),
            fluidRow(column(12,tableOutput('callAnsTbl'))
            )
    ),
    
    tabItem(tabName = 'incsCounts',
            h2('Incident Counts'),
            p('This page details elements of incidents by category:'),
            fluidRow(column(6, plotOutput('incsCounts')),
                     column(6, plotOutput('incsProps'))
                     ),
            p(''),
            fluidRow(column(12,tableOutput('incsCatTbl'))
            )
    ),
    
    tabItem(tabName = 'about',
            h2('About the AQI View-R'),
            p(''),
            p('This tool is primarily built to aid my learning of R. However, having worked
              within the NHS for several years it still surprises me to see so much of the data
              and processes buried away. The Ambulance Quality Indicators (AQIs) are a dataset
              that are released each month and cover a wide range of measures as recorded by ambulance
              trusts.'),
            p('The datasets (although entirely open and accessible) can be difficult to navigate,
              and even more challenging to extract pertinent information from. This tool aims to
              assist in this process by presenting some of the core information from the collection
              in an easy to use tool.'),
            p('To use the tool you must (presently, though perhaps I can develop this further) copy
              the link location from the AQI datasets page and paste it into URL field in the
              Selections tab. You can then select which Organisation you are interested in
              (also National or Regional), and finally which year you are interested in. Click the
              Submit button and the data will be gathered and presented in the appropriate tabs.'),
            p('This is very much a work in progress and will be developed in my free time. 
              If you are interested, links will be provided to my
              Github account so that you can download/view the code that underpins this
              development.'),
            p('The data is presented without any guarantees of accuracy, use at your own risk!')
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "AQI View-R"),
  sidebar,
  body
)