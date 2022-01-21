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
library(dashboardthemes)

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
    menuItem('Incidents - Counts', tabName = 'incsCounts', icon = icon('ambulance')),
    menuItem('Incidents - Response Times', tabName = 'incsRTs', icon = icon('shipping-fast')),
    menuItem('HCP - Counts', tabName = 'hcpCounts', icon = icon('ambulance')),
    menuItem('HCP - Response Times', tabName = 'hcpRTs', icon = icon('shipping-fast')),
    menuItem('IFT - Counts', tabName = 'iftCounts', icon = icon('ambulance')),
    menuItem('IFT - Response Times', tabName = 'iftRTs', icon = icon('shipping-fast'))
    
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "flat_red"
  ),
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
    # to resolve height overlap: https://stackoverflow.com/questions/46259208/shiny-dashboard-mainpanel-height-issue
    tabItem(tabName = 'incsRTs',
            fluidRow(
              tabBox(
                width = 12,
                height = NULL,
                side = 'right',
                title = tagList(shiny::icon('shipping-fast'), 'Response Times'),
                selected = 'C1',
                id = "tabsetRTs", #height = "250px",
                tabPanel('C4', 'C4 Response Times',
                         fluidRow(column(6, plotOutput('C4RTMean')),
                                  column(6, plotOutput('C4RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C4RTTbl')))
                         ),
                tabPanel('C3', 'C3 Response Times',
                         fluidRow(column(6, plotOutput('C3RTMean')),
                                  column(6, plotOutput('C3RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C3RTTbl')))
                ),
                tabPanel('C2', 'C2 Response Times',
                         fluidRow(column(6, plotOutput('C2RTMean')),
                                  column(6, plotOutput('C2RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C2RTTbl')))
                ),
                tabPanel('C1', 'C1 Response Times',
                         fluidRow(column(6, plotOutput('C1RTMean')),
                                  column(6, plotOutput('C1RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C1RTTbl')))
                )
              )
            )
    ),
    
    
    
    
    
    
    
    tabItem(tabName = 'hcpCounts',
            h2('Incident Counts'),
            p('This page details elements of incidents by category:'),
            fluidRow(column(6, plotOutput('HCPincsCounts')),
                     column(6, plotOutput('HCPincsProps'))
            ),
            p(''),
            fluidRow(column(12,tableOutput('incsHCPTbl'))
            )
    ),
    
    
    tabItem(tabName = 'hcpRTs',
            fluidRow(
              tabBox(
                width = 12,
                height = NULL,
                side = 'right',
                title = tagList(shiny::icon('shipping-fast'), 'Response Times'),
                selected = 'HCP Level 1',
                id = "tabsetRTs", #height = "250px",
                tabPanel('HCP Level 4', 'HCP Level 4 Response Times',
                         fluidRow(column(6, plotOutput('HCP4RTMean')),
                                  column(6, plotOutput('HCP4RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCP4RTTbl')))
                ),
                tabPanel('HCP Level 3', 'HCP Level 3 Response Times',
                         fluidRow(column(6, plotOutput('HCP3RTMean')),
                                  column(6, plotOutput('HCP3RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCP3RTTbl')))
                ),
                tabPanel('HCP Level 2', 'HCP Level 2 Response Times',
                         fluidRow(column(6, plotOutput('HCP2RTMean')),
                                  column(6, plotOutput('HCP2RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCP2RTTbl')))
                ),
                tabPanel('HCP Level 1', 'HCP Level 1 Response Times',
                         fluidRow(column(6, plotOutput('HCP1RTMean')),
                                  column(6, plotOutput('HCP1RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCP1RTTbl')))
                )
              )
            )
    ),
    
    
    
    
    
    
    
    tabItem(tabName = 'iftCounts',
            h2('Incident Counts'),
            p('This page details elements of incidents by category:'),
            fluidRow(column(6, plotOutput('IFTincsCounts')),
                     column(6, plotOutput('IFTincsProps'))
            ),
            p(''),
            fluidRow(column(12,tableOutput('incsLFTTbl'))
            )
    ),
    
    tabItem(tabName = 'iftRTs',
            fluidRow(
              tabBox(
                width = 12,
                height = NULL,
                side = 'right',
                title = tagList(shiny::icon('shipping-fast'), 'Response Times'),
                selected = 'IFT Level 1',
                id = "tabsetRTs", #height = "250px",
                tabPanel('IFT Level 4', 'IFT Level 4 Response Times',
                         fluidRow(column(6, plotOutput('IFT4RTMean')),
                                  column(6, plotOutput('IFT4RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('IFT4RTTbl')))
                ),
                tabPanel('IFT Level 3', 'IFT Level 3 Response Times',
                         fluidRow(column(6, plotOutput('IFT3RTMean')),
                                  column(6, plotOutput('IFT3RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('IFT3RTTbl')))
                ),
                tabPanel('IFT Level 2', 'IFT Level 2 Response Times',
                         fluidRow(column(6, plotOutput('IFT2RTMean')),
                                  column(6, plotOutput('IFT2RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('IFT2RTTbl')))
                ),
                tabPanel('IFT Level 1', 'IFT Level 1 Response Times',
                         fluidRow(column(6, plotOutput('IFT1RTMean')),
                                  column(6, plotOutput('IFT1RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('IFT1RTTbl')))
                )
              )
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