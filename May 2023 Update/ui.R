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
# library(plotly)
# library(gtools)
# library(stringr)
# library(scales)
# library(hms)
library(dashboardthemes)

year <- c('2023', '2022', '2021', '2020', '2019', '2018')
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
             textInput("urlIn", label = "URL of data:", value = 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/04/AmbSYS-up-to-Mar-2023.csv'), #'aqi2023.csv'), # 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/01/AmbSYS-up-to-Dec21.csv'),
             selectInput("menuOrg", "Select Organisation:", manualOrgs),
             selectInput("menuYear", "Select Year:", year),
             
             div(style="display: inline-block; vertical-align:top; width: 100px;",
                 actionButton("reset", "Reset", icon("times-circle"), 
                              style="color: #FFFFFF; background-color: #005EB8; border-color: #768692")),
             
             div(style="display: inline-block; vertical-align:top; width: 100px;",
                 actionButton("submit", "Submit", icon("paper-plane"), 
                              style="color: #FFFFFF; background-color: #005EB8; border-color: #768692"))
    ),
    
# Menu items on the left menu
    menuItem('Call Answering', tabName = 'callAns', icon = icon('phone')),
    menuItem('Incidents - Counts', tabName = 'incsCounts', icon = icon('ambulance')),
    menuItem('Incidents - Response Times', tabName = 'incsRTs', icon = icon('shipping-fast')),
    menuItem('HCP/IFT - Counts', tabName = 'hcpiftCounts', icon = icon('ambulance')),
    menuItem('HCP/IFT - Response Times', tabName = 'hcpiftRTs', icon = icon('shipping-fast'))
    
    
    
  )
)

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "flat_red"
  ),
  tabItems(

# Menu: Call Answering    
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
    
# Menu: Incidents - Counts    
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

# Menu: Incidents - Response Times
    # to resolve height overlap: https://stackoverflow.com/questions/46259208/shiny-dashboard-mainpanel-height-issue
    tabItem(tabName = 'incsRTs',
            fluidRow(
              tabBox(
                width = 12,
                height = NULL,
                side = 'right',
                title = tagList(shiny::icon('shipping-fast'), 'Response Times'),
                selected = 'Category 1',
                id = "tabsetRTs", #height = "250px",
                tabPanel('Category 4', 'Category 4 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('C4RTMean')),
                                  column(6, plotOutput('C4RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C4RTTbl')))
                         ),
                tabPanel('Category 3', 'Category 3 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('C3RTMean')),
                                  column(6, plotOutput('C3RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C3RTTbl')))
                ),
                tabPanel('Category 2', 'Category 2 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('C2RTMean')),
                                  column(6, plotOutput('C2RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C2RTTbl')))
                ),
                tabPanel('Category 1', 'Category 1 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('C1RTMean')),
                                  column(6, plotOutput('C1RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('C1RTTbl')))
                )
              )
            )
    ),
    
    
#Menu: HCP/IFT - Counts    
    tabItem(tabName = 'hcpiftCounts',
            fluidRow(
              tabBox(
                width = 12,
                height = NULL,
                side = 'right',
                title = tagList(shiny::icon('ambulance'), 'HCP/IFT Incident Counts'),
                selected = 'HCP',
                id = "tabsetRTs", #height = "250px",
                tabPanel('IFT', 'IFT Incidents',
                         p(''),
                         fluidRow(column(6, plotOutput('IFTincsCounts')),
                                  column(6, plotOutput('IFTincsProps'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('incsLFTTbl')))
                ),
                tabPanel('HCP', 'HCP Incidents',
                         p(''),
                         fluidRow(column(6, plotOutput('HCPincsCounts')),
                                  column(6, plotOutput('HCPincsProps'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('incsHCPTbl')))
                )
              )
            )
    ),
    
    
# Menu: HCP/IFT - Response Times   
    tabItem(tabName = 'hcpiftRTs',
            fluidRow(
              tabBox(
                width = 12,
                height = NULL,
                side = 'right',
                title = tagList(shiny::icon('shipping-fast'), 'HCP/IFT Response Times'),
                selected = 'HCP/IFT Level 1',
                id = "tabsetRTs", #height = "250px",
                tabPanel('HCP/IFT Level 4', 'HCP/IFT Level 4 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('HCPIFT4RTMean')),
                                  column(6, plotOutput('HCPIFT4RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCPIFT4RTTbl')))
                ),
                tabPanel('HCP/IFT Level 3', 'HCP/IFT Level 3 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('HCPIFT3RTMean')),
                                  column(6, plotOutput('HCPIFT3RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCPIFT3RTTbl')))
                ),
                tabPanel('HCP/IFT Level 2', 'HCP/IFT Level 2 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('HCPIFT2RTMean')),
                                  column(6, plotOutput('HCPIFT2RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCPIFT2RTTbl')))
                ),
                tabPanel('HCP/IFT Level 1', 'HCP/IFT Level 1 Response Times',
                         p(''),
                         fluidRow(column(6, plotOutput('HCPIFT1RTMean')),
                                  column(6, plotOutput('HCPIFT1RT90th'))
                         ),
                         p(''),
                         fluidRow(column(12, tableOutput('HCPIFT1RTTbl')))
                )
              )
            )
    ),
    
# The ABOUT page    
    tabItem(tabName = 'about',
            h2('About the AQI View-R'),
            h3('Updated: 01/05/2023'),
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
              Selections tab (though I have set this to the latest default). You can then select 
              which Organisation you are interested in
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
