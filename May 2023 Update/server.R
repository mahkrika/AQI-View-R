#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#
#
# Useful notes:
# Find differences between files: https://stackoverflow.com/a/53333988
#
# R Error message: object of type 'closure' is not subsettable
# What a pita this can be. Had this when developing to use df_field instead of hard-coding all the fields.
# Instead of refering to columns using tbl$name instead use tbl[n] and it resolves:
# https://coolbutuseless.github.io/2019/02/12/object-of-type-closure-is-not-subsettable/
#
# Easy way to combine different columns in a df:
# https://stackoverflow.com/a/18115601
#
# aes_string() is now deprecated
# was used in each plot (e.g. "aes_string(x = xn)") but have replaced with new tidy compliant
# option of aes(x = .data[[xn]].
# https://stackoverflow.com/a/75217062
#
# For display of times as HH:mm:ss use lubridate to convert from integer to time:
# https://stackoverflow.com/a/27313681
#
#Problem: As of March 2023 file - now has *some* fields that are integers formatted with thousand comma separators.
# These fields are therefore read as chr fields and not integers...
# These commas need removing and the field converting to integers (present process just converts all data fields).
# The below works but if you do [,6:ncols] then it introduces NAs by coercion so screws it up.
#inFile[,6] <- as.integer(gsub(",", "", inFile[,6]))
# Solution: Have to isolate the  columns and treat them separately within a loop. See: Inputs > inData
# Also note that because of this you have to name this df differently and then recall it with the correct name
# in a later step. Otherwise you run into issues: 
#     Warning: Error in UseMethod: no applicable method for 'filter' applied to an object of class "NULL".
# So, do the loop as inFileStart and then build new (and later used) variable of inFile <- inFileStart...


library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(dashboardthemes)
library(openxlsx)
library(lubridate)
library(gtools)
library(stringr)
library(scales)
#library(devtools)
#devtools::install_version("MASS", "7.3-51.1")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
#Replacing all the hard-coded measures (A0, A1, etc.) and the measure names (C1 Incident, C2 Incident, etc.)
# with fields as pulled from the AQI landing page.
  # readxl does not function easily for a URL - using openxlsx instead: https://stackoverflow.com/a/68009945
  df_field <- read.xlsx('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2023/02/20220906-AmbSYS-indicator-list.xlsx')
  df_field <- df_field[3:125,1:3] # trim the file for only the relevant fields
  # Note! I use [n] syntax rather than $name syntax because Shiny seems to have been with that outside of events etc.
  df_field[4] <- paste(df_field[,2], '-', df_field[,3])
  df_field <- df_field[,c(1,4)]
  colnames(df_field) <- c('Field', 'Field_Detail')
  
  df_field <- data.frame(
    Field = df_field[,1],
    Field_Detail = df_field[,2]
  )
  
  field_key <- df_field

  
  ################################################################################
  # Reset parameters with Reset button # 
  observeEvent(input$reset, {
    updateSelectInput(session = getDefaultReactiveDomain(), 'menuOrg', selected = 'England');
    updateSelectInput(session = getDefaultReactiveDomain(), 'menuYear', selected = '2023');
  })
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## Inputs
  
  # inData takes the data from the input URL (urlIn) that is in the ui.R
  inData <- eventReactive(input$submit, {
    inLoc <- input$urlIn
    inFileStart <- read.csv(inLoc, header = TRUE, stringsAsFactors = FALSE, na.strings = c('.', '-'))
    # See notes, but tldr is new file has commas making fields read as chr. 
    # Replace the comma and convert fields.
      for (i in 6:ncol(inFileStart)){
        inFileStart[,i] <- as.integer(gsub(",", "", inFileStart[,i]))
      }
    inFile <- inFileStart # As noted above in notes - if you don't do this, it errors...
  })
  
  # selData refers to the selected data based upon the menu selected (year and area/organisation)
  selData <- eventReactive(input$submit, {
    df <- inData() %>% 
      filter(Year == input$menuYear & Org.Name == input$menuOrg)
  })
  
  #selDataL refers to the 'longer' version of the selected date (selData). This performs
  # a pivot for all columns from 6 onwards (starting with A0 - all previous are organisations/codes etc.).
  # It takes all of these fields and pivots so that the first 5 columns are regular, then every row
  # from consists of an individual measure A0, A1 etc. (changes from columns to rows).
  selDataL <- eventReactive(input$submit, {
    df <- selData() %>% 
      pivot_longer(cols = 6:ncol(selData()), names_to = 'Field', values_to = 'Values')
    df <- merge(df, field_key, by = 'Field')
  })
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graph: Line
  rendLineGsub <- function(xn, yn, useFactor, useFields, useTitle, useX, useY){
    renderPlot({
      dfa <- selDataL() %>% 
        dplyr::filter(
          Field %in% useFields
        )
      # initially used subset() but ran into difficulties:
      # https://stackoverflow.com/questions/17075529/subset-based-on-variable-column-name
      p <- ggplot(data = dfa,
                  aes(x = .data[[xn]], 
                      y = .data[[yn]], 
                      group = .data[[useFactor]], 
                      colour = .data[[useFactor]]
                      )) +
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
        labs(title = useTitle, #paste(field_key$Field, field_key$Field_Detail),#
             x = useX,
             y = useY
        ) + 
        theme(legend.position="bottom") + 
        theme(legend.position="bottom") + 
        guides(col = guide_legend(title = "",
                                  nrow=2, 
                                  byrow = TRUE
        ))
      p
    },
    width = "auto",
    height = "auto"
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Graph: Area - to shade the area under lines   
  rendAreaIdentitysub <- function(xn, yn, useFactor, useFields, useTitle, useX, useY, selFields, useFactorNames){
    renderPlot({
      dfa <- selDataL() %>% 
        dplyr::filter(
          Field %in% useFields
        )
      dfa$Field2 <- factor(dfa$Field, levels = rev(useFields))
      p <- ggplot(data = dfa, aes(x = .data[[xn]],
                                  y = .data[[yn]],
                                  group = .data[[selFields]]
                                  )) +
        geom_area(aes(fill = Field2), position = 'identity') +
        geom_line(aes(group = Field2)) +
        scale_fill_viridis_d(option = 'C',
                             labels = useFactorNames,
                             name = '') + #Set the legened title!
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
        labs(title = useTitle,
             x = useX,
             y = useY
        ) + 
        theme(legend.position="bottom") + 
        guides(fill = guide_legend(nrow=2,
                                   byrow = TRUE
        )
        )
      
      p
    }) 
  }
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  ## Graph: Point
  rendPointGsub <- function(xn, yn, useFactor, useFields, useTitle, useX, useY){
    renderPlot({
      dfa <- selDataL() %>% 
        dplyr::filter(
          Field %in% useFields
        )
      # initially used subset() but ran into difficulties:
      # https://stackoverflow.com/questions/17075529/subset-based-on-variable-column-name
      p <- ggplot(data = dfa,
                  aes(x = .data[[xn]], 
                      y = .data[[yn]], 
                      group = .data[[useFactor]], 
                      colour = .data[[useFactor]]
                      )) +
        geom_point() +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
        labs(title = useTitle,
             x = useX,
             y = useY
        ) + 
        theme(legend.position="bottom") + 
        guides(col = guide_legend(title = "",
                                  nrow=2, 
                                  byrow = TRUE
        ))
      p
    },
    width = "auto",
    height = "auto"
    )
  }

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
  ## Graph: Column
  rendColGsub <- function(xn, yn, useFactor, useFields, useTitle, useX, useY, useFactorNames, useFill){
    renderPlot({
      dfa <- selDataL() %>% 
        dplyr::filter(
          Field %in% useFields
        )
      dfa$Field2 <- factor(dfa$Field, levels = useFields)
      p <- ggplot(data = dfa,
                  aes(x = .data[[xn]], 
                      y = .data[[yn]]
                      #fill = .data[[useFactor]]
                  )) +
        {if (useFill == 1) geom_col(
          aes(fill = Field2),
          colour = "black", 
          alpha = 1,
          position = position_stack(reverse = TRUE)
          #position = position_fill(reverse = TRUE)
          #,position = 'identity'
          # https://stackoverflow.com/questions/42710056/reverse-stacked-bar-order
          #,position = "dodge" # To put side-by-side
        )} +
        {if (useFill == 2) geom_col(
          aes(fill = Field2),
          colour = "black", 
          alpha = 1,
          #position = position_stack(reverse = TRUE)
          position = position_fill(reverse = TRUE)
        )} +
        scale_fill_viridis_d(option = 'C',
                             labels = useFactorNames,
                             name = '') +
        #scale_fill_hue(direction = -1) +
        
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
        labs(title = useTitle,
             x = useX,
             y = useY
        ) + 
        theme(legend.position="bottom") + 
        guides(fill = guide_legend(nrow=2,
                                   byrow = TRUE
        )
        )
      p
    },
    width = "auto",
    height = "auto"
    )
  }
  
  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
## Outputs - oh, so many outputs. Maybe there is an easier way of doing this- build a function?
  
# Graphs
  # Line graphs
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  output$callsAns <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A1', 
                                  'Number of calls answered', 'Month', 'Count of calls answered')
  
  output$C1RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A25', 
                                  'Mean Response Time - C1', 'Month', 'Seconds')
  
  output$C1RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A26', 
                                  '90th Centile Response Time - C1', 'Month', 'Seconds') 
  
  output$C2RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A31', 
                                  'Mean Response Time - C2', 'Month', 'Seconds')
  
  output$C2RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A32', 
                                  '90th Centile Response Time - C2', 'Month', 'Seconds') 
  
  output$C3RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A34', 
                                  'Mean Response Time - C3', 'Month', 'Seconds')
  
  output$C3RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A35', 
                                  '90th Centile Response Time - C3', 'Month', 'Seconds') 
  
  output$C4RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A37', 
                                  'Mean Response Time - C4', 'Month', 'Seconds')
  
  output$C4RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', 'A38', 
                                  '90th Centile Response Time - C4', 'Month', 'Seconds') 
  
  output$HCPIFT1RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A83', 'A95'), 
                                       'Mean Response Time', 'Month', 'Seconds')
  
  output$HCPIFT1RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A84', 'A96'), 
                                       '90th Centile Response Time', 'Month', 'Seconds') 
  
  output$HCPIFT2RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A86', 'A98'), 
                                       'Mean Response Time', 'Month', 'Seconds')
  
  output$HCPIFT2RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A87', 'A99'), 
                                       '90th Centile Response Time', 'Month', 'Seconds') 
  
  output$HCPIFT3RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A89', 'A101'), 
                                       'Mean Response Time', 'Month', 'Seconds')
  
  output$HCPIFT3RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A90', 'A102'), 
                                       '90th Centile Response Time', 'Month', 'Seconds') 
  
  output$HCPIFT4RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A92', 'A104'), 
                                       'Mean Response Time', 'Month', 'Seconds')
  
  output$HCPIFT4RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A93', 'A105'), 
                                       '90th Centile Response Time', 'Month', 'Seconds') 
  
  
  
  # Area graphs
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$callsAnsTime <- rendAreaIdentitysub('Month', 'Values', 'Field_Detail', c('A4', 'A3', 'A114', 'A5', 'A6'),
                                             'Call answer times', 'Month', 'Seconds', 'Field2',
                                             c('99th centile call answer time',
                                               '95th centile call answer time',
                                               '90th centile call answer time',
                                               'Mean call answer time',
                                               'Median call answer time'))
  
  # COlumn graphs
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  output$incsCounts <- rendColGsub('Month', 'Values', 'Field', c('A8', 'A10', 'A11', 'A12'), 
                                   'Number of Incidents per Category', 'Month', 'Incidents',
                                   c('C1 incidents',
                                     'C2 incidents',
                                     'C3 incidents',
                                     'C4 incidents'), 1)
  
  output$incsProps <- rendColGsub('Month', 'Values', 'Field', c('A8', 'A10', 'A11', 'A12'), 
                                  'Proportion of Category Incidents', 'Month', 'Proportion',
                                  c('C1 incidents',
                                    'C2 incidents',
                                    'C3 incidents',
                                    'C4 incidents'), 2)
  
  output$HCPincsCounts <- rendColGsub('Month', 'Values', 'Field', c('A74', 'A75', 'A76', 'A77'), 
                                      'Number of Incidents per Category', 'Month', 'Incidents',
                                      c('HCP Level 1 incidents',
                                        'HCP Level 2 incidents',
                                        'HCP Level 3 incidents',
                                        'HCP Level 4 incidents'), 1)
  
  output$HCPincsProps <- rendColGsub('Month', 'Values', 'Field', c('A74', 'A75', 'A76', 'A77'), 
                                     'Proportion of Category Incidents', 'Month', 'Proportion',
                                     c('HCP Level 1 incidents',
                                       'HCP Level 2 incidents',
                                       'HCP Level 3 incidents',
                                       'HCP Level 4 incidents'), 2)
  
  
  output$IFTincsCounts <- rendColGsub('Month', 'Values', 'Field', c('A78', 'A79', 'A80', 'A81'), 
                                      'Number of Incidents per Category', 'Month', 'Incidents',
                                      c('IFT Level 1 incidents',
                                        'IFT Level 2 incidents',
                                        'IFT Level 3 incidents',
                                        'IFT Level 4 incidents'), 1)
  
  output$IFTincsProps <- rendColGsub('Month', 'Values', 'Field', c('A78', 'A79', 'A80', 'A81'), 
                                     'Proportion of Category Incidents', 'Month', 'Proportion',
                                     c('IFT Level 1 incidents',
                                       'IFT Level 2 incidents',
                                       'IFT Level 3 incidents',
                                       'IFT Level 4 incidents'), 2)
  
  output$C1HCPIFT <- rendColGsub('Month', 'Values', 'Field', c('A115', 'A74', 'A78'), 
                                 'Number of Incidents per Category', 'Month', 'Incidents',
                                 c('C1 incidents excluding HCP and IFT',
                                   'HCP Level 1 incidents',
                                   'IFT Level 1 incidents'), 1)
  
  output$C1HCPIFTProps <- rendColGsub('Month', 'Values', 'Field', c('A115', 'A74', 'A78'), 
                                      'Proportion of Category Incidents', 'Month', 'Proportion',
                                      c('C1 incidents excluding HCP and IFT',
                                        'HCP Level 1 incidents',
                                        'IFT Level 1 incidents'), 2)
  
  output$C2HCPIFT <- rendColGsub('Month', 'Values', 'Field', c('A119', 'A75', 'A79'), 
                                 'Number of Incidents per Category', 'Month', 'Incidents',
                                 c('C2 incidents excluding HCP and IFT',
                                   'HCP Level 2 incidents',
                                   'IFT Level 2 incidents'), 1)
  
  output$C2HCPIFTProps <- rendColGsub('Month', 'Values', 'Field', c('A119', 'A75', 'A79'), 
                                      'Proportion of Category Incidents', 'Month', 'Proportion',
                                      c('C2 incidents excluding HCP and IFT',
                                        'HCP Level 2 incidents',
                                        'IFT Level 2 incidents'), 2)
  # Tables
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  # Notes on my dirty solution to getting thousand comma separators for visibilty - the irony being that I complain about
  # the AQI dataset having problematic commas is not lost...
  # Anyway, where a table contains only integers this is not a problem, do a mutate of Ax = comma(Ax) and all is fine.
  # But where a table has a mutate with the same field of Ax used for a proportion as well then this can cause problems. I have
  # no idea why you can't just do a mutate followed by another %>% mutate, but you can't so another solution is needed. It's
  # very, very dirty but it works. Mutate an extra variation of Ax_ = comma(Ax) then use this as your reference field for the
  # properly named field entry in the table. BUT! doing this means that you now have both Ax and the Ax_ fields in your table
  # though I don't know why at all! The crafty bit is doing a select at the end to drop these Ax fields and keep the Ax_
  # fields. I have no doubt that it is a waste of resource, but it does work and satisfies the job.
  
  # For RTs to be displayed as a time use lubridate - see notes at the top for more detail on this.
  
  # Call Answering
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$callAnsTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A1', 'A3', 'A4', 'A114', 'A5', 'A6') %>% 
      mutate(A1 = comma(A1),
             A3 = seconds_to_period(A3),
             A3 = sprintf('%02d:%02d:%02d', A3@hour, minute(A3), second(A3)),
             A4 = seconds_to_period(A4),
             A4 = sprintf('%02d:%02d:%02d', A4@hour, minute(A4), second(A4)),
             A114 = seconds_to_period(A114),
             A114 = sprintf('%02d:%02d:%02d', A114@hour, minute(A114), second(A114)),
             A5 = seconds_to_period(A5),
             A5 = sprintf('%02d:%02d:%02d', A5@hour, minute(A5), second(A5)),
             A6 = seconds_to_period(A6),
             A6 = sprintf('%02d:%02d:%02d', A6@hour, minute(A6), second(A6))
      ) %>% 
      dplyr::rename('Number of calls answered' = A1,
                    'Mean call answer time' = A3,
                    'Median call answer time' = A4,
                    '90th centile call answer time' = A114,
                    '95th centile call answer time' = A5,
                    '99th centile call answer time' = A6) %>% 
      arrange(Month)
  }, striped = TRUE)  
  
  # Response Times: C1
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$C1RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A25', 'A26', 'A8') %>% 
      mutate(A25 = seconds_to_period(A25),
             A25 = sprintf('%02d:%02d:%02d', A25@hour, minute(A25), second(A25)),
             A26 = seconds_to_period(A26),
             A26 = sprintf('%02d:%02d:%02d', A26@hour, minute(A26), second(A26)),
             A8 = comma(A8)) %>% 
      dplyr::rename('Mean response time: C1' = A25,
                    '90th centile response time: C1' = A26,
                    'C1 incidents' = A8) %>% 
      arrange(Month)
  }, striped = TRUE) 

  # Response Times: C2
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
  output$C2RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A31', 'A32', 'A10') %>% 
      mutate(A31 = seconds_to_period(A31),
             A31 = sprintf('%02d:%02d:%02d', A31@hour, minute(A31), second(A31)),
             A32 = seconds_to_period(A32),
             A32 = sprintf('%02d:%02d:%02d', A32@hour, minute(A32), second(A32)),
             A10 = comma(A10)
             #A31_ = as_hms(A31)
             ) %>% 
      dplyr::rename('Mean response time: C2' = A31,
                    '90th centile response time: C2' = A32,
                    'C2 Incidents' = A10) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  # Response Times: C3
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$C3RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A34', 'A35', 'A11') %>% 
      mutate(A34 = seconds_to_period(A34),
             A34 = sprintf('%02d:%02d:%02d', A34@hour, minute(A34), second(A34)),
             A35 = seconds_to_period(A35),
             A35 = sprintf('%02d:%02d:%02d', A35@hour, minute(A35), second(A35)),
             A11 = comma(A11)) %>% 
      dplyr::rename('Mean response time: C3' = A34,
                    '90th centile response time: C3' = A35,
                    'C3 Incidents' = A11) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  # Response Times: C4
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$C4RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A37', 'A38', 'A12') %>% 
      mutate(A37 = seconds_to_period(A37),
             A37 = sprintf('%02d:%02d:%02d', A37@hour, minute(A37), second(A37)),
             A38 = seconds_to_period(A38),
             A38 = sprintf('%02d:%02d:%02d', A38@hour, minute(A38), second(A38)),
             A12 = comma(A12)) %>% 
      dplyr::rename('Mean response time: C4' = A37,
                    '90th centile response time: C4' = A38,
                    'C4 Incidents' = A12) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  # Response Times: HCP/IFT Level 1
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$HCPIFT1RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A83', 'A95', 'A84', 'A96', 'A74', 'A78') %>% 
      mutate(A83 = seconds_to_period(A83),
             A83 = sprintf('%02d:%02d:%02d', A83@hour, minute(A83), second(A83)),
             A95 = seconds_to_period(A95),
             A95 = sprintf('%02d:%02d:%02d', A95@hour, minute(A95), second(A95)),
             A84 = seconds_to_period(A84),
             A84 = sprintf('%02d:%02d:%02d', A84@hour, minute(A84), second(A84)),
             A96 = seconds_to_period(A96),
             A96 = sprintf('%02d:%02d:%02d', A96@hour, minute(A96), second(A96)),
             A74 = comma(A74),
             A78 = comma(A78)
      ) %>% 
      dplyr::rename('Mean: HCP Level 1' = A83,
                    'Mean: IFT Level 1' = A95,
                    '90th centile: HCP Level 1' = A84,
                    '90th centile: IFT Level 1' = A96,
                    'HCP Level 1 Incidents' = A74,
                    'IFT Level 1 Incidents' = A78) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  # Response Times: HCP/IFT Level 2
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$HCPIFT2RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A86', 'A98', 'A87', 'A99', 'A75', 'A79') %>%
      mutate(A86 = seconds_to_period(A86),
             A86 = sprintf('%02d:%02d:%02d', A86@hour, minute(A86), second(A86)),
             A98 = seconds_to_period(A98),
             A98 = sprintf('%02d:%02d:%02d', A98@hour, minute(A98), second(A98)),
             A87 = seconds_to_period(A87),
             A87 = sprintf('%02d:%02d:%02d', A87@hour, minute(A87), second(A87)),
             A99 = seconds_to_period(A99),
             A99 = sprintf('%02d:%02d:%02d', A99@hour, minute(A99), second(A99)),
             A75 = comma(A75),
             A79 = comma(A79)
      ) %>% 
      dplyr::rename('Mean: HCP Level 2' = A86,
                    'Mean: IFT Level 2' = A98,
                    '90th centile: HCP Level 2' = A87,
                    '90th centile: IFT Level 2' = A99,
                    'HCP Level 2 Incidents' = A75,
                    'IFT Level 2 Incidents' = A79) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  # Response Times: HCP/IFT Level 3
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$HCPIFT3RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A89', 'A101', 'A90', 'A102', 'A76', 'A80') %>% 
      mutate(A89 = seconds_to_period(A89),
             A89 = sprintf('%02d:%02d:%02d', A89@hour, minute(A89), second(A89)),
             A101 = seconds_to_period(A101),
             A101 = sprintf('%02d:%02d:%02d', A101@hour, minute(A101), second(A101)),
             A90 = seconds_to_period(A90),
             A90 = sprintf('%02d:%02d:%02d', A90@hour, minute(A90), second(A90)),
             A102 = seconds_to_period(A102),
             A102 = sprintf('%02d:%02d:%02d', A102@hour, minute(A102), second(A102)),
             A76 = comma(A76),
             A80 = comma(A80)
      ) %>% 
      dplyr::rename('Mean: HCP Level 3' = A89,
                    'Mean: IFT Level 3' = A101,
                    '90th centile: HCP Level 3' = A90,
                    '90th centile: IFT Level 3' = A102,
                    'HCP Level 3 Incidents' = A76,
                    'IFT Level 3 Incidents' = A80) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  # Response Times: HCP/IFT Level 4
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$HCPIFT4RTTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A92', 'A104', 'A93', 'A105', 'A77', 'A81') %>% 
      mutate(A92 = seconds_to_period(A92),
             A92 = sprintf('%02d:%02d:%02d', A92@hour, minute(A92), second(A92)),
             A104 = seconds_to_period(A104),
             A104 = sprintf('%02d:%02d:%02d', A104@hour, minute(A104), second(A104)),
             A93 = seconds_to_period(A93),
             A93 = sprintf('%02d:%02d:%02d', A93@hour, minute(A93), second(A93)),
             A105 = seconds_to_period(A105),
             A105 = sprintf('%02d:%02d:%02d', A105@hour, minute(A105), second(A105)),
             A77 = comma(A77),
             A81 = comma(A81)
      ) %>% 
      dplyr::rename('Mean: HCP Level 4' = A92,
                    'Mean: IFT Level 4' = A104,
                    '90th centile: HCP Level 4' = A93,
                    '90th centile: IFT Level 4' = A105,
                    'HCP Level 4 Incidents' = A77,
                    'IFT Level 4 Incidents' = A81) %>% 
      arrange(Month)
  }, striped = TRUE) 
  
  
  # Incidents: C1-4
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$incsCatTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             c('A8', 'A10', 'A11', 'A12')) %>% 
      dplyr::group_by(Month) %>% 
      dplyr::mutate(
             A8_ = comma(A8),
             A10_ = comma(A10),
             A11_ = comma(A11),
             A12_ = comma(A12),
             C1_Perc = A8 / sum(A8+A10+A11+A12) * 100,
             C2_Perc = A10 / sum(A8+A10+A11+A12) * 100,
             C3_Perc = A11 / sum(A8+A10+A11+A12) * 100,
             C4_Perc = A12 / sum(A8+A10+A11+A12) * 100
             ) %>% 
      dplyr::rename('C1 Incidents' = A8_,
                    'C2 Incidents' = A10_,
                    'C3 Incidents' = A11_,
                    'C4 Incidents' = A12_,
                    'C1 Incidents %' = C1_Perc,
                    'C2 Incidents %' = C2_Perc,
                    'C3 Incidents %' = C3_Perc,
                    'C4 Incidents %' = C4_Perc) %>%
      arrange(Month) %>% 
      select(-c(6:9))#Drop the mysterious extra A8, A10, A11, A12 fields...
  }, striped = TRUE)
  
  # Incidents: HCP Level 1-4
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$incsHCPTbl <- renderTable({
    selData() %>% 
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
             'A74', 'A75', 'A76', 'A77') %>% 
      dplyr::group_by(Month) %>% 
      mutate(A74_ = comma(A74),
             A75_ = comma(A75),
             A76_ = comma(A76),
             A77_ = comma(A77),
             HCP1_Perc = A74 / sum(A74+A75+A76+A77) * 100,
             HCP2_Perc = A75 / sum(A74+A75+A76+A77) * 100,
             HCP3_Perc = A76 / sum(A74+A75+A76+A77) * 100,
             HCP4_Perc = A77 / sum(A74+A75+A76+A77) * 100) %>%
      
      dplyr::rename('HCP Level 1 Incidents' = A74_,
                    'HCP Level 2 Incidents' = A75_,
                    'HCP Level 3 Incidents' = A76_,
                    'HCP Level 4 Incidents' = A77_,
                    'HCP Level 1 Incidents %' = HCP1_Perc,
                    'HCP Level 2 Incidents %' = HCP2_Perc,
                    'HCP Level 3 Incidents %' = HCP3_Perc,
                    'HCP Level 4 Incidents %' = HCP4_Perc) %>% 
      arrange(Month) %>% 
      select(-c(6:9))
  }, striped = TRUE)

  # Incidents: IFT Level 1-4
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  output$incsLFTTbl <- renderTable({
    selData() %>%
      select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name',
             'A78', 'A79', 'A80', 'A81') %>%
      dplyr::group_by(Month) %>%
      mutate(
             A78_ = comma(A78),
             A79_ = comma(A79),
             A80_ = comma(A80),
             A81_ = comma(A81),
             LFT1_Perc = A78 / sum(A78+A79+A80+A81) * 100,
             LFT2_Perc = A79 / sum(A78+A79+A80+A81) * 100,
             LFT3_Perc = A80 / sum(A78+A79+A80+A81) * 100,
             LFT4_Perc = A81 / sum(A78+A79+A80+A81) * 100) %>%

      dplyr::rename('IFT Level 1 Incidents' = A78_,
                    'IFT Level 2 Incidents' = A79_,
                    'IFT Level 3 Incidents' = A80_,
                    'IFT Level 4 Incidents' = A81_,
                    'IFT Level 1 Incidents %' = LFT1_Perc,
                    'IFT Level 2 Incidents %' = LFT2_Perc,
                    'IFT Level 3 Incidents %' = LFT3_Perc,
                    'IFT Level 4 Incidents %' = LFT4_Perc) %>%
      arrange(Month) %>%
      select(-c(6:9))
  }, striped = TRUE)
})
# Fin.
