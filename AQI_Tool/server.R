#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(gtools)
library(stringr)
library(scales)
library(hms)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  field_key <- data.frame(
    Field = c('A1', 'A2', 'A3', 'A4', 'A114', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 
              'A12', 'A57', 'A112', 'A74', 'A75', 'A76', 'A77', 'A78', 'A79', 'A80', 'A81', 
              'A115', 'A119', 'A13', 'A14', 'A15', 'A16', 'A111', 'A17', 'A18', 'A19', 'A20', 
              'A21', 'A22', 'A23', 'A113', 'A24', 'A25', 'A26', 'A27', 'A28', 'A29', 'A30', 
              'A31', 'A32', 'A33', 'A34', 'A35', 'A36', 'A37', 'A38', 'A82', 'A83', 'A84', 
              'A85', 'A86', 'A87', 'A88', 'A89', 'A90', 'A91', 'A92', 'A93', 'A94', 'A95', 
              'A96', 'A97', 'A98', 'A99', 'A100', 'A101', 'A102', 'A103', 'A104', 'A105', 
              'A116', 'A117', 'A118', 'A120', 'A121', 'A122', 'A39', 'A40', 'A41', 'A42', 
              'A43', 'A44', 'A45', 'A46', 'A47', 'A48', 'A49', 'A50', 'A51', 'A52', 'A106', 
              'A107', 'A108', 'A109', 'A110', 'A53', 'A54', 'A55', 'A56', 'A58', 'A59', 'A60', 
              'A61', 'A62', 'A63', 'A64', 'A65', 'A66', 'A67', 'A68', 'A69', 'A70', 'A71', 'A72', 
              'A73'),
    
    Field_Detail = c('Calls answered', 'Total call answer time Total call answer time', 
                     'Mean call answer time', 'Median call answer time', '90th centile call answer time', 
                     '95th centile call answer time', '99th centile call answer time', 'All incidents', 
                     'C1 incidents', 'C1T incidents', 'C2 incidents', 'C3 incidents', 'C4 incidents', 
                     'HCP incidents with non-emergency conveyance', 
                     'Incidents with non-emergency conveyance', 'HCP Level 1 incidents', 
                     'HCP Level 2 incidents', 'HCP Level 3 incidents', 'HCP Level 4 incidents', 
                     'IFT Level 1 incidents', 'IFT Level 2 incidents', 'IFT Level 3 incidents', 
                     'IFT Level 4 incidents', 'C1 incidents excluding HCP and IFT', 'C2 incidents excluding HCP and IFT', 
                     'C1 NoC / PTQ / keywords incidents', 'Total time to NoC / PTQ / keywords C1', 
                     'Mean time to NoC / PTQ / keywords C1', '90th centile time to NoC / PTQ / keywords C1', 
                     'C1 incidents from NHS 111', 'Incidents with no face-to-face response', 'Incidents closed with advice: Non-C5', 
                     'Incidents referred to other service: Non-C5', 'Incidents with call back before response on scene: Non-C5', 
                     'Incidents closed with advice: C5', 'Incidents referred to other service: C5', 
                     'Incidents with call back before response on scene: C5', 'C5 incidents with response on scene', 
                     'Total response time: C1', 'Mean response time: C1', '90th centile response time: C1', 
                     'Total response time: C1T', 'Mean response time: C1T', '90th centile response time: C1T', 
                     'Total response time: C2', 'Mean response time: C2', '90th centile response time: C2', 
                     'Total response time: C3', 'Mean response time: C3', '90th centile response time: C3', 
                     'Total response time: C4', 'Mean response time: C4', '90th centile response time: C4', 
                     'Total response time: HCP Level 1', 'Mean response time: HCP Level 1', 
                     '90th centile response time: HCP Level 1', 'Total response time: HCP Level 2', 
                     'Mean response time: HCP Level 2', '90th centile response time: HCP Level 2', 
                     'Total response time: HCP Level 3', 'Mean response time: HCP Level 3', 
                     '90th centile response time: HCP Level 3', 'Total response time: HCP Level 4', 
                     'Mean response time: HCP Level 4', '90th centile response time: HCP Level 4', 
                     'Total response time: IFT Level 1', 'Mean response time: IFT Level 1', 
                     '90th centile response time: IFT Level 1', 'Total response time: IFT Level 2', 
                     'Mean response time: IFT Level 2', '90th centile response time: IFT Level 2', 
                     'Total response time: IFT Level 3', 'Mean response time: IFT Level 3', 
                     '90th centile response time: IFT Level 3', 'Total response time: IFT Level 4', 
                     'Mean response time: IFT Level 4', '90th centile response time: IFT Level 4', 
                     'Total response time: C1 excluding HCP and IFT', 'Mean response time: C1 excluding HCP and IFT', 
                     '90th centile response time: C1 excluding HCP and IFT', 'Total response time: C2 excluding HCP and IF', 
                     'Mean response time: C2 excluding HCP and IFT', '90th centile response time: C2 excluding HCP and IFT', 
                     'Resources allocated to C1', 'Resources arriving to C1', 'Resources allocated to C1T', 
                     'Resources arriving to C1T', 'Resources allocated to C2', 'Resources arriving to C2', 
                     'Resources allocated to C3', 'Resources arriving to C3', 'Resources allocated to C4', 
                     'Resources arriving to C4', 'Bystander CPR count', 'Total time to bystander CPR', 'Mean time to bystander CPR', 
                     '90th centile time to bystander CPR', 'Section 136 count', 'Total response time: Section 136', 
                     'Mean response time: Section 136', '90th centile response time: Section 136', 'Section 136 transport', 
                     'Incidents with transport to ED', 'Incidents with transport not to ED', 'Incidents with no transport', 
                     'Incidents with face-to-face response', 'HCP 1-hour response', 'HCP 2-hour response', 'HCP 3-hour response', 
                     'HCP 4-hour response', 'Total response time: HCP 1-hour response', 'Mean response time: HCP 1-hour response', 
                     '90th centile response time: HCP 1-hour response', 'Total response time: HCP 2-hour response', 
                     'Mean response time: HCP 2-hour response', '90th centile response time: HCP 2-hour response', 
                     'Total response time: HCP 3-hour response', 'Mean response time: HCP 3-hour response', 
                     '90th centile response time: HCP 3-hour response', 'Total response time: HCP 4-hour response', 
                     'Mean response time: HCP 4-hour response', '90th centile response time: HCP 4-hour response')
  )
  
  
  
  ################################################################################
  # Reset parameters with Reset button # 
  observeEvent(input$reset, {
    updateSelectInput(session = getDefaultReactiveDomain(), 'menuOrg', selected = 'England');
    updateSelectInput(session = getDefaultReactiveDomain(), 'menuYear', selected = '2021');
  })
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Functions
  
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
                  aes_string(x = xn, 
                             y = yn, 
                             group = useFactor, 
                             colour = useFactor)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = comma) +
        scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
        labs(title = useTitle,
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
  
  rendAreaIdentitysub <- function(xn, yn, useFactor, useFields, useTitle, useX, useY, selFields, useFactorNames){
   renderPlot({
     dfa <- selDataL() %>% 
       dplyr::filter(
         Field %in% useFields
       )
     dfa$Field2 <- factor(dfa$Field, levels = rev(useFields))
     p <- ggplot(data = dfa, aes_string(x = xn,
                                 y = yn,
                                 group = selFields)) +
       geom_area(aes(fill = Field2), position = 'identity') +
       geom_line(aes(group = Field2)) +
       scale_fill_viridis_d(option = 'C',
                            labels = useFactorNames) +
       scale_y_continuous(labels = comma) +
       scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
       labs(title = useTitle,
            x = useX,
            y = useY
       ) + 
       theme(legend.position="bottom") + 
       guides(col = guide_legend(title = "",
                                 nrow=3, 
                                 byrow = TRUE
       ))
     
     p
   }) 
  }
  
  
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
                aes_string(x = xn, 
                           y = yn, 
                           group = useFactor, 
                           colour = useFactor)) +
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
  
  ## Graph: Column
  rendColGsub <- function(xn, yn, useFactor, useFields, useTitle, useX, useY, useFactorNames, useFill){
    renderPlot({
    dfa <- selDataL() %>% 
      dplyr::filter(
        Field %in% useFields
      )
    dfa$Field2 <- factor(dfa$Field, levels = useFields)
    p <- ggplot(data = dfa,
                aes_string(x = xn, 
                           y = yn
                           #fill = useFactor
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
                           labels = useFactorNames) +
      #scale_fill_hue(direction = -1) +
      
      scale_y_continuous(labels = comma) +
      scale_x_continuous(labels = c(1:12), breaks = c(1:12)) +
      labs(title = useTitle,
           x = useX,
           y = useY
      ) + 
      theme(legend.position="bottom") + 
      guides(col = guide_legend(title = " ",
                                nrow=3, 
                                byrow = TRUE
      ))
    p
    },
    width = "auto",
    height = "auto"
    )
  }

  inData <- eventReactive(input$submit, {
    inLoc <- input$urlIn
    inFile <- read.csv(inLoc, header = TRUE, stringsAsFactors = FALSE, na.strings = c('.', '-'))#%>%
  })
  
  selData <- eventReactive(input$submit, {
    df <- inData() %>% 
      filter(Year == input$menuYear & Org.Name == input$menuOrg)
  })
  
  selDataL <- eventReactive(input$submit, {
    df <- selData() %>% 
      pivot_longer(cols = 6:ncol(selData()), names_to = 'Field', values_to = 'Values')
    df <- merge(df, field_key, by = 'Field')
  })
   
   output$callsAns <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A1'), 
                                   'Number of calls answered', 'Month', 'Count of calls answered')
   
   output$callsAnsTime <- rendAreaIdentitysub('Month', 'Values', 'Field_Detail', c('A4', 'A3', 'A114', 'A5', 'A6'),
                                              'Call answer times', 'Month', 'Seconds', 'Field2',
                                              c('99th centile call answer time',
                                                '95th centile call answer time',
                                                '90th centile call answer time',
                                                'Mean call answer time',
                                                'Median call answer time'))
   
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
   
   
   output$C1RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A25'), 
                                   'Mean Response Time - C1', 'Month', 'Seconds')
   
   output$C1RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A26'), 
                                   '90th Centile Response Time - C1', 'Month', 'Seconds') 
   
   output$C1RTTbl <- renderTable({
     selData() %>% 
       select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
              'A25', 'A26') %>% 
       mutate(A25 = comma(A25),
              A26 = comma(A26)) %>% 
       dplyr::rename('Mean response time: C1' = A25,
                     '90th centile response time: C1' = A26)
   }, striped = TRUE) 
   
   output$C2RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A31'), 
                                   'Mean Response Time - C2', 'Month', 'Seconds')
   
   output$C2RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A32'), 
                                   '90th Centile Response Time - C2', 'Month', 'Seconds') 
   
   output$C2RTTbl <- renderTable({
     selData() %>% 
       select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
              'A31', 'A32') %>% 
       mutate(A31 = comma(A31),
              A32 = comma(A32)) %>% 
       dplyr::rename('Mean response time: C2' = A31,
                     '90th centile response time: C2' = A32)
   }, striped = TRUE) 
   
   output$C3RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A34'), 
                                   'Mean Response Time - C3', 'Month', 'Seconds')
   
   output$C3RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A35'), 
                                   '90th Centile Response Time - C3', 'Month', 'Seconds') 
   
   output$C3RTTbl <- renderTable({
     selData() %>% 
       select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
              'A34', 'A35') %>% 
       mutate(A34 = comma(A34),
              A35 = comma(A35)) %>% 
       dplyr::rename('Mean response time: C3' = A34,
                     '90th centile response time: C3' = A35)
   }, striped = TRUE) 
   
   output$C4RTMean <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A37'), 
                                   'Mean Response Time - C4', 'Month', 'Seconds')
   
   output$C4RT90th <- rendLineGsub('Month', 'Values', 'Field_Detail', c('A38'), 
                                   '90th Centile Response Time - C4', 'Month', 'Seconds') 
   
   output$C4RTTbl <- renderTable({
     selData() %>% 
       select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
              'A37', 'A38') %>% 
       mutate(A37 = comma(A37),
              A38 = comma(A38)) %>% 
       dplyr::rename('Mean response time: C4' = A37,
                     '90th centile response time: C4' = A38)
   }, striped = TRUE) 
   
   output$incsCatTbl <- renderTable({
     selData() %>% 
       select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
              'A8', 'A10', 'A11', 'A12') %>% 
       mutate(A8 = comma(A8),
              A10 = comma(A10),
              A11 = comma(A11),
              A12 = comma(A12)) %>% 
       dplyr::rename('C1 Incidents' = A8,
                     'C2 Incidents' = A10,
                     'C3 Incidents' = A11,
                     'C4 Incidents' = A12)
   }, striped = TRUE)
   
 
   output$callAnsTbl <- renderTable({
     selData() %>% 
       select('Year', 'Month', 'Region', 'Org.Code', 'Org.Name', 
              'A1', 'A3', 'A4', 'A114', 'A5', 'A6') %>% 
       mutate(A1 = comma(A1),
              A3 = comma(A3),
              A4 = comma(A4),
              A114 = comma(A114),
              A5 = comma(A5),
              A6 = comma(A6)) %>% 
       dplyr::rename('Number of calls answered' = A1,
                     'Mean call answer time' = A3,
                     'Median call answer time' = A4,
                     '90th centile call answer time' = A114,
                     '95th centile call answer time' = A5,
                     '99th centile call answer time' = A6)
   }, striped = TRUE)  
   
   
})
