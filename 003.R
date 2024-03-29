library(dplyr)
library(ggplot2)
library(tidyr)
#library(plotly)
library(gtools)
library(stringr)
library(scales)
library(hms)
#library(viridis)

inLocation <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/AmbSYS-to-Nov-2021.csv'
inData <- read.csv("AmbSYS-to-Nov-2021.csv", header = TRUE, stringsAsFactors = FALSE, na.strings = c('.', '-'))
fullOrgs <- unique(inData$Org.Code)
fullOrgNames <- unique(inData$Org.Name)
fullYears <- unique(inData$Year)
fullRegions <- unique(inData$Region)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions

## Graph: Line
rendLineGsub <- function(df, xn, yn, useFactor, useFields){
  dfa <- df %>% 
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
    scale_y_continuous(labels = comma)
  p
}

## Graph: Point
rendPointGsub <- function(df, xn, yn, useFactor, useFields){
  dfa <- df %>% 
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
    scale_y_continuous(labels = comma)
  p
}

## Graph: Column
rendColGsub <- function(df, xn, yn, useFactor, useFields){
  dfa <- df %>% 
    dplyr::filter(
      Field %in% useFields
    )
  dfa$Field <- factor(dfa$Field, levels = rev(useFields))
  p <- ggplot(data = dfa,
              aes_string(x = xn, 
                         y = yn, 
                         fill = useFactor)) +
    geom_col(colour = "black", 
             alpha = 0.5 
             #position = "dodge" # To put side-by-side
    ) +
    scale_fill_hue(direction = -1) +
    scale_y_continuous(labels = comma)
  p
}

## Graph: Proportion Full Column
rendPropColGsub <- function(df, xn, yn, useFactor, useFields){
  dfa <- df %>% 
    dplyr::filter(
      Field %in% useFields
    )
  dfa$Field <- factor(dfa$Field, levels = rev(useFields))
  
  # Had to explicitly name fields as couldn't get variables to feed through :-(
  dfb <- dfa %>% 
    group_by(Month, Field) %>% 
    summarise(n = sum(Values)) %>% 
    mutate(percentage = n / sum(n))
  
  p <- ggplot(data = dfb,
              aes(x = Month,
                  y = percentage,
                  group = Field,
                  fill = Field)) +
    geom_col(colour = "black", alpha = 0.5) +
    scale_fill_hue(direction = -1) +
    scale_y_continuous(labels = percent)
  p
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data preparation

## Filter dataset
selData <- inData %>% 
  filter(Year == 2021 & Org.Code == 'RX6')

## Convert the numerical columns to numerical data from strings
# Unlist: https://www.statology.org/r-list-object-cannot-be-coerced-to-type-double/
selData[,6:ncol(selData)] <- as.numeric(unlist(selData[,6:ncol(selData)]))

## Convert the filtered dataset into long format
selDataL <- selData %>% 
  pivot_longer(cols = 6:ncol(selData), names_to = 'Field', values_to = 'Values')

## Sort the filtered long dataset on the Field name as character so that goes A8, A9, A10
##    instead of A10, A11... A8, A9.
# https://stackoverflow.com/questions/47223286/reordering-ggplot2-barplots-by-a-mixed-character-and-numerical-variable
# https://stackoverflow.com/questions/38931194/warning-when-defining-factor-duplicated-levels-in-factors-are-deprecated
selDataL$Field <- factor(selDataL$Field, levels = unique(mixedsort(as.character(selDataL$Field))))


selDataL <- merge(selDataL, field_key, by = 'Field')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Graph outputs using functions

# Point - Call answer times by month
rendPointGsub(selDataL, 'Month', 'Values', 'detail_field', c('A3', 'A4', 'A114', 'A5', 'A6')) + 
  labs(title = 'Call answer times',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Seconds') +
  #scale_x_continuous(labels = 'Month', breaks = 'Month') +
  theme(legend.position="bottom") + 
  guides(col = guide_legend(title = "",
                            nrow=2, 
                            byrow = TRUE
                            ))




# LINE - Number of calls answered by month
rendLineGsub(selDataL, 'Month', 'Values', 'Field', c('A1')) + 
  labs(title = 'Number of calls answered',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Count of calls answered')

# LINE - Number of incidents per category by month
rendLineGsub(selDataL, 'Month', 'Values', 'Field', c('A8', 'A10', 'A11', 'A12')) + 
  labs(title = 'Number of incidents per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Count of incidents')

# LINE - Mean response time per category by month
rendLineGsub(selDataL, 'Month', 'Values', 'Field', c('A25', 'A31', 'A34', 'A37')) +
  labs(title = 'Mean response time per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Mean response time (sec)')

# LINE - 90th centile response time per category by month
rendLineGsub(selDataL, 'Month', 'Values', 'Field', c('A26', 'A32', 'A35', 'A38')) +
  labs(title = '90th centile response time per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = '90th centile response time (sec)')

# COLUMN - Number of incidents per category by month
rendColGsub(selDataL, 'Month', 'Values', 'Field', c('A8', 'A10', 'A11', 'A12')) + 
  labs(title = 'Number of incidents per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Count of incidents')

# COLUMN PROPORTION - Proportion of incidents per category by month
rendPropColGsub(selDataL, 'Month', 'Values', 'Field', c('A8', 'A10', 'A11', 'A12')) + 
  labs(title = 'Number of incidents per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Percentage of incidents (%)')

# COLUMN PROPORTION - Proportion of Face-to-Face / No Face-to-Face responses
rendPropColGsub(selDataL, 'Month', 'Values', 'Field', c('A56', 'A17')) + 
  labs(title = 'Proportion of Face-to-Face / No Face-to-Face responses',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Percentage (%)')










selDataL2 <- selDataL %>% 
  filter(Field %in% c('A3', 'A4', 'A114', 'A5', 'A6'))
  #filter(Field %in% c('A6'))

selDataL2$detail_field2 <- factor(selDataL2$detail_field, levels = c('99th centile call answer time',
                                                       '95th centile call answer time',
                                                       '90th centile call answer time',
                                                       'Mean call answer time',
                                                       'Median call answer time'))

ggplot(selDataL2, aes(Month, Values, group = detail_field2)) +
  geom_area(aes(fill = detail_field2), position = 'identity') +
  geom_line(aes(group = detail_field2)) +
  scale_fill_viridis_d(option = 'C')


selDataL2$Field2 <- factor(selDataL2$Field, levels = rev(c('A4', 'A3', 'A114', 'A5', 'A6')))
ggplot(selDataL2, aes(Month, Values, group = Field2)) +
  geom_area(aes(fill = Field2), position = 'identity') +
  geom_line(aes(group = Field2)) +
  scale_fill_viridis_d(option = 'C',
                       labels = c('99th centile call answer time',
                                  '95th centile call answer time',
                                  '90th centile call answer time',
                                  'Mean call answer time',
                                  'Median call answer time'))

