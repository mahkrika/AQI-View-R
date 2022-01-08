library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(gtools)
library(stringr)

inLocation <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/12/AmbSYS-to-Nov-2021.csv'
inData <- read.csv(inLocation, header = TRUE, stringsAsFactors = FALSE)

selData <- inData %>% 
  filter(Year == 2021 & Org.Code == 'RYA')

#Unlist: https://www.statology.org/r-list-object-cannot-be-coerced-to-type-double/
selData[,6:ncol(selData)] <- as.numeric(unlist(selData[,6:ncol(selData)]))

selDataL <- selData %>% 
  pivot_longer(cols = 6:ncol(selData), names_to = 'Field', values_to = 'Values')

# https://stackoverflow.com/questions/47223286/reordering-ggplot2-barplots-by-a-mixed-character-and-numerical-variable
# https://stackoverflow.com/questions/38931194/warning-when-defining-factor-duplicated-levels-in-factors-are-deprecated
selDataL$Field <- factor(selDataL$Field, levels = unique(mixedsort(as.character(selDataL$Field))))

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
    geom_point()
  p
}

# Number of incidents per category by month
rendLineGsub(selDataL, 'Month', 'Values', 'Field', c('A8', 'A10', 'A11', 'A12')) + 
  labs(title = 'Number of incidents per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Count of incidents')

# Mean response time per category by month
rendLineGsub(selDataL, 'Month', 'Values', 'Field', c('A25', 'A31', 'A34', 'A37')) +
  labs(title = 'Mean response time per category by month',
       subtitle = str_to_title(selDataL$Org.Name),
       x = 'Month',
       y = 'Mean response time (sec)')


