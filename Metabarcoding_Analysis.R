## Metabarcoding Statistical Analysis
## March 12th, 2024
## Carolyn Hanrahan 

# Load in packages 
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggbiplot)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggforce)
library(vegan)
library(MASS)


# Import data 
data <- read_xlsx("No_Reads_Metabarcoding_Results.xlsx")

# clean data 
data_clean <- data %>% select(c('Interpretation (Diet item)','S23_0476':'S23_0636'))

data_longer <- data_clean %>%
  pivot_longer(cols = 'S23_0476':'S23_0636',
               names_to = "Sample",
               values_to = "ReadCount")

view(data_longer)

data_longer <- mutate(data_longer, PA = ifelse(ReadCount > 0, "1", "0"))
data_longer$PA <- as.numeric(data_longer$PA)

data_longer <- data_longer[,-3]

data_wider <- data_longer %>%
  pivot_wider(names_from = "Interpretation (Diet item)",
              values_from = "PA")
