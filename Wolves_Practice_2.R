## Another R script to continue practicing data analysis with a modified version of the Vancouver Wolves Data 
## Carolyn Hanrahan 
## February 20, 2024 

## Load libraries 
## Load in packages 
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)



## Import Data and convert to data frame 
data <- Practice_data_wolves
print(data)

dataFrame <- data.frame(data)

view(dataFrame)

## Calculate ANOVA for harbor seal. How different are the read counts for harbor seal based on season? 

anova_result_2 <- aov(cbind(ReadCount_Fall, ReadCount_Summer1, ReadCount_Summer2) ~ 1, data = dataFrame)

summary(anova_result_2)

## So this is not specifically for harbor seal.... ahhhhh.... 