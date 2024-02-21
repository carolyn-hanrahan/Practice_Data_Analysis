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
library(plotrix)



## Import Data and convert to data frame 
data <- Practice_data_wolves
print(data)

dataFrame <- data.frame(data)

view(dataFrame)

## Calculate ANOVA for harbor seal. How different are the read counts for harbor seal based on season? 

anova_result_2 <- aov(cbind(ReadCount_Fall, ReadCount_Summer1, ReadCount_Summer2) ~ 1, data = dataFrame)

summary(anova_result_2)

## So this is not specifically for harbor seal.... ^


print(dataFrame)

as.matrix(dataFrame)


## Bar plot showing read count for american beaver for each of the three seasons: 

barplot(as.matrix(dataFrame[1,2:4]))


## Bar plot showing read count for harbor seal for each of the three seasons:)  
y_axis <- c(0,50000)

barplot(as.matrix(dataFrame[16,2:4]),
        main = "Harbor Seal Read Count by Season",
        col = "darkblue",
        ylab = "DNA Read Count",
        xlab = "Season",
        ylim= y_axis)

## Pie chart showing read count for harbor seal for each of the three seasons: 

view(dataFrame_HS)

categories <- c("fall", "summer", "summer 2")

pie3D(as.matrix(dataFrame_HS), labels = categories, main = "Harbor Seal Read Count by Season")



## More ANOVA practice..... 
## Need to calculate means for each category before being able to do ANOVA*

dataFrame_HS <- dataFrame[16,2:4]
print(dataFrame_HS)


view(dataFrame)

anova_result3 <- aov(dataFrame_HS[,2:4])




