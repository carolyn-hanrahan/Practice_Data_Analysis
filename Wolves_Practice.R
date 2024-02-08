### Vancouver Wolves Practice Analysis 
### Carolyn Hanrahan 
### February 8 2024 

## Load in packages 
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

read_csv("Wolves_Filtered_100.xlsx")

data <- Wolves_Filtered_100
view(data)



## Let's create a dataframe because that seems like a good idea. 
DietItem <- data$...1
ReadCount <- data$...2

DataFrame <- data.frame(DietItem, ReadCount)

view(DataFrame)

## Remove the first row of the dataframe because I think it may be causing problems 
DataFrame <- DataFrame[2:40, ]
view(DataFrame)

## Let's graph read counts
barplot(DataFrame$DietItem~DataFrame$ReadCount)

barplot(DataFrame$ReadCount,
        names.arg = unique(DataFrame$DietItem),
        col = "skyblue",
        main = "Read Count by Diet Type",
        xlab = "Diet type",
        ylab = "Read count")


##ANOVA 

anova_result <- aov(DataFrame$DietItem ~ DataFrame$ReadCount, data=DataFrame


