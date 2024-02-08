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

barplot(as.numeric(DataFrame$ReadCount),
        names.arg = DataFrame$DietItem,
        col = "skyblue",
        main = "Read Count by Diet Type",
        xlab = "Diet type",
        ylab = "Read count")


## Let's make a different graph....hmmmmmmmmmmmm

ggplot(data=DataFrame) +
  aes(x=DietItem,y=ReadCount) + 
  geom_boxplot() + 
  theme(legend.position="none")
 

##ANOVA 

# in order to run an ANOVA, I need two ore more differing variables. In this case I will create a new column and add it to my dataframe. This column will be populated with random numbers between 1-3 and will indicate season 1, 2, or 3 in this hypothetical scenario. 
## add a column of random numbers between 1 and 3 to DF 

set.seed(123)

DataFrame$Season <- sample(1:3)

print(DataFrame)

#anova_result <- aov(DataFrame$DietItem ~ DataFrame$ReadCount, data=DataFrame


