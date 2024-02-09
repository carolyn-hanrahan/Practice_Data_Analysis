### Vancouver Wolves Practice Analysis 
### Carolyn Hanrahan 
### February 8 2024 

## Load in packages 
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

read_xlsx("Wolves_Filtered_100.xlsx")

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

DataFrame$ReadCount2 <- runif(39, min=0, max=30000)

print(DataFrame)
view(DataFrame)

DataFrame$ReadCount3 <- runif(39, min=0, max=30000)

view(DataFrame)

#value_counts <- table(DataFrame$Season)
#barplot(value_counts)

#pie(value_counts) # this shows that there are basically the same number of samples per season 


## Calculating ANOVA! 

# note to self: okay I think I need to recreate this dataframe so that there is a column for each season rather than a "season" column...? 

anova_result <- aov(ReadCount ~ ReadCount2, data=DataFrame)

summary(anova_result) #A larger F-value suggests that the variance between groups is larger compared to the variance within groups, which could indicate a significant difference in means.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I will now attempt to create a MDS plot for similarity/dissimilarity 

mds_data <- DataFrame[, -1]

# dissimilarity matrix: 

dissim_matrix <- dist(mds_data)


# MDS
mds_result <- cmdscale(dissim_matrix)

#plot
plot(mds_result, type= "n", xlab= "Dimension 1", ylab= "Dimension 2")



