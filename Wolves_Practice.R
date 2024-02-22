### Vancouver Wolves Practice Analysis 
### Carolyn Hanrahan 
### February 8 2024 

## this initial analysis looks at more of a beta diversity metric, examining total read counts per food group for the whole dataset.
## One thing to consider moving forward is to look at diversity within individual samples 
## Objectives: 
## - Describe dietary diversity 
## - Determine how significant shorebirds are to overall coyote diet 

## Load in packages 
library(vegan)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

Wolves_filtered_100 <- read_xlsx("Wolves_Filtered_100.xlsx")

data <- Wolves_Filtered_100
view(data)

data_clean <- data %>% select(c('Diet item','S22-4647':'S22-4641'))

data_longer <- data_clean %>%
  pivot_longer(cols = 'S22-4647':'S22-4641',
               names_to = "Sample",
               values_to = "ReadCount")


data_longer <- mutate(data_longer, PA = ifelse(ReadCount > 0, "1", "0"))

data_longer$PA <- as.numeric(data_longer$PA)
data_longer <- data_longer[,-3]


data_wider <- data_longer %>%
  pivot_wider(names_from = "Diet item",
              values_from = "PA")

## add a new column for diversity 

data_wider <- data_wider %>%
  mutate(Diversity = rowSums(x = data_wider[2:ncol(data_wider)]))

season <- c('1','2','3')
sample(season, size = 1)
data_wider$Season <- sample(season, size = 1)


for( i in 1:nrow(data_wider)) {
  data_wider[i, ncol(data_wider)] <- sample(season, size = 1)
}
  
     







 # pivot wider (help file)
# names_from = Diet item 
# values_from = PA (something like this) 


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


anova_result <- aov(ReadCount ~ ReadCount2 ~ ReadCount3, data=DataFrame)

summary(anova_result) #A larger F-value suggests that the variance between groups is larger compared to the variance within groups, which could indicate a significant difference in means.


## Attempting a t test... 

t.test(ReadCount, ReadCount2) ** ## error ocurring because "ReadCount2" is not an object...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# I will now attempt to create a MDS plot for similarity/dissimilarity 

mds_data <- DataFrame[, -1]

# dissimilarity matrix: 

dissim_matrix <- dist(mds_data)


# MDS
mds_result <- cmdscale(dissim_matrix)

#plot
plot(mds_result, type= "n", xlab= "Dimension 1", ylab= "Dimension 2")

## Summary: in this R script, I import the Vancouver wolves dataset and create a dataframe that includes read count and diet type.
## Note once again that we are looking at total read count per diet type across the whole study. 
## To better replicate my study, I add in two new columns: "ReadCount2" and "ReadCount3" --> these are meant to simulate two additional seasons so I can compare. 
## Following this, I created some basic charts/graphs to display read counts and did an ANOVA analysis between the three seasons. I also attempt a t test. 
## I begin to create a MDS plot but the data does not really lend itself to it... *should circle back* 

## Moving forward: 
  ## create a bar graph showing read counts per season with all three seasons represented in different colors 
  ## additional statistics 
  ## examining individual samples 






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Example data
categories <- c("Category A", "Category B", "Category C")
values1 <- DataFrame$ReadCount
values2 <- DataFrame$ReadCount2
values3 <- DataFrame$ReadCount3

# Combine the data into a matrix or data frame
data_matrix <- matrix(c(values1, values2, values3), ncol = 3, byrow = TRUE)
colnames(data_matrix) <- categories
rownames(data_matrix) <- c("Group 1", "Group 2", "Group 3")

# Create a bar graph
barplot(data_matrix, beside = TRUE, col = rainbow(3)) main = "Bar Graph with Three Categories", xlab = "Groups", ylab = "Values", legend.text = categories)







## seasons on x-axis, diversity measure on the y-axis
## Number of diet items per samples 
## season information contained in one column (season 1, 2, 3) 
## add a diversity row 
## wide to long format in tidyverse 
## if else


