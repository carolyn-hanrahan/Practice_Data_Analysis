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
#library(MASS)

# Install and load the readxl package
install.packages("readxl")
library(readxl)

# Read the Excel file/Import data into R: 
data <- read_excel("test.xlsx")



data_clean <- data %>% select(c('Interpretation (Diet item)', 'S23_0476':'S23_2874'))

data_clean <- data_clean[1:29,]


data_longer <- data_clean %>%
  pivot_longer(cols = 'S23_0476':'S23_2874',
               names_to = "Sample",
               values_to = "No_Reads")


# turn this data into presence/absence data
#PA_test_data <- mutate(test_data_longer, PA = ifelse(No_Reads > 0, "1", "0"))


#PA_test_data$PA <- as.numeric(PA_test_data$PA)
#PA_test_data <- PA_test_data[,-3]



data_wider <- data_longer %>%
  pivot_wider(names_from = "Interpretation (Diet item)",
              values_from = "No_Reads")


# Split the list column into two separate columns
data_wider <- unnest_wider(data_wider, "NA", names_sep = "_")

# Rename the last two columns
data_wider$Year <- data_wider$NA_1

data_wider$CoyoteID <- data_wider$NA_2

# Remove two columns in the middle
clean_data <- data_wider[, -c(29:30)]


# Now we have a clean_data file with all the necessary information! 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# turn data into presence/absence data to calculate "species richness" 

Presence_absence <- as.data.frame(lapply(clean_test_data, function(x) ifelse(x > 0, 1, 0)))


PA_test_data <- mutate(clean_test_data, PA = ifelse("No Reads" > 0, "1", "0"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ :) 


# t-test to examine individual diet items between seasons. The below code examines the contribution of chicken to coyote diet 

group_1 <- clean_test_data[1:56,2] # all of the data from 2022 

group_2 <- clean_test_data[57:139,2] # all of the data from 2023 

t.test(group_1, group_2)


# t-test to examine the contribution of pilot whale to coyote diet by season 

group_1 <- clean_test_data[1:56,21] # all of the data from 2022 

group_2 <- clean_test_data[57:139,21] # all of the data from 2023 

t.test(group_1, group_2)


# t-test to examine the contribution of seal to coyote diet by season 

group_1 <- clean_test_data[1:56,25] # all of the data from 2022 

group_2 <- clean_test_data[57:139,25] # all of the data from 2023 

t.test(group_1, group_2)


# Notes from t-test analysis: None of the p-values appear to be less than 0.05. This indicates that there is no statistically significant difference by season for the food categories chosen... 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examining Species Richness/Presence-Absence Data: 











#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



library(xlsx)

write.xlsx(clean_test_data, "clean_test_data")


#Subset FOO data 

test_data_FOO <- test_data %>% select(c('Interpretation (Diet item)', 'FOO'))

test_data_FOO <- test_data_FOO[1:27,]



plot("Interpretation (Diet item", 'FOO', data=test_data_FOO)

# Create a bar plot

ggplot(test_data_FOO, aes(x = 'Interpretation (Diet item)')) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Occurrence for Diet Items", x = "Diet Items", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Display the plot
print(plot)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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

# add a new column for diversity 

data_wider <- data_wider %>%
  mutate(Diversity = rowSums(x = data_wider[2:ncol(data_wider)]))

