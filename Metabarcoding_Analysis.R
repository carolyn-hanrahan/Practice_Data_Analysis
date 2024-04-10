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


PA_data <- as.data.frame(clean_data)

for(i in 2:28){
  PA_data[,i] <- as.numeric(PA_data[,i])
} 

for(i in 2:28){
  for (j in 1:nrow(PA_data)){
    PA_data[j,i] <- ifelse(PA_data[j,i] >0, "1", "0")
  }
}


for(i in 2:28){
  PA_data[,i] <- as.numeric(PA_data[,i])
} 

PA_data$SpeciesRichness <- rowSums(PA_data[,2:28])


# List of how many samples come from each coyote individual 
CoyoteCount <- PA_data %>% 
  count(CoyoteID)



# Trying to determine which coyotes have samples from both 2023 and 2022: 
Test <- PA_data %>%
  filter(CoyoteID == "CCNS22_F5")

Test$PA <- rep(1, times=nrow(Test))

View(pivot_wider(data=Test, names_from = Year, values_from = PA))

CCNS22_F5



hmmm <- CoyoteCount[CoyoteCount$n > 1, ]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ :) 


# t-test to examine individual diet items between seasons. The below code examines the contribution of chicken to coyote diet 

group_1 <- clean_data[1:56,2] # all of the data from 2022 

group_2 <- clean_data[57:139,2] # all of the data from 2023 

t.test(group_1, group_2)


# t-test to examine the contribution of pilot whale to coyote diet by season 

group_1 <- clean_data[1:56,21] # all of the data from 2022 

group_2 <- clean_data[57:139,21] # all of the data from 2023 

t.test(group_1, group_2)


# t-test to examine the contribution of seal to coyote diet by season 

group_1 <- clean_test_data[1:56,25] # all of the data from 2022 

group_2 <- clean_test_data[57:139,25] # all of the data from 2023 

t.test(group_1, group_2)


# Notes from t-test analysis: None of the p-values appear to be less than 0.05. This indicates that there is no statistically significant difference by season for the food categories chosen... 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Examining Species Richness/Presence-Absence Data By Year: 

## Calculating Average Diversity by Year: 

average_diversity <- PA_data %>%
  group_by(Year) %>%
  summarise(avg_diversity = mean(SpeciesRichness))


## Plotting [good graph]: 

ggplot(average_diversity, aes(x = Year, y = avg_diversity, fill=Year)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Diversity (species richness) by Season", x = "Year", y = "Average Species Richness per Sample") +
  scale_fill_manual(values = c("skyblue", "lightgreen"))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Subset Overall FOO data and Visualize 

test_data_FOO <- data %>% select(c('Interpretation (Diet item)', 'FOO'))

test_data_FOO <- test_data_FOO[1:27,]


# Rename the first column
colnames(test_data_FOO)[1] <- "diet_item"


# Load necessary libraries
library(ggplot2)  # For data visualization

#Create a barplot 
ggplot(test_data_FOO, aes(x = diet_item, y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Prevalence of Diet Items", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ordered bar plot 

ggplot(test_data_FOO, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Prevalence of Diet Items", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Pie chart 
ggplot(test_data_FOO, aes(x = diet_item, y = FOO, fill = FOO) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Prevalence of Diet Items", fill = "Diet Item") +
  theme_minimal())


# Plotting FOO data by Year: 

Fall_data <- clean_data[1:56,]

Summer_data <- clean_data[57:139,]




# Ordered bar plot 

ggplot(Fall_data, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Prevalence of Diet Items", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ANOVA --> Analysis of Variance between Species Richness and Year 

results <- aov(SpeciesRichness ~ Year, data= PA_data)

summary(results)

Residuals = residuals(results)

# p-value = 0.00466, therefore a statistically significant result 

# Plotting the residuals from the ANOVA: 

ggplot(results, aes(x = Year, y = Residuals)) +
  geom_boxplot(fill = c("skyblue", "orange"), color = "black") +
  labs(title = "ANOVA Residuals", x = "Year", y = "Residuals")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

