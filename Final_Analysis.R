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
library(rJava)
library(xlsx)
library(openxlsx)
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

clean_data_2022 <- clean_data[1:56,]

clean_data_2023 <- clean_data[57:139,]

PA_2022 <- PA_data[1:56,]

PA_2023 <- PA_data[57:139,]

# Now we have a clean_data file with all the necessary information! 
#__________________________________________________________________________________
# Calculate FOO for diet type for each season*****************************

# 2022 data: 
total <- data.frame(colSums(PA_2022[,2:28]))

divisor <- 56

total_FOO_2022 <- total/divisor

# Rename columns
colnames(total_FOO_2022) <-  "Fall_FOO"

total_FOO_2022$diet_item <- row.names(total_FOO_2022)

# total_FOO_2022 now provides FOO values for 2022 data 

# Ordered bar plot 

ggplot(total_FOO_2022, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "Prevalence of Diet Items (Fall)", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#2023
total <- data.frame(colSums(PA_2023[,2:28]))

divisor <- 83

total_FOO_2023 <- total/divisor

# Rename columns
colnames(total_FOO_2023) <-  "Summer_FOO"

total_FOO_2023$diet_item <- row.names(total_FOO_2023)

ggplot(total_FOO_2023, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Prevalence of Diet Items (Summer)", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create the double bar plot

all_data <- cbind(total_FOO_2022, total_FOO_2023)

all_data <- all_data[,1:3]

# Load the ggplot2 package
library(ggplot2)

# Convert the dataframe to long format
library(tidyr)

all_long <- pivot_longer(all_data, cols = c("Fall_FOO", "Summer_FOO"), names_to = "Season", values_to = "FOO")

# Create the bar plot
ggplot(all_long, aes(x = diet_item, y = FOO, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fall and Summer FOO of Diet Items",
       x = "Diet Item",
       y = "FOO",
       fill = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels vertically

# Changing the x-axis to order diet items 
order <- c('white-tailed deer', 'chicken','eastern cottontail', 'New England cottontail', 'turkey')


ggplot(all_long, aes(x = factor(diet_item, level=order), y = FOO, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fall and Summer FOO of Diet Items",
       x = "Diet Item",
       y = "FOO",
       fill = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels vertically


# EXAMPLE CODE: create bar plot with specific axis order
ggplot(df, aes(x=factor(team, level=c('Mavs', 'Heat', 'Nets', 'Lakers')), y=points)) +
  geom_col()

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


# Trying to see what the most common diet items were for this individual: 

diet_item_freq <- colSums(Test[, 2:28])

# Create a bar plot
barplot(diet_item_freq, 
        main = "Frequency of Diet Items for CCNS22_F5",
        xlab = " ",
        ylab = "Frequency",
        col = "darkgreen",
        names.arg = names(diet_item_freq),
        las = 2)



# Define colors based on the year
year_colors <- c("2022" = "skyblue", "2023" = "lightpink")  # Add more colors for additional years

# Create a bar plot with colors based on the year
barplot(diet_item_freq, 
        main = "Frequency of Diet Items by Year",
        xlab = "Diet Items",
        ylab = "Frequency",
        col = year_colors,  # Use year_colors for colors
        names.arg = names(diet_item_freq),
        las = 2,
        legend.text = TRUE)


# Calculate the frequency of each diet item for each year

Year_2022 <- colSums(Test[Test$Year == 2022, 2:28])
Year_2023 <- colSums(Test[Test$Year == 2023, 2:28])

# Define colors for each year
year_colors <- c("2022" = "skyblue", "2023" = "lightpink")  # Add more colors for additional years

# Combine the frequencies into a matrix
height_matrix <- rbind(Year_2022, Year_2023)

# Create the bar plot
barplot(height_matrix,
        beside = TRUE,
        main = "Frequency of Diet Items by Year for CCNS22_F5",
        xlab = "Diet Items",
        ylab = "Frequency",
        col = c(year_colors["2022"], year_colors["2023"]),  # Use colors for each year
        legend.text = TRUE,
        args.legend = list(x = "topright"))  # Position the legend





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

average_diversity_plot <- ggplot(average_diversity, aes(x = Year, y = avg_diversity, fill=Year)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Diversity (species richness) by Season", x = "Year", y = "Average Species Richness per Sample") +
  scale_fill_manual(values = c("skyblue", "lightgreen"))

# Add error bars
average_diversity_plot <- average_diversity_plot + geom_errorbar(aes(ymin = values - errors, ymax = values + errors), 
                                                                 width = 0.4,                   # Width of the error bars
                                                                 color = "red",                # Color of the error bars
                                                                 position = position_dodge(0.9))  # Dodge the error bars to align with the bars


#Fall data
Fall_data <- PA_data[1:56,]

mean_sr_fall <- mean(Fall_data$SpeciesRichness)
#2.679

#Spring data 
Summer_data <- PA_data[57:139,]
mean_sr_summer <- mean(Summer_data$SpeciesRichness)
#3.651

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
  labs(title = "Prevalence of Diet Items Across all Seasons", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Ordered bar plot 

ggplot(test_data_FOO, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "turquoise") +
  labs(title = "Prevalence of Diet Items Across all Seasons", x = "Diet Item", y = "FOO") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Pie chart 
ggplot(test_data_FOO, aes(x = diet_item, y = FOO, fill = FOO) +
         geom_bar(stat = "identity", width = 1) +
         coord_polar("y", start = 0) +
         labs(title = "Prevalence of Diet Items", fill = "Diet Item") +
         theme_minimal())


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ plotting diversity by season

#Fall data
Fall_data <- PA_data[1:56,]


mean_sr_fall <- mean(Fall_data$SpeciesRichness)

#2.679

#Spring data 
Summer_data <- PA_data[57:139,]

mean_sr_summer <- mean(Summer_data$SpeciesRichness)

#3.65

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ANOVA --> Analysis of Variance between Species Richness and Year 

results <- aov(SpeciesRichness ~ Year, data= PA_data)

summary(results)

Residuals = residuals(results)

# p-value = 0.00466, therefore a statistically significant result 

# Plotting the residuals from the ANOVA: 

ggplot(results, aes(x = Year, y = SpeciesRichness)) +
  geom_boxplot(fill = c("skyblue", "orange"), color = "black") +
  labs(title = "Dietary Richness by Year", x = "Year", y = "Species Richness")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Jaccard Dissimilarity Index 

library(vegan)

# Calculate Jaccard dissimilarity index
jaccard_index <- vegdist(rbind(PA_2022[,2], PA_2023[,2]), method = "jaccard")

# Print the Jaccard dissimilarity index
print(jaccard_index)

# The above index calculates how similar chicken is in coyote diet for 2022 and 2023 

#Grey or harbor seal: 
jaccard_index <- vegdist(rbind(PA_2022[,25], PA_2023[,25]), method = "jaccard")

print(jaccard_index)

# The above index examines grey/harbor seal and calculates a jaccard index of 0.93, indicating 
# high overlap between seasons 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comparing a sample from an individual coyote from fall and summer. Coyote "CCNS22_F5"

jaccard_index <- vegdist(rbind(PA_2022[7,2:28], PA_2023[68,2:28]), method = "jaccard")

print(jaccard_index)

u <- PA_2022[7,2:28]
z <- PA_2023[68,2:28]

# Transpose the dataframe
transposed_u <- t(u)
transposed_z <- t(z)

jaccard_index <- vegdist(rbind(transposed_u[,1], transposed_z[,1]), method = "jaccard")

print(jaccard_index)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

# Petraitis' W': 
# Petraitis' W' is a measure used in ecology to quantify the variability or heterogeneity in the distribution 
# of a particular trait within a community or population.
# Note that the below proportions are based on proportion of samples, not proportion of indivudals

# Petraitis' W for 2022: 

prop_2022 <- total_FOO_2022[,1]

# Calculate Petraitis' W'
petraitis_W <- 1 - sum(prop_2022^2)

# Print the result
print(petraitis_W)

# Petraitis' W' for the 2022 dataset: 0.301, indicating moderate variability. 

# Petraitis' W for 2023: 

prop_2023 <- total_FOO_2023[,1]

# Calculate Petraitis' W'
petraitis_W <- 1 - sum(prop_2023^2)

print(petraitis_W)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Levin's Measure of Niche Breadth.......
# Levin's niche breadth ranges from 1 to n, where 1 indicates that a species utilizes only one resource/environmental condition 

# Calculate Levin's index 2022
levins_index <- 1 / sum(prop_2022^2)

# Print the result
print(levins_index)
# Result: 1.43 indicates relatively broad resource usage 

# Calculate Levin's index 2023
levins_index <- 1 / sum(prop_2023^2)

# Print the result
print(levins_index)