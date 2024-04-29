## Metabarcoding Statistical Analysis -- Coyote Sex
## April 25, 2024
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
sex_data <- read_excel("coyote_sex_data.xlsx")

sex_data <- sex_data %>% select(c('Interpretation (Diet item)', 'S23_0476':'S23_2874'))

sex_data <- sex_data[1:30,]


sex_data_longer <- sex_data %>%
  pivot_longer(cols = 'S23_0476':'S23_2874',
               names_to = "Sample",
               values_to = "No_Reads")



sex_data_wider <- sex_data_longer %>%
  pivot_wider(names_from = "Interpretation (Diet item)",
              values_from = "No_Reads")


# Split the list column into two separate columns
sex_data_wider <- unnest_wider(sex_data_wider, "NA", names_sep = "_")

# Rename the last two columns
sex_data_wider$Year <- sex_data_wider$NA_1

sex_data_wider$CoyoteID <- sex_data_wider$NA_2

sex_data_wider$Sex <- sex_data_wider$NA_3

# Remove two columns in the middle
sex_data_wider <- sex_data_wider[, -c(29:31)]

sex_data <- sex_data_wider

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# turn data into presence/absence data to calculate "species richness" 

sex_data <- as.data.frame(sex_data)


for(i in 2:28){
  sex_data[,i] <- as.numeric(sex_data[,i])
} 

for(i in 2:28){
  for (j in 1:nrow(sex_data)){
    sex_data[j,i] <- ifelse(sex_data[j,i] >0, "1", "0")
  }
}


for(i in 2:28){
  sex_data[,i] <- as.numeric(sex_data[,i])
} 

sex_data$SpeciesRichness <- rowSums(sex_data[,2:28])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subset male and female 


female <- sex_data %>%
  filter(Sex == "F")

male <- sex_data %>%
  filter(Sex == "M")



#__________________________________________________________________________________
# Calculate FOO for diet type for FEMALE

# female 
total_f <- colSums(female[,2:28])

divisor <- 75

total_FOO_female <- as.data.frame(total_f/divisor)

# Rename columns
colnames(total_FOO_female) <-  "FOO"

total_FOO_female$diet_item <- row.names(total_FOO_female)

# Ordered bar plot 

ggplot(total_FOO_female, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "magenta") +
  labs(title = "Prevalence of Diet Items (Female)", x = "Diet Item", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7))  # Set y-axis limits
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate FOO for diet type for MALE

# male 
total_m <- colSums(male[,2:28])

divisor <- 64

total_FOO_male <- as.data.frame(total_m/divisor)

# Rename columns
colnames(total_FOO_male) <-  "FOO"

total_FOO_male$diet_item <- row.names(total_FOO_male)

# Ordered bar plot 

ggplot(total_FOO_male, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Prevalence of Diet Items (Male)", x = "Diet Item", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7))  # Set y-axis limits
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Combining

# Create the double bar plot

all_data <- cbind(total_FOO_female, total_FOO_male)

all_data <- all_data[,1:3]

# Rename columns
colnames(all_data) <-  "FOO_female"

# Renaming the third column
names(all_data)[3] <- "FOO_male"

print(all_data)

names(all_data)[2] <- "diet_item"

# pivot 
all_long <- pivot_longer(all_data, cols = c("FOO_female", "FOO_male"), names_to = "Sex", values_to = "FOO")

# Create the bar plot
ggplot(all_long, aes(x = reorder(diet_item, -FOO), y = FOO, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "FOO of Diet Items: Female vs Male",
       x = "Diet Item",
       y = "FOO",
       fill = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c("red", "blue"))# Rotate x-axis labels vertically



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ statistics 

# ANOVA --> Analysis of Variance between Species Richness and Year 

results <- aov(SpeciesRichness ~ Sex, data= sex_data)

summary(results)

Residuals = residuals(results)

# p-value = 0.00466, therefore a statistically significant result 

# Plotting the residuals from the ANOVA: 

ggplot(results, aes(x = Sex, y = SpeciesRichness)) +
  geom_boxplot(fill = c("red", "blue"), color = "black") +
  labs(title = "Dietary Richness: Female vs Male", x = "Sex", y = "Species Richness")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``





