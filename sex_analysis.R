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
       fill = "Sex") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values = c("red", "blue"))# Rotate x-axis labels vertically



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ statistics 

# ANOVA --> Analysis of Variance between Species Richness and Sex 

results <- aov(SpeciesRichness ~ Sex, data= sex_data)

summary(results)

Residuals = residuals(results)

# p-value = 0.961, therefore not a statistically significant difference.  

# Plotting the residuals from the ANOVA: 

ggplot(results, aes(x = Sex, y = SpeciesRichness)) +
  geom_boxplot(fill = c("red", "blue"), color = "black") +
  labs(title = "Dietary Richness: Female vs Male", x = "Sex", y = "Species Richness")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
# Shannon's diversity 

library(vegan)


# Calculate Shannon's diversity index for all of the male samples: 
shannon_indices_male <- diversity(male[,2:28], index = "shannon")

# Print Shannon's diversity indices for each sample (male) 
print(shannon_indices_male)

# Mean shannon's diversity index for all the male samples 
overall_shannon_male <- mean(shannon_indices_male)

print(overall_shannon_male)

# value = 1.0366 indicating ______

# Calculate Shannon's diversity index for all of the 2023 samples: 
shannon_indices_female <- diversity(female[,2:28], index = "shannon")

# Print Shannon's diversity indices for each female sample  
print(shannon_indices_female)

# Mean shannon's diversity index for all the samples from 2023: 
overall_shannon_female <- mean(shannon_indices_female)

print(overall_shannon_female)
# value = 1.006621. There is reasonable/moderate variation/diversity on average. The diversity is higher than that of the 2022 data. 

# Test for statistical significance:

# T-test on the Shannon's diversity indices calculated in the above steps: 
t_test_result <- t.test(shannon_indices_male, shannon_indices_female)

# Print the results
print(t_test_result)

# p-value from this t-test is 0.7792, indicating NO statistically significant difference in diversity between male/female based on shannon's diversity  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ trying a new plot: 

# Plotting the groups
boxplot(shannon_indices_female, shannon_indices_male, names = c("Female", "Male"), 
        main = "Shannon's Diversity: Female vs. Male", ylab = "Shannon's Diversity", col = c("red", "blue"))

# Adding t-test results to the plot
text(x = 1.5, y = max(c(shannon_indices_female, shannon_indices_male)) + 10, 
     labels = sprintf("t = %.2f, p = %.3f", 
                      t_test_result$statistic, t_test_result$p.value),
     adj = 0.5)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ more advanced ggplot


# Jaccard index for sex 

# Load the vegan package
library(vegan)

# create matrices
mat_female <- female[,2:28]

mat_female <- as.matrix(mat_female)

mat_male <- male[,2:28]

mat_male <- as.matrix(mat_male)

# Calculate Jaccard dissimilarity index
jaccard_index <- vegdist(rbind(mat_female, mat_male), method = "jaccard")


# Check for missing or infinite values in the dissimilarity matrix
if (any(is.nan(jaccard_index)) || any(is.infinite(jaccard_index))) {
  # Handle missing or infinite values (e.g., remove them)
  jaccard_index <- jaccard_index[!is.nan(jaccard_index) & !is.infinite(jaccard_index)]
}

# Print the Jaccard dissimilarity index
print(jaccard_index)

# Compute the mean value of the dissimilarity matrix
mean_jaccard <- mean(jaccard_index)

print(mean_jaccard)

# Hmmmmmm 0.806 <- does this make sense?***

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~






