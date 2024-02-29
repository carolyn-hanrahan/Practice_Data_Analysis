### Vancouver Wolves Practice Analysis 
### Carolyn Hanrahan 
### February 8 2024 

## this initial analysis looks at more of a beta diversity metric, examining total read counts per food group for the whole dataset.
## One thing to consider moving forward is to look at diversity within individual samples 
## Objectives: 
## - Describe dietary diversity 
## - Determine how significant shorebirds are to overall coyote diet 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~code written with Reed~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pivot wider (help file)
# names_from = Diet item 
# values_from = PA (something like this) 

# Load in packages 
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
view(data_longer)


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
  
     
view(data_wider)


## February 26th, 2024: 
## goals: ANOVA (diversity by season)
## create MDS plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ continuation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ANOVA 

results <- aov(Diversity ~ Season, data= data_wider)

summary(results)

#Interpreting the results: 
#     The Df column displays the degrees of freedom for the independent variable (the number of levels in the variable minus 1), and the degrees of freedom for the residuals (the total number of observations minus one and minus the number of levels in the independent variables).
#     The Sum Sq column displays the sum of squares (a.k.a. the total variation between the group means and the overall mean).
#     The Mean Sq column is the mean of the sum of squares, calculated by dividing the sum of squares by the degrees of freedom for each parameter.
#     The F value column is the test statistic from the F test. This is the mean square of each independent variable divided by the mean square of the residuals. The larger the F value, the more likely it is that the variation caused by the independent variable is real and not due to chance.
#     The Pr(>F) column is the p value of the F statistic. This shows how likely it is that the F value calculated from the test would have occurred if the null hypothesis of no difference among group means were true.

# In this case, the P-value is 0.078, which is greater than 0.05, meaning that there is no statistical significance between diversity and season in this case. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~continuation 2/29/2024~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Plot diversity by season*


## Calculating Average Diversity by Season: 

average_diversity <- data_wider %>%
  group_by(Season) %>%
  summarise(avg_diversity = mean(Diversity))


## Plotting [good graph]: 

ggplot(average_diversity, aes(x = Season, y = avg_diversity, fill=Season)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Diversity (species richness) by Season", x = "Season", y = "Average Species Richness per Sample") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightcoral"))




# Bar plot using ggplot2
# ggplot(data_wider, aes(x = Season, y = Diversity, fill = Season)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Diversity (species richness) by Season", x = "Season", y = "Diversity") +
#   theme_minimal() +
#   scale_fill_manual(values=c("skyblue", "magenta", "forestgreen"))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate frequency of occurrence by summing columns: 

# first alter dataframe to get rid of unnecessary things


FOO_df <- data_wider[, 2:40]
view(FOO_df)
  
new_row <- colSums(FOO_df)

FOO_df <- rbind(FOO_df, new_row)
view(FOO_df)


## this creates a new dataframe with the last row being FOO for each diet item. 

# Now I will subset it further to only have FOO and each diet item: 

FOO_df <- FOO_df[112,]

view(FOO_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ new diversity metric 
# Here I used the vegan package to calculate shannon's diversity index using the FOO dataframe. 

library(vegan)


diversity_result <- diversity(FOO_df, index="shannon")

## we need the season data, so here I incorporate that: 

diversity_result2 <- diversity(data_wider[,2:40, 42], index="shannon")


print(diversity_result)
print(diversity_result2)


# struggling here... 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ more  plotting

## Need richness and abundance for each species for shannon's diversity 

## plot results of ANOVA 

## PERMANOVA/ADNOIS/MDS

## Jaccard Index 

## visualizations for frequency of occurrence 



# Create a boxplot for ANOVA results
boxplot_data <- data.frame(
  Group = data_wider$Diversity,
  Residuals = residuals(anova_result))



# Boxplot using ggplot2
ggplot(boxplot_data, aes(x = Group, y = Residuals)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot for ANOVA Residuals", x = "Group", y = "Residuals")




 ################################## CREATE DATAFRAME WITH ABUNDANCE INFO: 

view(data_clean)


data_longer_2 <- data_clean %>%
  pivot_longer(cols = 'S22-4647':'S22-4641',
               names_to = "Sample",
               values_to = "ReadCount")



## this dataframe has abundance info! 

pivot_wider_2 <- data_longer_2 %>% 
  pivot_wider(names_from = "Diet item",
              values_from = "ReadCount")


season <- c('1','2','3')
sample(season, size = 1)

pivot_wider_2$Season <- sample(season, size = 1)


for( i in 1:nrow(pivot_wider_2)) {
  pivot_wider_2[i, ncol(pivot_wider_2)] <- sample(season, size = 1)
} 



##################################### PRACTICE NMDS PLOTS 

# Extract abundance data
abundance_data <- pivot_wider_2[, 2:40]

# Calculate dissimilarity matrix (Bray-Curtis is commonly used)
dissimilarity_matrix <- vegdist(abundance_data, method = "bray")

# Perform NMDS analysis
nmds_result <- metaMDS(dissimilarity_matrix)

# Extract NMDS coordinates
nmds_coordinates <- scores(nmds_result)

# Create a data frame with NMDS coordinates and Site information
nmds_data <- cbind(pivot_wider_2$Season, nmds_coordinates)
colnames(nmds_data) <- c("Season", "NMDS1", "NMDS2")





# Create NMDS plot using ggplot2
ggplot(nmds_data, aes(x = NMDS1, y = NMDS2, label = Season)) +
  geom_point() +
  geom_text_repel() +  # Add labels without overlap
  labs(title = "NMDS Plot", x = "NMDS1", y = "NMDS2")
























