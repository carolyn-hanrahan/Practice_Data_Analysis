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

divisor <- 57

total_FOO_2022 <- total/divisor

# Rename columns
colnames(total_FOO_2022) <-  "FOO"

total_FOO_2022$diet_item <- row.names(total_FOO_2022)

# total_FOO_2022 now provides FOO values for 2022 data 

# Ordered bar plot 

ggplot(total_FOO_2022, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "Prevalence of Diet Items (Fall)", x = "Diet Item", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7))  # Set y-axis limits
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#2023
total <- data.frame(colSums(PA_2023[,2:28]))

divisor <- 83

total_FOO_2023 <- total/divisor

# Rename columns
colnames(total_FOO_2023) <-  "FOO"

total_FOO_2023$diet_item <- row.names(total_FOO_2023)

ggplot(total_FOO_2023, aes(x = reorder(diet_item, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Prevalence of Diet Items (Summer)", x = "Diet Item", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7))
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
ggplot(all_long, aes(x = reorder(diet_item, -FOO), y = FOO, fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fall and Summer FOO of Diet Items",
       x = "Diet Item",
       y = "FOO",
       fill = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_manual(values = c("Fall_FOO" = "darkorange", "Summer_FOO" = "skyblue"))# Rotate x-axis labels vertically

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
  geom_boxplot(fill = c("darkorange", "skyblue"), color = "black") +
  labs(title = "Dietary Richness by Season: Fall 2022 and Summer 2023", x = "Season", y = "Species Richness")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# T-test assuming poisson distribution - 5/2/24 - model 
library(lmerTest)


install.packages("lme4", type = "source")

m <- glmer(SpeciesRichness ~ Year + (1|CoyoteID), data=PA_data, family=poisson)

summary(m) #Better AIC (better fit model)

m1 <- glmer.nb(SpeciesRichness ~ Year + (1|CoyoteID), data=PA_data)

summary(m1)


# model incorporating coyote sex 

m2 <- glmer(SpeciesRichness ~ Sex + (1|CoyoteID), data = sex_data, family = poisson)

summary(m2)


# model incorporating sex and season: 

m3 <- lmer(SpeciesRichness ~ Year + Sex + (1|CoyoteID), data=sex_data)
summary(m3)

em3 <- emmeans(m3, ~Year:Sex)

em3_result <- data.frame(em3)

ggplot(data = em3_result, mapping = aes(x=Year, y = emmean, fill = Sex))+
  geom_bar(stat = "identity", position = position_dodge())+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.3, position = position_dodge(0.9))+
  theme_classic()+
  labs(title = "Species Richness by Season (2022 vs 2023) and Sex", x = "Year", y = "Species Richness")+
  scale_fill_manual(values = c('thistle','lightslateblue'))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(emmeans)
library(ggplot2)

em <- emmeans(m, pairwise ~ Year, type="response")

em_results <- data.frame(em$emmeans)

ggplot(em_results, aes(x = Year, y = rate)) +
  geom_bar(stat= "identity", fill = c("darkorange", "skyblue"), color = "black") +
  geom_errorbar(aes(ymin= asymp.LCL, ymax=asymp.UCL), width=.2) +
  labs(title = "Dietary Richness by Season: Fall 2022 and Summer 2023", x = "Season", y = "Species Richness")



em2 <- emmeans(m2, pairwise ~ Sex, type="response")

em_results2 <- data.frame(em2$emmeans)

ggplot(em_results2, aes(x = Sex, y = rate)) +
  geom_bar(stat= "identity", fill = c("thistle3", "lightslateblue"), color = "black") +
  geom_errorbar(aes(ymin= asymp.LCL, ymax=asymp.UCL), width=.2) +
  labs(title = "Dietary Richness by Sex: Female vs. Male", x = "Season", y = "Species Richness")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ other approach to Jaccard index: 


# Convert data frames to matrices
mat_PA_2022 <- as.matrix(PA_2022[,2:28])
mat_PA_2023 <- as.matrix(PA_2023[,2:28])



# 27 total diet items. 3 unique diet items in the fall and 5 unique diet items in the summer. 19 diet items overlap for both seasons. 


# Load the vegan package
library(vegan)

jaccard_data <- PA_data[,2:28]

jd <- jaccard_data[rowSums(jaccard_data)>0, ]

# Calculate Jaccard dissimilarity index
jaccard_index <- vegdist(jd, method = "jaccard", binary = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NMDS~~~~~~~~~~~~~~~~~~~~~~~~~~~ May 8th, 2024 
library(vegan)

MDS_object <- metaMDS(jaccard_index, k=2)

plot(MDS_object)

# extract NMDS scores to plot by groups/colors: 

data.scores <- as.data.frame(scores(MDS_object))

# add columns to dataframe with year 
PA_data1 <- sex_data[rowSums(jaccard_data)>0, ]

data.scores$Year <- PA_data1$Year
data.scores$Sex <- PA_data1$Sex

# plot 
library(ggplot2)

ggplot(data.scores, aes(x=NMDS1, y=NMDS2, colour=Year)) + 
  geom_point(size=4, aes(shape=Sex)) +
  scale_color_manual(values = c("orange", "skyblue")) +
  scale_shape_manual(values = c(1,16)) +
  stat_ellipse()
  
# not very different, lots of overlap 
# 2023 = more concentrated, within 2023 there is less dissimilarity. If there is higher dietary diversity in the summer, there is more likelihood for overlap. Therefore more similarity exists in summer than in the fall. 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ adding diet items to NMDS plot~~~~~~~~~ May 10th, 2024 
library(vegan)

# fit <- envfit(MDS_object, jaccard_index, permutations = 999)
# 
# head(fit)
# 
# ordiplot(MDS_object, type="n", main="title")
# orditorp(MDS_object, display = "sites", labels = F)
# plot(fit, p.max= 0.001, col="black", cex= 0.7)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Plotting "correlation"/phi coefficients 
library(reshape2)
phi_co <- cor(jaccard_data)

# melt to reformat
melted_phi_co <- melt(phi_co)

#plot 
library(ggplot2)
ggplot(data=melted_phi_co, aes(x=Var1, y=Var2,
                               fill = value)) +
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust =1))

# reorder corr matrix
# using corr coefficient as distance metric
dist <- as.dist((1-phi_co)/2)

# hierarchical clustering the dist matrix
hc <- hclust(dist)
phi_co <-phi_co[hc$order, hc$order]

# reduce the size of correlation matrix
melted_corr_mat <- melt(phi_co)
#head(melted_corr_mat)

#plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust =1))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Print the Jaccard dissimilarity index
print(jaccard_index)

# Compute the mean value of the dissimilarity matrix
mean_jaccard <- mean(jaccard_index)

print(mean_jaccard)

# Mean jaccard dissimilarity index is 0.8, meaning that approximately 80% of the datasets are the same. This is a relativelty high level of similarity. 
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# The below code is adapted from the methods from: "https://www.researchgate.net/publication/363745467_DNA_metabarcoding_reveals_that_coyotes_in_New_York_City_consume_wide_variety_of_native_prey_species_and_human_food 

library(vegan)

# Calculate Shannon's diversity index for all of the 2022 samples: 
shannon_indices_2022 <- diversity(PA_2022[,2:28], index = "shannon")

# Print Shannon's diversity indices for each sample from 2022 dataset: 
print(shannon_indices_2022)

# Mean shannon's diversity index for all the samples from 2022: 
overall_shannon_2022 <- mean(shannon_indices_2022)

print(overall_shannon_2022)
# value = 0.848, indicating moderate diversity. "there is a reasonable variation
# in the occurrence of different diet items across the samples, but it's not extremely high"

# Calculate Shannon's diversity index for all of the 2023 samples: 
shannon_indices_2023 <- diversity(PA_2023[,2:28], index = "shannon")

# Print Shannon's diversity indices for each sample from 2023 dataset: 
print(shannon_indices_2023)

# Mean shannon's diversity index for all the samples from 2023: 
overall_shannon_2023 <- mean(shannon_indices_2023)

print(overall_shannon_2023)
# value = 1.14. There is reasonable/moderate variation/diversity on average. The diversity is higher than that of the 2022 data. 

# Test for statistical significance:

# T-test on the Shannon's diversity indices calculated in the above steps: 
t_test_result <- t.test(shannon_indices_2022, shannon_indices_2023)

# Print the results
print(t_test_result)

# p-value from this t-test is 0.0068, indicating a statistically significant difference in diversity between seasons. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ plotting shannon's diversity 
# Plotting the groups
boxplot(shannon_indices_2022, shannon_indices_2023, names = c("Fall", "Summer"), 
        main = "Shannon's Diversity: Fall 2022 vs. Summer 2023", ylab = "Shannon's Diversity", col = c("darkorange", "skyblue"))

# Adding t-test results to the plot
text(x = 1.5, y = max(c(shannon_indices_female, shannon_indices_male)) + 10, 
     labels = sprintf("t = %.2f, p = %.3f", 
                      t_test_result$statistic, t_test_result$p.value),
     adj = 0.5)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PCA/NMDS Plots

# library(multcompView)
# library(flashClust)
# library(FactoMineR)
# library(factoextra)
# 
# # Perform PCA 2022
# pca_result <- PCA(mat_PA_2022, graph = FALSE)
# 
# # Plot results
# plot.PCA(pca_result)
# 
# 
# # Perform PCA 2023 
# pca_result <- PCA(mat_PA_2023, graph = FALSE)
# 
# # Plot results
# plot.PCA(pca_result)
# 
# 
# # Combined NMDS/PCA plot for both seasons 
# PCA_data <- PA_data[,2:29]
# 
# # Ensure that 'Year' column is a factor
# PCA_data$Year <- factor(PCA_data$Year)
# 
# # Compute Bray-Curtis dissimilarity matrix
# dist_matrix <- vegdist(PCA_data[, -which(names(PCA_data) == "Year")], method = "bray")
# 
# dist_matrix <- na.omit(dist_matrix)
# 
# # Perform NMDS analysis
# nmds_result <- metaMDS(dist_matrix)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~NMDS~~~~~~~~~~~~~~~~~~~~~~~~~~~ May 8th, 2024 
library(vegan)

MDS_object <- metaMDS(jaccard_index, k=2)








