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
FOO_all_seasons <- read_excel("FOO_Groups.xlsx")
FOO_summer <- read_excel("FOO_Groups_Summer.xlsx")
FOO_fall <- read_excel("FOO_Groups_Fall.xlsx")

# Create the bar plot for FOO by group for all data: 
ggplot(FOO_all_seasons, aes(x = Diet_Group, y = FOO)) +
  geom_col(fill = "blue") +
  labs(title = "FOO by Diet Group",
       x = "Diet Group",
       y = "FOO")

ggplot(FOO_all_seasons, aes(x = reorder(Diet_Group, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Prevalence of Diet Items by Group", x = "Diet Group", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create barplot with data for the fall samples: 
ggplot(FOO_fall, aes(x = reorder(Diet_Group, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  labs(title = "Prevalence of Diet Items by Group (fall)", x = "Diet Group", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create barplot with data for the summer samples: 
ggplot(FOO_summer, aes(x = reorder(Diet_Group, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Prevalence of Diet Items by Group (summer)", x = "Diet Group", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.8))
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

