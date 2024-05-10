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

colnames(FOO_summer)[2] <- "FOO_summer"
colnames(FOO_fall)[2] <- "FOO_fall"

FOO_all <- merge(FOO_summer, FOO_fall, by.x = "Diet_Group", by.y = "Diet_Group")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
p1 <- ggplot(FOO_fall, aes(x = reorder(Diet_Group, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(x = "Diet Group", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.7)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14), axis.title = element_text(size=19), plot.title = element_text(size=19))

# Create barplot with data for the summer samples: 
p2 <- ggplot(FOO_summer, aes(x = reorder(Diet_Group, -FOO), y = FOO)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Diet Group", y = "FOO") +  # Add plot title
  coord_cartesian(ylim = c(0, 0.8)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14), axis.title = element_text(size=19), plot.title = element_text(size=19))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Using "patchwork" to combine plots 

library(patchwork)


patchwork <- p1 + p2

patchwork + plot_annotation(
  title = 'Frequency of Occurrence by Diet Group (fall and summer)',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = 'Created by: Carolyn Hanrahan 5/9/24'
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

u <- FOO_fall[,2]

FOO_summer$FOO_fall <- u

FOO_summer$summer <- FOO_summer$FOO

FOO_summer$fall <- FOO_summer$FOO_fall

FOO_summer <- FOO_summer[,-2]

FOO_summer <- FOO_summer[, -2]

FOO_summer$fall <- FOO_summer$fall

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FOO for ALL data: 
FOO_all <- FOO_summer

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FOO_longer <- pivot_longer(FOO_all, cols = c(2,3), names_to = "year", values_to ="value")


ggplot(FOO_longer, aes(x=Diet_Group, y= value, fill = year))

ggplot(FOO_longer, aes(x = reorder(Diet_Group, -value), y = value, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fall and Summer FOO by Diet Group",
       x = "Diet Group",
       y = "FOO",
       fill = "Season") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("FOO_fall" = "darkorange", "FOO_summer" = "skyblue"))








