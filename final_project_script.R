## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data from Excel
Raw_data <- read_excel("Raw_data.xlsx")




##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(Raw_data$funny)
mean(Raw_data$romance_tension)
mean(Raw_data$enjoyance)
sd(Raw_data$funny)
min(Raw_data$funny)
max(Raw_data$funny)
mean(Raw_data$romance_tension)
mean(Raw_data$enjoyance)
mean(Raw_data$awkwardness)
sd(Raw_data$romance_tension)
min(Raw_data$romance_tension)
table(Raw_data$enjoyance)
table(Raw_data$awkwardness)
mean(Raw_data$romance_tension)
mean(Raw_data$funny)
mean(Raw_data$enjoyance)
mean(Raw_data$awkwardness)
sd(Raw_data$romance_tension)
min(Raw_data$romance_tension)
#############################################################################################
#################### Figure 1: boxplot             ####################   
#################################################################################


# BOX PLOT

ggplot(Raw_data, aes(x = enjoyance, y = romance_tension)) +
  geom_boxplot() +
  labs(title = "Box Plot of romance_tension and enjoyance",
       x = "enjoyance",
       y = "romance_tension") +
  theme_minimal()


# Perform ANOVA
anova <- aov(romance_tension ~ enjoyance, data = Raw_data)
# Summarize ANOVA results
summary(anova)
# get R2
# between/total
# OR between/(between+within)
4.2/(4.2+20)





##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(Raw_data$funny,Raw_data$romance_tension)
print(linear_plot)

# add x line and y line for means
meany <- mean(Raw_data$romance_tension)
meanx <- mean(Raw_data$funny)

abline(h = meany, col = "black")
abline(v = meanx, col = "black")

linear_relationship <- lm(romance_tension ~funny, data = Raw_data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################

##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot


# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(Raw_data$funny, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
##################################################################################
####################  Table 2: contingcy table   ####################   
##################################################################################
table(Raw_data$enjoyance, Raw_data$awkwardness)
chisq.test(table(Raw_data$enjoyance, Raw_data$awkwardness))
