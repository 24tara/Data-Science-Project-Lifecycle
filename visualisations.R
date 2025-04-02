#load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(tidyverse)

#load excel file paths
young_children <- "/Users/elisaschezzini/Desktop/data science project lifecycle/coursework/Age 2011-2021.xlsx"
gdhi <- "/Users/elisaschezzini/Desktop/data science project lifecycle/coursework/GDHI and Population 1997-2022.xlsx"

#read specific sheets from the excel files
young_children_2021 <- read_excel(young_children, sheet="2021")
young_children_change <- read_excel(young_children, sheet="change 2011-2021")

gdhi_data <- read_excel(gdhi, sheet="GDHI")
population_data <- read_excel(gdhi, sheet="Population")

#convert all columns except region name to numeric in GDHI and Population dataset
gdhi_data <- gdhi_data %>%
  mutate(across(-`Region name`, as.numeric))

population_data <- population_data %>%
  mutate(across(-`Region name`, as.numeric))

#reshape GDHI data from wide to long format
gdhi_long <- gdhi_data %>%
  pivot_longer(
    cols = -`Region name`,  
    names_to = "Year",
    values_to = "GDHI"
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert Year column to numeric

#reshape Population data from wide to long format 
population_long <- population_data %>%
  pivot_longer(
    cols = -`Region name`,
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(Year))

#merge GDHI and Population datasets based on region name and year
merged_data <- left_join(gdhi_long, population_long, by = c("Region name", "Year"))

#display the first few rows of the merged dataset
head(merged_data)
#count the number of rows
nrow(merged_data)
#get summary statistics
summary(merged_data)

#identify and siaply duplicate entries for the same region name and year
merged_data %>%
  group_by(`Region name`, Year) %>%
  filter(n() > 1) %>%
  arrange(`Region name`, Year)

#remove duplicate rows while keeping all assoicated columns
merged_data <- merged_data %>%
  distinct(`Region name`, Year, .keep_all = TRUE)

#convert GDHI and Population to numeric and remove rows with missing values
merged_data <- merged_data %>%
  mutate(GDHI = as.numeric(GDHI),
         Population = as.numeric(Population)) %>%
  drop_na(GDHI, Population)

#fit a simple linear regression model: GDHI as a function of Population
model1 <- lm(GDHI ~ Population, data = merged_data)

#display the summary of the model
summary(model1)

#create a scatter plot of GDHI vs Population with a linear regression line
ggplot(merged_data, aes(x = Population, y = GDHI)) +
  geom_point(color = "blue", alpha = 0.5) + #scatter points 
  geom_smooth(method = "lm", color = "red", se = FALSE) + #linear regression line
  ggtitle("Regression: GDHI vs Population") + #title
  xlab("Population") + #x-axis
  ylab("GDHI") #y-axis

#apply a log transformation to both GDHI and Population to reduce skewness
merged_data <- merged_data %>%
  mutate(log_GDHI = log(GDHI),
         log_Population = log(Population))

#fit a log-log regression model
model2 <- lm(log_GDHI ~ log_Population, data = merged_data)

#display the summary of the model
summary(model2)

#violin plot - age distribution in 2021
ggplot(young_children_2021, aes(x = `local authority name`, y = `All usual residents`)) + 
  geom_violin(fill = "ivory", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Age Distribution by Local Authority")

#box plot - change in population 2011-2021
ggplot(young_children_change, aes(x = `local authority name`, y = `All usual residents`)) +
  geom_boxplot(fill = "royalblue", alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Population Change (2011-2021)")

#scatter plot - GDHI vs Population
ggplot(merged_data, aes(x = Population, y = GDHI)) +
  geom_point(color = "purple") +
  ggtitle("GDHI vs Population") +
  xlab("Population") +
  ylab("GDHI")

#two dimensional - KDE plot – GDHI vs Population
ggplot(merged_data, aes(x = Population, y = GDHI)) +
  geom_density_2d() +
  ggtitle("Two-Dimensional KDE Plot: Population vs GDHI") +
  xlab("Population") +
  ylab("GDHI")
