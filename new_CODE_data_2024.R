rm(list = ls())

# Free unused R memory
gc()


# Setting working directory
getwd()  # returns current working directory
# SET THIS WORKING DIRECTORY TO YOUR WD PATH
setwd("D:/Tartu 24-25/MASTERS THESIS/dataverse_files.ParlGov")  

# load packages
install.packages("ggplot2")
install.packages("corrr")
install.packages("tidyverse")

library(ggplot2)
library(corrr)
library(tidyverse)
library(robotstxt)
library(rvest)
library(magrittr)
library(ggrepel)
library(dplyr)
library(readr)
library(stringr)

# Define the file path
file_path <- "D:/Tartu 24-25/MASTERS THESIS/dataverse_files.ParlGov/csv_DATA__vote_share_&_left_right.csv"

# Read the CSV file
data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)

# Display the first few rows
head(data)

# Check structure of data
str(data)


# Clean and convert numeric columns
data <- data %>%
  mutate(
    vote_share = str_replace_all(vote_share, ",", "."),  # Convert commas to dots
    left_right = str_replace_all(left_right, ",", "."),  # Convert commas to dots
    vote_share = as.numeric(vote_share),
    left_right = as.numeric(left_right)
  )

# Check for remaining NAs
sum(is.na(data$vote_share))  # Count NA values in vote_share
sum(is.na(data$left_right))  # Count NA values in left_right

# Remove rows where vote_share or left_right are NA
data_cleaned <- data %>%
  filter(!is.na(vote_share) & !is.na(left_right))

# Calculate Ideology Index (II) for each country
country_ideology_set <- data_cleaned %>%
  group_by(country_name) %>%
  summarise(country_ideology = sum(vote_share * left_right, na.rm = TRUE) / sum(vote_share, na.rm = TRUE))

# Merge country_ideology back into the dataset
data <- left_join(data, country_ideology_set, by = "country_name")

# Calculate Dalton Polarization Index (PI) for each country
dalton_polarization_set <- data_cleaned %>%
  left_join(country_ideology_set, by = "country_name") %>%  # Add Ideology Index (II)
  group_by(country_name) %>%
  summarise(dalton_polarization = sqrt(sum(vote_share * ((left_right - country_ideology) / 5)^2, na.rm = TRUE)))

# Merge PI back into the dataset
data <- left_join(data, dalton_polarization_set, by = "country_name")

# Calculate Ideology Index (II) for each country
dispersion_set <- data_cleaned %>%
  left_join(country_ideology_set, by = "country_name") %>%  # Add Ideology Index (II)
  left_join(dalton_polarization_set, by = "country_name") %>%  # Add Ideology Index (II)
  group_by(country_name) %>%
  summarise(dispersion = country_ideology * (10 - dalton_polarization))

# Merge DI back into the dataset
data <- left_join(data, dispersion_set, by = "country_name")

country_indexes <- data %>%
  select(country_name, country_ideology, dalton_polarization, dispersion) %>%
  distinct(country_name, .keep_all = TRUE)

country_indexes

######################################################################################


# Create a new column 'New_dem' with an initial value of 0 for all rows in "country_indexes"
country_indexes$new_dem <- 0
# Create a new column 'New_dem'with an initial value of 0 for all rows in "data"
data$new_dem <- 0

# List of New democracies (New democracies preceded by a left-wing authoritarian regime)
new_dem <- c(
  "Poland", "Estonia", "Latvia", "Lithuania", "Czech Republic", 
  "Slovakia", "Hungary", "Romania", "Bulgaria", "Slovenia", "Croatia"
)

# Adding a 'New_dem' column with a value of 1 for the specified countries and 0 for the rest
country_indexes$new_dem <- ifelse(
  country_indexes$country_name %in% new_dem, 
  1, 
  0
)

# Adding a 'New_dem' column with a value of 1 for the specified countries and 0 for the rest
data$new_dem <- ifelse(
  data$country_name %in% new_dem, 
  1, 
  0
)

################ CORRELATION ANALYSIS ##########################################

# Columns I want to analyze
selected_columns <- c("country_ideology", "dalton_polarization", "dispersion", "new_dem")

# Create an empty data frame to store results
correlation_matrix<- matrix(NA, nrow = length(selected_columns), ncol = length(selected_columns))
colnames(correlation_matrix) <- selected_columns
rownames(correlation_matrix) <- selected_columns

# Loop through the selected columns and calculate correlation and p-value for each pair
for (i in 1:length(selected_columns)) {
  for (j in 1:length(selected_columns)) {
    if (i != j) {
      test_result <- cor.test(country_indexes[[selected_columns[i]]], country_indexes[[selected_columns[j]]], use = "complete.obs")
      correlation_matrix[i, j] <- paste("r = ", round(test_result$estimate, 3), 
                                              "; p = ", format(round(test_result$p.value, 3), nsmall = 3))
    }
  }
}

correlation_matrix


#######################################################
data <- data %>%
  mutate(prev_regime_type = case_when(
    country_name %in% c("Latvia", "Lithuania", "Estonia") ~ "soviet republic",
    country_name %in% c("Czech Republic", "Bulgaria", "Poland", "Slovakia", "Hungary", "Romania", "Bulgaria") ~ "eastern bloc",
    country_name %in% c("Croatia", "Slovenia") ~ "yugoslavia",
    TRUE ~ "other"  # Assign "other" to all other countries
  ))


country_indexes <- country_indexes %>%
  mutate(prev_regime_type = case_when(
    country_name %in% c("Latvia", "Lithuania", "Estonia") ~ "soviet republic",
    country_name %in% c("Czech Republic", "Bulgaria", "Poland", "Slovakia", "Hungary", "Romania", "Bulgaria") ~ "eastern bloc",
    country_name %in% c("Croatia", "Slovenia") ~ "yugoslavia",
    TRUE ~ "other"  # Assign "other" to all other countries
  ))


# Perform ANOVA for each numeric variable
anova_ideology <- aov(country_ideology ~ prev_regime_type, data = country_indexes)
anova_polarization <- aov(dalton_polarization ~ prev_regime_type, data = country_indexes)
anova_dispersion <- aov(dispersion ~ prev_regime_type, data = country_indexes)

# Display ANOVA results
summary(anova_ideology)
summary(anova_polarization)
summary(anova_dispersion)


# Extracting ANOVA results
anova_results <- data.frame(
  Variable = c("Country Ideology", "Polarization", "Dispersion"),
  F_Statistic = c(summary(anova_ideology)[[1]]$`F value`[1],
                  summary(anova_polarization)[[1]]$`F value`[1],
                  summary(anova_dispersion)[[1]]$`F value`[1]),
  DF = c(paste(summary(anova_ideology)[[1]]$`Df`[1], "/", summary(anova_ideology)[[1]]$`Df`[2], sep = ""),
         paste(summary(anova_polarization)[[1]]$`Df`[1], "/", summary(anova_polarization)[[1]]$`Df`[2], sep = ""),
         paste(summary(anova_dispersion)[[1]]$`Df`[1], "/", summary(anova_dispersion)[[1]]$`Df`[2], sep = "")),
  P_Value = c(summary(anova_ideology)[[1]]$`Pr(>F)`[1],
              summary(anova_polarization)[[1]]$`Pr(>F)`[1],
              summary(anova_dispersion)[[1]]$`Pr(>F)`[1])
)

# Print the summary table
print(anova_results)




#####
# List of countries to leave
selected_countries <- c(
  "Poland", "Estonia", "Latvia", "Lithuania", "Czech Republic", 
  "Slovakia", "Hungary", "Romania", "Bulgaria", "Slovenia", "Croatia"
)

# Filtering the country_indexes table
data_new_dem <- country_indexes %>%
  filter(country_name %in% selected_countries)

# Checking the result
print(data_new_dem)

# Perform ANOVA for each numeric variable
anova_ideology_1 <- aov(country_ideology ~ prev_regime_type, data = data_new_dem)
anova_polarization_1 <- aov(dalton_polarization ~ prev_regime_type, data = data_new_dem)
anova_dispersion_1 <- aov(dispersion ~ prev_regime_type, data = data_new_dem)

# Display ANOVA results
summary(anova_ideology_1)
summary(anova_polarization_1)
summary(anova_dispersion_1)




#NOTE: I NEED TO DO THE SAME WITH VOTE_SHARE_UPD and with Vote_share of previous election to make this analysis more robust, for now I'm gathering this data. Based on my previous research (kinda similar) I used EU Political Barometer - and correlations were not stable in the sense of "statistically significant" or not and with strength of relation (R2) between all of them except "new democracies" and "dispersion" (their correlation was in all cases statistically significant and R2 was between 0.47 and 0.63)

################# GRAPHS
library(ggplot2)
library(ggrepel)

# Drawing Draft with trend line
ggplot_2024_draft <- ggplot(country_indexes, aes(x = country_ideology, y = dalton_polarization, color = as.factor(new_dem))) +
  geom_point(size = 3) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + 
  geom_text_repel(aes(label = country_name), size = 3) +  
  geom_vline(xintercept = 5, linetype = "dashed", color = "blue") +  
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +   
  labs(
    title = "Country Ideology & Dalton Polarization (parl.gov2024)",
    x = "Country Ideology",
    y = "Dalton Polarization",
    color = "New democracies preceded by a left-wing authoritarian regime"
  ) +
  theme_minimal()  
ggplot_2024_draft

#First graph: New democracy
ggplot_2024 <- ggplot(country_indexes, aes(x = country_ideology, y = dalton_polarization, color = as.factor(new_dem))) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +   # Scatter plot points with transparency for overlap handling
  geom_text_repel(aes(label = country_name), size = 3.5, max.overlaps = 15) +  # Add country labels without overlap
  geom_vline(xintercept = 5, linetype = "dashed", color = "blue", linewidth = 0.8) +  # Add country labels without overlap
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", linewidth = 0.8) +  
  scale_color_manual(values = c("0" = "darkorange", "1" = "royalblue"), name = "New Democracy") +   # Customize color scale for New Democracies
  labs(title = "Parties Dispersion on the Left-right Scale",  # Labels and title
       subtitle = "Ideological Position vs. Political Polarization",
       x = "Country Ideology (1-10 scale)",
       y = "Dalton’s Polarization (1-10 scale)",
       color = "New Democracy Status") +  
  theme_minimal(base_size = 14) + # Apply a minimal theme for better clarity
  theme(legend.position = "right", # Adjust legend positioning
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggplot_2024

#Second graph: New democracy + trend line
ggplot_2024_trend <- ggplot(country_indexes, aes(x = country_ideology, y = dalton_polarization, color = as.factor(new_dem))) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +   # Scatter plot points with transparency for overlap handling
  geom_text_repel(aes(label = country_name), size = 3.5, max.overlaps = 15) +  # Add country labels without overlap
  geom_vline(xintercept = 5, linetype = "dashed", color = "blue", linewidth = 0.8) +  # Add country labels without overlap
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", linewidth = 0.8) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # Лінії тренду для кожної групи
  scale_color_manual(values = c("0" = "darkorange", "1" = "royalblue"), name = "New Democracy") +   # Customize color scale for New Democracies
  labs(title = "Parties Dispersion on the Left-right Scale",  # Labels and title
       subtitle = "Ideological Position vs. Political Polarization",
       x = "Country Ideology (1-10 scale)",
       y = "Dalton’s Polarization (1-10 scale)",
       color = "New Democracy Status") +  
  theme_minimal(base_size = 14) + # Apply a minimal theme for better clarity
  theme(legend.position = "right", # Adjust legend positioning
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggplot_2024_trend


# Third graph: Previous regime type
ggplot_2024_regime <- ggplot(country_indexes, aes(x = country_ideology, y = dalton_polarization, color = as.factor(prev_regime_type))) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +   # Scatter plot points with transparency for overlap handling
  geom_text_repel(aes(label = country_name), size = 3.5, max.overlaps = 15) +   # Add country labels without overlap
  geom_vline(xintercept = 5, linetype = "dashed", color = "blue", linewidth = 0.8) +   # Vertical and horizontal reference lines
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", linewidth = 0.8) +  
  scale_color_manual(values = c("soviet republic" = "#27AE60", "eastern bloc" = "blue","yugoslavia" = "#FF5733", "other" = "orange", name = "Previous Regime Type")) + 
  labs(title = "Parties Dispersion on the Left-right Scale",  # Labels and title
       subtitle = "Ideological Position vs. Political Polarization",
       x = "Country Ideology (1-10 scale)",
       y = "Dalton’s Polarization (1-10 scale)",
       color = "Previous Regime Type") +  
  theme_minimal(base_size = 14) +  # Apply a minimal theme for better clarity
  theme(legend.position = "right",# Adjust legend positioning
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggplot_2024_regime

# Forth graph: Previous regime type + Trend line
ggplot_2024_regime_trend <- ggplot(country_indexes, aes(x = country_ideology, y = dalton_polarization, color = as.factor(prev_regime_type))) +
  geom_point(size = 3, alpha = 0.8, shape = 16) +   # Scatter plot points with transparency for overlap handling
  geom_text_repel(aes(label = country_name), size = 3.5, max.overlaps = 15) +   # Add country labels without overlap
  geom_vline(xintercept = 5, linetype = "dashed", color = "blue", linewidth = 0.8) +   # Vertical and horizontal reference lines
  geom_hline(yintercept = 4, linetype = "dashed", color = "red", linewidth = 0.8) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  # Лінії тренду для кожної групи
  scale_color_manual(values = c("soviet republic" = "#27AE60", "eastern bloc" = "blue","yugoslavia" = "#FF5733", "other" = "orange", name = "Previous Regime Type")) + 
  labs(title = "Parties Dispersion on the Left-right Scale",  # Labels and title
       subtitle = "Ideological Position vs. Political Polarization",
       x = "Country Ideology (1-10 scale)",
       y = "Dalton’s Polarization (1-10 scale)",
       color = "Previous Regime Type") +  
  theme_minimal(base_size = 14) +  # Apply a minimal theme for better clarity
  theme(legend.position = "right",# Adjust legend positioning
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggplot_2024_regime_trend



#####################################
#REGRESSION ANALYSIS


