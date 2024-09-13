#############################################################################
#############################################################################
#############################################################################

# Authors: Omar Betancourt-León, Verónica Rodríguez-Mata, 
# Antonieta Martínez-Guerrero, and Armando Pérez-Torres

# code for the article: "Histological, Histochemical, and Morphometric 
# Characterization of Epidermal Leydig Cells and Histochemistry of 
# Epidermal Apical Cells from Juvenile and Adult Axolotls (Ambystoma 
# mexicanum)", 2024.

#############################################################################
#############################################################################
#############################################################################

# Install and load the necessary libraries
if (!require('readxl')) install.packages('readxl')  # For reading Excel files
if (!require('dplyr')) install.packages('dplyr')    # For data manipulation
if (!require('tidyr')) install.packages('tidyr')    # For reshaping data
if (!require('fmsb')) install.packages('fmsb')      # For radar chart plotting

# Load the required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(fmsb)
library(gtools)

# Read the Excel file containing the data
# file_path <- "/mnt/data/data scrip  INGLES.xlsx"
# data <- read_excel(file_path)
data <- data_scrip_INGLES  # Use the preloaded data instead of reading the file

# Replace age codes with full text values (e.g., "4m" becomes "4 months")
for (z in 1:nrow(data)) {
  ifelse(data$Age[z] == "4m", data$Age[z] <- "4 months", data$Age[z] <- data$Age[z])
}
for (z in 1:nrow(data)) {
  ifelse(data$Age[z] == "24m", data$Age[z] <- "24 months", data$Age[z] <- data$Age[z])
}
for (z in 1:nrow(data)) {
  ifelse(data$Age[z] == "48m", data$Age[z] <- "48 months", data$Age[z] <- data$Age[z])
}

# Function to check the normality of the data using the Shapiro-Wilk test
# Returns TRUE if the p-value is greater than 0.05 (i.e., the data is normally distributed)
check_normality <- function(x) {
  shapiro.test(x)$p.value > 0.05
}

# Check normality of cell count by age group and region
normality_results <- data %>%
  group_by(Age, Region) %>%
  summarise(normal = check_normality(No_celulas))

# Determine whether to use mean or median based on the normality test results
# If all groups are normally distributed, use the mean, otherwise use the median
use_mean <- all(normality_results$normal)

# Calculate the central tendency (mean or median) of cell counts for each age group and region
central_measures <- data %>%
  group_by(Age, Region) %>%
  summarise(No_celulas = if (use_mean) mean(No_celulas) else median(No_celulas))

# Transform data for radar chart plotting
data_radar <- central_measures %>%
  spread(key = Region, value = No_celulas) %>%
  ungroup()

# Sort data for radar chart by age
data_radar <- arrange(data_radar, mixedorder(Age))

# Add rows for the maximum and minimum limits of the radar chart
max_val <- max(data_radar[,-1], na.rm = TRUE)
min_val <- 0  # Minimum value for the radar chart

# Convert 'Age' column to character type to avoid issues during concatenation
data_radar$Age <- as.character(data_radar$Age)

# Create rows for the maximum and minimum values
max_row <- c("Max.", rep(max_val, ncol(data_radar) - 1))
min_row <- c("Min.", rep(min_val, ncol(data_radar) - 1))

# Add the max and min rows to the radar data
data_radar <- rbind(max_row, min_row, data_radar)

# Convert all columns except 'Age' to numeric
data_radar[,-1] <- apply(data_radar[,-1], 2, as.numeric)
data_radar <- data_radar[,-1]

# Print the final structure of the dataframe to verify the data
print(data_radar)

# Create the radar chart
jpeg("radar_celulas_sept.jpg", width = 2050, height = 1200, res = 300)
par(mar = c(1, 1, 1, 1))  # Adjust margins to fit the legend
radarchart(data_radar, axistype = 1, 
           pcol = c("#D81B60", "#004D40", "#1E88E5"),  # Line colors for each group
           plwd = 2,  # Line width
           plty = 1,  # Line type
           cglcol = "grey",  # Color of the grid lines
           cglty = 1,  # Type of grid lines
           axislabcol = "grey",  # Color of axis labels
           caxislabels = seq(min_val, max_val, length.out = 5),  # Axis label values
           cglwd = 0.8,  # Grid line width
           vlcex = 0.8)  # Label size

# Add a legend to the radar chart
legend(x = "topright", inset = c(0, 0), legend = unique(data$Age), 
       col = c("#D81B60", "#004D40", "#1E88E5"),  # Colors for the legend
       lty = 1, lwd = 4,  # Increase the line width in the legend
       cex = 1)  # Increase the size of the legend

# Close the JPEG device
dev.off()

###############################################################

# Create a radar chart in PNG format with different color settings
png("radar_celulas_color2.png", width = 1200, height = 700, res = 200)
par(mar = c(2, 2, 2, 2))  # Adjust margins to fit the legend

# areas <- c(rgb(1, 0, 0, 0.25), rgb(0, 1, 0, 0.25), rgb(0, 0, 1, 0.25))

radarchart(data_radar, axistype = 1, 
           pcol = c("red4", "chartreuse4", "black"),  # Line colors
           plwd = 2,  # Line width
           plty = 1,  # Line type
           cglcol = "grey",  # Grid color
           cglty = 1,  # Grid line type
           axislabcol = "grey",  # Axis label color
           caxislabels = seq(min_val, max_val, length.out = 5),  # Axis label values
           cglwd = 0.8,  # Grid line width
           vlcex = 0.8)  # Label size

# Add a title to the radar chart
title(main = "Central tendency values of LCs count across different regions and ages", cex.main = 0.8)

# Add a legend to the radar chart
legend(x = "topright", inset = c(0, 0), legend = unique(data$Age), 
       col = c("red4", "chartreuse4", "black"), lty = 1, lwd = 2, cex = 0.8)

# Close the PNG device
dev.off()
