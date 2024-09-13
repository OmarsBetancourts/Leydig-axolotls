# Authors: Omar Betancourt-León, Verónica Rodríguez-Mata, Antonieta Martínez-Guerrero, and Armando Pérez-Torres

# Install and load the necessary libraries
if (!require('readxl')) install.packages('readxl')  # For reading Excel files
if (!require('dplyr')) install.packages('dplyr')    # For data manipulation
if (!require('tidyr')) install.packages('tidyr')    # For reshaping data
if (!require('fmsb')) install.packages('fmsb')      # For radar chart plotting
if (!require('rstatix')) install.packages('rstatix') # For statistical tests

# Load the required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(fmsb)
library(rstatix)
library(gtools)

# Set working directory (optional, adjust the path as needed)
setwd("path/to/file")

# Read the Excel file containing the data
# file_path <- "ruta/al/archivo/data_scrip_INGLES.xlsx"
# data <- read_excel(file_path)
data <- data_scrip_INGLES  # Use the preloaded data instead of reading the file

# Replace age codes with full text values (e.g., "4m" becomes "4 months")
data$Age <- recode(data$Age, "4m" = "4 months", "24m" = "24 months", "48m" = "48 months")

# Perform Kruskal-Wallis test for each region and store the results
kruskal_results <- data %>%
  group_by(Region) %>%
  kruskal_test(No_celulas ~ Age)

# Save Kruskal-Wallis test results to a CSV file
write.csv(kruskal_results, "kruskal_Results_noCel.csv", row.names = FALSE)

# Filter regions with significant results (p < 0.05)
kruskal_results2 <- kruskal_results %>%
  filter(p < 0.05) %>%
  pull(Region)

# Perform post-hoc Dunn test for significant regions, with Bonferroni correction
posthoc_results <- data %>%
  filter(Region %in% kruskal_results2) %>%
  group_by(Region) %>%
  dunn_test(No_celulas ~ Age, p.adjust.method = "bonferroni")

# Save post-hoc test results to a CSV file
write.csv(posthoc_results, "posthoc_cells.csv", row.names = FALSE)

# Display the post-hoc test results
posthoc_results
