# Install required packages if they are not already installed
# install.packages(c("readxl", "dplyr", "ggplot2", "moments"))

# Load the necessary libraries:
# - 'readxl' is used for reading Excel files.
# - 'dplyr' is used for efficient data manipulation.
# - 'ggplot2' is used for data visualization.
# - 'moments' is used to compute statistical moments (e.g., skewness and kurtosis).
library(readxl)
library(dplyr)
library(ggplot2)
library(moments)

# 1. Read the Excel file containing the dataset.
# The dataset is assumed to be stored in "Data Cells and Areas.xlsx".
data <- read_excel("Data Cells and Areas.xlsx")
head(data)  # Display the first few rows to verify correct data import.

# 2. Adjust zero values in the "Leydig Cells/mm2" column.
# For any observation with a value of 0, add a small random number between 1e-10 and 1e-9.
# This step is performed solely to prevent computational errors when performing the Kolmogorov-Smirnov test,
# and it does not alter the outcomes of the pairwise comparisons.
idx_zeros <- which(data$`Leydig Cells/mm2` == 0)
data$`Leydig Cells/mm2`[idx_zeros] <- data$`Leydig Cells/mm2`[idx_zeros] + 
  runif(length(idx_zeros), min = 1e-10, max = 1e-9)

# 3. Reorder the 'Age' variable in ascending order.
# It is assumed that the 'Age' column contains values such as "4 months", "24 months", "48 months", etc.
# The numeric component is extracted and used to determine the order.
data$Age_num <- as.numeric(gsub("[^0-9]", "", data$Age))
data$Age <- factor(data$Age, levels = unique(data$Age[order(data$Age_num)]))

# Define a function to calculate the mode of a vector.
# This function returns the value that appears most frequently.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 4. Generate a boxplot for "Leydig Cells/mm2" by Age.
# Each box is filled with a manually specified color corresponding to the age group.
p_box <- ggplot(data, aes(x = Age, y = `Leydig Cells/mm2`, fill = Age)) +
  geom_boxplot() +
  scale_fill_manual(values = c("4 months"  = "#D81B60",
                               "24 months" = "#004D40",
                               "48 months" = "#1E88E5")) +
  labs(x = "Age", y = "Leydig Cells/mm2",
       title = "Boxplot of Leydig Cells/mm2 by Age (Ascending Order)") +
  theme_minimal()

print(p_box)

# 5. Compute a summary statistics table grouped by Age.
# The summary includes:
# - Mean
# - Median
# - Standard Deviation (SD)
# - Interquartile Range (IQR)
# - Skewness
# - Kurtosis
summary_table <- data %>%
  group_by(Age) %>%
  summarise(
    Mean = mean(`Leydig Cells/mm2`, na.rm = TRUE),
    Median = median(`Leydig Cells/mm2`, na.rm = TRUE),
    SD = sd(`Leydig Cells/mm2`, na.rm = TRUE),
    IQR = IQR(`Leydig Cells/mm2`, na.rm = TRUE),
    Skewness = skewness(`Leydig Cells/mm2`, na.rm = TRUE),
    Kurtosis = kurtosis(`Leydig Cells/mm2`, na.rm = TRUE)
  )

print(summary_table)

# 6. Perform pairwise statistical tests: Mann-Whitney U test and Kolmogorov-Smirnov test.
# First, generate all unique combinations of the Age groups.
ages <- unique(data$Age)
combinations <- combn(ages, 2, simplify = FALSE)

# Perform the Mann-Whitney U test for each pair of age groups.
mann_whitney_results <- lapply(combinations, function(pair) {
  group1 <- data$`Leydig Cells/mm2`[data$Age == pair[1]]
  group2 <- data$`Leydig Cells/mm2`[data$Age == pair[2]]
  
  test <- wilcox.test(group1, group2)
  data.frame(Age1 = pair[1], Age2 = pair[2], Mann_Whitney_p = test$p.value)
})
mann_whitney_results <- do.call(rbind, mann_whitney_results)
cat("Mann-Whitney U test results:\n")
print(mann_whitney_results)

# Perform the Kolmogorov-Smirnov test for each pair of age groups.
ks_results <- lapply(combinations, function(pair) {
  group1 <- data$`Leydig Cells/mm2`[data$Age == pair[1]]
  group2 <- data$`Leydig Cells/mm2`[data$Age == pair[2]]
  
  test <- ks.test(group1, group2)
  data.frame(Age1 = pair[1], Age2 = pair[2], KS_p = test$p.value)
})
ks_results <- do.call(rbind, ks_results)
cat("\nKolmogorov-Smirnov test results:\n")
print(ks_results)

# 7. Create an ECDF (Empirical Cumulative Distribution Function) plot for all data.
# Thin lines (size = 0.8) are used to enhance visual clarity.
p_ecdf <- ggplot(data, aes(x = `Leydig Cells/mm2`, color = Age)) +
  stat_ecdf(size = 0.8) +
  scale_color_manual(values = c("4 months"  = "#D81B60",
                                "24 months" = "#004D40",
                                "48 months" = "#1E88E5")) +
  labs(x = "Leydig Cells/mm2", y = "Cumulative Probability",
       title = "ECDF of Leydig Cells/mm2 by Age") +
  theme_minimal()

print(p_ecdf)

# 8. Create an ECDF plot with a zoom on the cumulative probability range from 0.30 to 0.70.
# This zoomed-in view, with thin lines (size = 0.8), allows for a detailed comparison of the distribution shapes.
p_ecdf_zoom <- ggplot(data, aes(x = `Leydig Cells/mm2`, color = Age)) +
  stat_ecdf(size = 0.8) +
  scale_color_manual(values = c("4 months"  = "#D81B60",
                                "24 months" = "#004D40",
                                "48 months" = "#1E88E5")) +
  labs(x = "Leydig Cells/mm2", y = "Cumulative Probability",
       title = "ECDF (Zoom: [0.30, 0.70])") +
  theme_minimal() +
  coord_cartesian(xlim = c(2.5, 3.5), ylim = c(0.45, 0.55))

print(p_ecdf_zoom)
