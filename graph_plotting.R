
# Load necessary libraries
library(raster)
library(RColorBrewer)
library(gridExtra)
library(ggplot2)
library(grid)
library(dplyr)


# Paths to the folders (replace with your actual paths)
folder_year1 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2016"
folder_year2 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2018"
folder_year3 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2020"
folder_year4 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2022"
folder_year5 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2024"

# Custom plot titles for cities
plot_titles <- c("Chattogram", "Nagpur", "Pune", "Surat")

# Function to create an ECDF plot for a single city across three years
create_ecdf_plot <- function(city_index) {
  # Read the corresponding raster files for this city from each folder
  raster_year1 <- raster(list.files(folder_year1, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year2 <- raster(list.files(folder_year2, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year3 <- raster(list.files(folder_year3, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year4 <- raster(list.files(folder_year4, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year5 <- raster(list.files(folder_year5, pattern = "\\.tif$", full.names = TRUE)[city_index])
  
  # Extract non-NA values from each raster
  values_year1 <- getValues(raster_year1)[!is.na(getValues(raster_year1))]
  values_year2 <- getValues(raster_year2)[!is.na(getValues(raster_year2))]
  values_year3 <- getValues(raster_year3)[!is.na(getValues(raster_year3))]
  values_year4 <- getValues(raster_year4)[!is.na(getValues(raster_year4))]
  values_year5 <- getValues(raster_year5)[!is.na(getValues(raster_year5))]
  
  # Create a data frame for ggplot
  df <- data.frame(
    Value = c(values_year1, values_year2, values_year3, values_year4, values_year5),
    Year = factor(rep(c("2016", "2018", "2020", "2022", "2024"), 
                      times = c(length(values_year1), length(values_year2), length(values_year3),
                                length(values_year4), length(values_year5))))
  )
  
  # Generate ECDF plot comparing three years
  p <- ggplot(df, aes(x = Value, color = Year)) +
    stat_ecdf(geom = "step") +
    labs(title = plot_titles[city_index], x = "Values", y = "ECDF") +
    theme_minimal() +
    scale_color_manual(values = c("2016" = "purple", "2018" = "blue", "2020" = "green", "2022"="red", "2024" = "black"))
  
  return(p)
}

# Generate ECDF plots for all 4 cities
plots <- lapply(1:4, create_ecdf_plot)

# Arrange all plots in a 4x3 grid
grid_plot <- marrangeGrob(plots, nrow = 2, ncol = 2)

# Save the arranged plots as a PNG file
png("ecdf_comparison_five_years.png", width = 1600, height = 1200, res = 150)
grid.draw(grid_plot)
dev.off()

# Custom plot titles for cities
plot_titles <- c("Chattogram", "Nagpur", "Pune", "Surat")

# Function to calculate ECDF statistics for a single city across three years
calculate_ecdf_stats <- function(city_index) {
  # Read the corresponding raster files for this city from each folder
  raster_year1 <- raster(list.files(folder_year1, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year2 <- raster(list.files(folder_year2, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year3 <- raster(list.files(folder_year3, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year4 <- raster(list.files(folder_year4, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year5 <- raster(list.files(folder_year5, pattern = "\\.tif$", full.names = TRUE)[city_index])
  
  # Extract non-NA values from each raster
  values_year1 <- getValues(raster_year1)[!is.na(getValues(raster_year1))]
  values_year2 <- getValues(raster_year2)[!is.na(getValues(raster_year2))]
  values_year3 <- getValues(raster_year3)[!is.na(getValues(raster_year3))]
  values_year4 <- getValues(raster_year4)[!is.na(getValues(raster_year4))]
  values_year5 <- getValues(raster_year5)[!is.na(getValues(raster_year5))]
  
  # Create ECDF functions for each year
  ecdf_year1 <- ecdf(values_year1)
  ecdf_year2 <- ecdf(values_year2)
  ecdf_year3 <- ecdf(values_year3)
  ecdf_year4 <- ecdf(values_year4)
  ecdf_year5 <- ecdf(values_year5)
  
  # Define key quantiles to extract (25%, 50% (median), 75%)
  quantiles <- c(0.25, 0.5, 0.75)
  
  # Compute quantiles for each year
  quantiles_year1 <- quantile(values_year1, probs = quantiles)
  quantiles_year2 <- quantile(values_year2, probs = quantiles)
  quantiles_year3 <- quantile(values_year3, probs = quantiles)
  quantiles_year4 <- quantile(values_year4, probs = quantiles)
  quantiles_year5 <- quantile(values_year5, probs = quantiles)
  
  # Compute cumulative probabilities at specific values (e.g., mean of all values)
  specific_value <- mean(c(values_year1, values_year2, values_year3, values_year4, values_year5))
  cum_prob_year1 <- ecdf_year1(specific_value)
  cum_prob_year2 <- ecdf_year2(specific_value)
  cum_prob_year3 <- ecdf_year3(specific_value)
  cum_prob_year4 <- ecdf_year4(specific_value)
  cum_prob_year5 <- ecdf_year5(specific_value)
  
  # Return a data frame with the results for this city
  return(data.frame(
    City = plot_titles[city_index],
    Year = c("2016", "2018", "2020", "2022", "2024"),
    `Q1 (25%)` = c(quantiles_year1[1], quantiles_year2[1], quantiles_year3[1], quantiles_year4[1], quantiles_year5[1]),
    `Median (50%)` = c(quantiles_year1[2], quantiles_year2[2], quantiles_year3[2], quantiles_year4[2], quantiles_year5[2]),
    `Q3 (75%)` = c(quantiles_year1[3], quantiles_year2[3], quantiles_year3[3], quantiles_year4[3], quantiles_year5[3]),
    `Cumulative Probability (Mean)` = c(cum_prob_year1, cum_prob_year2, cum_prob_year3, cum_prob_year4, cum_prob_year5)
  ))
}

# Apply the function to all cities and combine the results
ecdf_stats <- do.call(rbind, lapply(1:4, calculate_ecdf_stats))

# View the results
print(ecdf_stats)

# Save the results to a CSV file
write.csv(ecdf_stats, "ecdf_statistics.csv", row.names = FALSE)

# Function to perform pairwise K-S tests for a single city across three years
perform_ks_test <- function(city_index) {
  # Read the corresponding raster files for this city from each folder
  raster_year1 <- raster(list.files(folder_year1, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year2 <- raster(list.files(folder_year2, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year3 <- raster(list.files(folder_year3, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year4 <- raster(list.files(folder_year4, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year5 <- raster(list.files(folder_year5, pattern = "\\.tif$", full.names = TRUE)[city_index])
  
  # Extract non-NA values from each raster
  values_year1 <- getValues(raster_year1)[!is.na(getValues(raster_year1))]
  values_year2 <- getValues(raster_year2)[!is.na(getValues(raster_year2))]
  values_year3 <- getValues(raster_year3)[!is.na(getValues(raster_year3))]
  values_year4 <- getValues(raster_year4)[!is.na(getValues(raster_year4))]
  values_year5 <- getValues(raster_year5)[!is.na(getValues(raster_year5))]
  
  # Perform pairwise K-S tests
  ks_test_1_2 <- ks.test(values_year1, values_year2)
  ks_test_1_3 <- ks.test(values_year1, values_year3)
  ks_test_1_4 <- ks.test(values_year1, values_year4)
  ks_test_1_5 <- ks.test(values_year1, values_year5)
  ks_test_2_3 <- ks.test(values_year2, values_year3)
  ks_test_2_4 <- ks.test(values_year2, values_year4)
  ks_test_2_5 <- ks.test(values_year2, values_year5)
  ks_test_3_4 <- ks.test(values_year3, values_year4)
  ks_test_3_5 <- ks.test(values_year3, values_year5)
  ks_test_4_5 <- ks.test(values_year4, values_year5)
  
  # Create a data frame to store results
  result <- data.frame(
    City = plot_titles[city_index],
    Comparison = c("Year 1 vs Year 2", "Year 1 vs Year 3", "Year 1 vs Year 4", "Year 1 vs Year 5"
                   , "Year 2 vs Year 3", "Year 2 vs Year 4", "Year 2 vs Year 5", "Year 3 vs Year 4"
                   , "Year 3 vs Year 5", "Year 4 vs Year 5"),
    D_statistic = c(ks_test_1_2$statistic, ks_test_1_3$statistic, ks_test_1_4$statistic,ks_test_1_5$statistic,
                    ks_test_2_3$statistic, ks_test_2_4$statistic, ks_test_2_5$statistic,
                    ks_test_3_4$statistic, ks_test_3_5$statistic, ks_test_4_5$statistic),
    P_value = c(ks_test_1_2$p.value, ks_test_1_3$p.value, ks_test_1_4$p.value, ks_test_1_5$p.value,
                ks_test_2_3$p.value, ks_test_2_4$p.value, ks_test_2_5$p.value,
                ks_test_3_4$p.value, ks_test_3_5$p.value, ks_test_4_5$p.value)
  )
  
  return(result)
}

# Apply the function to all cities and combine the results
ks_results <- do.call(rbind, lapply(1:4, perform_ks_test))

# View the results
print(ks_results)

# Save the results to a CSV file
write.csv(ks_results, "ks_test_results.csv", row.names = FALSE)



# Load necessary libraries
library(raster)
library(ggplot2)
library(gridExtra)

# Paths to the folders (replace with your actual paths)
folder_year1 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2016"
folder_year2 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2018"
folder_year3 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2020"
folder_year4 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2022"
folder_year5 <- "/Users/jayantabiswas/Library/CloudStorage/OneDrive-TheUniversityofMemphis/Khulna University/Modified_RSEI/Final output/2024"

# Custom plot titles for cities
plot_titles <- c("Chattogram", "Nagpur", "Pune", "Surat")

# Function to create a violin plot for a single city across three years
create_violin_plot <- function(city_index) {
  # Read the corresponding raster files for this city from each folder
  raster_year1 <- raster(list.files(folder_year1, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year2 <- raster(list.files(folder_year2, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year3 <- raster(list.files(folder_year3, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year4 <- raster(list.files(folder_year4, pattern = "\\.tif$", full.names = TRUE)[city_index])
  raster_year5 <- raster(list.files(folder_year5, pattern = "\\.tif$", full.names = TRUE)[city_index])
 
  # Extract non-NA values from each raster
  values_year1 <- getValues(raster_year1)[!is.na(getValues(raster_year1))]
  values_year2 <- getValues(raster_year2)[!is.na(getValues(raster_year2))]
  values_year3 <- getValues(raster_year3)[!is.na(getValues(raster_year3))]
  values_year4 <- getValues(raster_year4)[!is.na(getValues(raster_year4))]
  values_year5 <- getValues(raster_year5)[!is.na(getValues(raster_year5))]
  
  # Create a data frame for ggplot
  df <- data.frame(
    Value = c(values_year1, values_year2, values_year3, values_year4, values_year5),
    Year = factor(rep(c("2016", "2018", "2020", "2022", "2024"), 
                      times = c(length(values_year1), length(values_year2), length(values_year3),
                                length(values_year4), length(values_year5))))
  )
  
  # Generate violin plot comparing three years
  p <- ggplot(df, aes(x = Year, y = Value, fill = Year)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, position = position_dodge(width = 0.9)) +  # Boxplot for median and quartiles
    stat_summary(fun = "median", geom = "point", size = 2, color = "red") +  # Median point
    labs(title = plot_titles[city_index], x = "Year", y = "RSEI Value") +
    theme_minimal() +
    scale_fill_manual(values = c("2016" = "purple", "2018"= "skyblue", "2020" = "lightgreen", "2022"="lightyellow", "2024" = "salmon"))
  
  return(p)
}

# Generate violin plots for all 12 cities
plots <- lapply(1:4, create_violin_plot)

# Arrange all plots in a 4x3 grid
grid_plot <- marrangeGrob(plots, nrow = 2, ncol = 2)

# Save the arranged plots as a PNG file
png("violin_comparison_three_years.png", width = 1600, height = 1200, res = 150)
grid.draw(grid_plot)
dev.off()

