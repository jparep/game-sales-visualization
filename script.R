# Load required libraries
library(tidyverse)
library(ggthemes)

# Function to load and clean data
load_and_clean_data <- function(file_path) {
  """
  Load the dataset, clean missing or unnecessary data, and prepare it for analysis.

  Args:
    file_path: Path to the CSV file.

  Returns:
    A cleaned tibble with relevant data.
  """
  games <- read.csv(file_path, stringsAsFactors = FALSE) %>%
    filter(Year != "N/A") %>%
    mutate(Year = as.integer(Year)) %>% # Convert Year to integer
    select(-Rank) # Remove unnecessary column
  return(games)
}


# Function to summarize sales by year
summarize_sales_by_year <- function(data) {
  """
  Summarize sales data by year for different regions.

  Args:
    data: Cleaned games dataset.

  Returns:
    A tibble with yearly sales summaries.
  """
  summary <- data %>%
    group_by(Year) %>%
    summarise(
      sum_global_sales = sum(Global_Sales, na.rm = TRUE),
      sum_na_sales = sum(NA_Sales, na.rm = TRUE),
      sum_eu_sales = sum(EU_Sales, na.rm = TRUE),
      sum_jp_sales = sum(JP_Sales, na.rm = TRUE),
      sum_other_sales = sum(Other_Sales, na.rm = TRUE),
      .groups = "drop"
    )
  return(summary)
}
