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

# Plot yearly sales trends
plot_sales_trends <- function(summary) {
  """
  Create a line plot showing sales trends over the years.

  Args:
    summary: Tibble with summarized sales data.
  """
  ggplot(summary, aes(x = Year)) +
    geom_line(aes(y = sum_global_sales, color = "Global Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_na_sales, color = "North America Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_eu_sales, color = "Europe Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_jp_sales, color = "Japan Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_other_sales, color = "Other Sales"), linetype = "dashed") +
    scale_color_manual(
      name = "Sales",
      values = c("Global Sales" = "red", "North America Sales" = "blue",
                 "Europe Sales" = "green", "Japan Sales" = "orange", 
                 "Other Sales" = "yellow")
    ) +
    ggtitle("Yearly Sales Trends by Region") +
    xlab("Year") + ylab("Sales (in millions)") +
    theme_stata() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


# Function to find top N games by sales
top_games_by_sales <- function(data, top_n = 10) {
  """
  Identify the top N games by global sales.

  Args:
    data: Cleaned games dataset.
    top_n: Number of top games to return.

  Returns:
    A tibble of top N games.
  """
  top_games <- data %>%
    group_by(Name) %>%
    summarise(sum_global_sales = sum(Global_Sales, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(sum_global_sales)) %>%
    slice_head(n = top_n)
  return(top_games)
}

# Plot top N games by sales
plot_top_games <- function(top_games) {
  """
  Create a bar chart for the top N games by sales.

  Args:
    top_games: Tibble of top games by sales.
  """
  ggplot(top_games, aes(x = reorder(Name, sum_global_sales), y = sum_global_sales)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    ggtitle("Top Games by Global Sales") +
    xlab("Game") + ylab("Sales (in millions)") +
    theme_stata()
}

# Function to calculate genre distribution
calculate_genre_distribution <- function(data) {
  """
  Calculate percentage distribution of global sales by genre.

  Args:
    data: Cleaned games dataset.

  Returns:
    A tibble with percentage sales by genre.
  """
  genre_dist <- data %>%
    group_by(Genre) %>%
    summarise(sum_global_sales = sum(Global_Sales, na.rm = TRUE), .groups = "drop") %>%
    mutate(percentage = sum_global_sales / sum(sum_global_sales) * 100)
  return(genre_dist)
}




