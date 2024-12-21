# Load required libraries
library(tidyverse)
library(ggthemes)

# Function to load and clean data
load_and_clean_data <- function(file_path) {
  # Safely load and clean the dataset
  tryCatch({
    read.csv(file_path, stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      filter(!is.na(Year) & Year != "N/A") %>%
      mutate(Year = as.integer(Year)) %>%
      select(-Rank) # Remove unnecessary column
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

# Function to summarize sales by year
summarize_sales_by_year <- function(data) {
  data %>%
    group_by(Year) %>%
    summarise(
      across(c(Global_Sales, NA_Sales, EU_Sales, JP_Sales, Other_Sales), 
             ~ sum(.x, na.rm = TRUE), 
             .names = "sum_{col}"),
      .groups = "drop"
    )
}

# Plot yearly sales trends
plot_sales_trends <- function(summary) {
  ggplot(summary, aes(x = Year)) +
    geom_line(aes(y = sum_Global_Sales, color = "Global Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_NA_Sales, color = "North America Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_EU_Sales, color = "Europe Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_JP_Sales, color = "Japan Sales"), linetype = "dashed") +
    geom_line(aes(y = sum_Other_Sales, color = "Other Sales"), linetype = "dashed") +
    scale_color_manual(
      name = "Sales",
      values = c("Global Sales" = "red", "North America Sales" = "blue",
                 "Europe Sales" = "green", "Japan Sales" = "orange", 
                 "Other Sales" = "yellow")
    ) +
    labs(
      title = "Yearly Sales Trends by Region",
      x = "Year",
      y = "Sales (in millions)"
    ) +
    theme_stata() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Function to find top N games by sales
top_games_by_sales <- function(data, top_n = 10) {
  data %>%
    group_by(Name) %>%
    summarise(
      sum_global_sales = sum(Global_Sales, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    arrange(desc(sum_global_sales)) %>%
    slice_head(n = top_n)
}

# Plot top N games by sales
plot_top_games <- function(top_games) {
  ggplot(top_games, aes(x = reorder(Name, sum_global_sales), y = sum_global_sales)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Top Games by Global Sales",
      x = "Game",
      y = "Sales (in millions)"
    ) +
    theme_stata()
}

# Function to calculate genre distribution
calculate_genre_distribution <- function(data) {
  data %>%
    group_by(Genre) %>%
    summarise(
      sum_global_sales = sum(Global_Sales, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    mutate(percentage = sum_global_sales / sum(sum_global_sales) * 100)
}

# Plot genre distribution
plot_genre_distribution <- function(genre_dist) {
  ggplot(genre_dist, aes(x = "", y = percentage, fill = Genre)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    labs(title = "Genre Distribution by Global Sales") +
    geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
    theme_void() +
    theme(legend.position = "right")
}

# Function to find top N publishers by sales
top_publishers_by_sales <- function(data, top_n = 20) {
  data %>%
    group_by(Publisher) %>%
    summarise(
      sum_global_sales = sum(Global_Sales, na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    arrange(desc(sum_global_sales)) %>%
    slice_head(n = top_n)
}

# Plot top N publishers by sales
plot_top_publishers <- function(publishers) {
  ggplot(publishers, aes(x = reorder(Publisher, sum_global_sales), y = sum_global_sales)) +
    geom_col(fill = "darkorange") +
    coord_flip() +
    labs(
      title = "Top Publishers by Global Sales",
      x = "Publisher",
      y = "Sales (in millions)"
    ) +
    theme_stata()
}

# Main script
if (interactive()) {
  games <- load_and_clean_data("vgsales.csv")
  
  yearly_sales <- summarize_sales_by_year(games)
  print(plot_sales_trends(yearly_sales))
  
  top_games <- top_games_by_sales(games, top_n = 10)
  print(plot_top_games(top_games))
  
  genre_dist <- calculate_genre_distribution(games)
  print(plot_genre_distribution(genre_dist))
  
  top_publishers <- top_publishers_by_sales(games, top_n = 20)
  print(plot_top_publishers(top_publishers))
}
