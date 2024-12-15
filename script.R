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
