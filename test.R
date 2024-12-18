library(tidyverse)

# Function to load and clean data
load_and_clean_data <- function(file_path) {
  games <- read.csv(file_path, stringsAsFactors = FALSE) %>%
    filter(Year != "N/A") %>%
    mutate(Year = as.integer(Year)) %>% # Convert Year to integer
    select(-Rank) # Remove unnecessary column
  return(games)
}
