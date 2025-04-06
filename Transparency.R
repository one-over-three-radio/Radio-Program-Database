# Load necessary libraries
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(gtools)
library(lubridate)
library(ggplot2)
library(readr)

#upload database and Voting files and format them to be used

Database <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/1:3/I:III_Data/Data_bass/Database.csv")
Voting <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/1:3/I:III_Data/Data_bass/Voting.csv")


# Create the Artist_Counts table

Artist_Counts <- data.frame(
  Artists = unique(Database$Artist),  # First column: List of unique artists
  Total_Songs = sapply(unique(Database$Artist), function(artist) sum(Database$Artist == artist)),  # Second column: Count of total songs
  Songs_Played = sapply(unique(Database$Artist), function(artist) sum(Database$Artist == artist & grepl("I/III", Database$Grouping))),  # Third column: Count of "I/III" in Grouping
  Songs_Picked = sapply(unique(Database$Artist), function(artist) sum(Database$Artist == artist & grepl("Picked", Database$Grouping))),  # Fourth column: Count of "Picked" in Grouping
  Songs_Paid = sapply(unique(Database$Artist), function(artist) sum(Database$Artist == artist & grepl("Paid", Database$Grouping)))  # Fifth column: Count of "Paid" in Grouping
)
row.names(Artist_Counts) <- NULL


# Create the Running_List data frame
Running_List <- Database %>%
  filter(str_detect(Grouping, "I/III")) %>%  # Keep only rows where "I/III" appears in Grouping
  mutate(Show = str_extract(Grouping, "(?<=I/III)\\_\\d{3}")) %>%  # Extract the four characters following "I/III"
  select(Artists = Artist, Name, Album, Show, Grouping, Year, Comments ) %>%  # Select relevant columns and rename them
  arrange(Show)  # Sort by the Show column alphabetically


####adjust show number for coresponding time period 
####adjust sample number if more artits can be chosen 

# Select two random rows with Show 
selected_rows <- Running_List %>%
  filter(as.numeric(str_extract(Show, "\\d{3}")) >= 47) %>%  # Filter rows with numeric Show >= 39
  slice_sample(n = 2)  # Randomly select 2 rows

print(selected_rows)





# Save Running_List as CSV
write.csv(Running_List, "Running_List.csv", row.names = FALSE)

# Save Artist Counts as CSV
write.csv(Artist_Counts, "Artist_Counts.csv", row.names = FALSE)

