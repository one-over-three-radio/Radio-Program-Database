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




#Update SCORESS 


# Initialize Voting_Elements with elements and default scores
Voting_Elements <- data.frame(
  Elements = aesthetics_lists, 
  Value_Score = rep(1, length(aesthetics_lists))
)

# Define a helper function for multiplier logic
get_multiplier <- function(episode, weeks_difference) {
  if (episode >= 1) {
    return(case_when(
      weeks_difference <= 1 ~ 0.10,
      weeks_difference <= 2 ~ 0.15,
      weeks_difference <= 3 ~ 0.20,
      weeks_difference <= 4 ~ 0.25,
      weeks_difference <= 5 ~ 0.30,
      weeks_difference <= 6 ~ 0.35,
      weeks_difference <= 7 ~ 0.40,
      weeks_difference <= 8 ~ 0.45,
      TRUE ~ 0.50
    ))
  } else {
    return(case_when(
      weeks_difference <= 1 ~ 0.60,
      weeks_difference <= 2 ~ 0.65,
      weeks_difference <= 3 ~ 0.70,
      weeks_difference <= 4 ~ 0.75,
      weeks_difference <= 5 ~ 0.80,
      weeks_difference <= 6 ~ 0.85,
      weeks_difference <= 7 ~ 0.90,
      TRUE ~ 0.95
    ))
  }
}

# Pre-filter Voting to exclude rows with NA in Episode
Voting <- Voting %>% filter(!is.na(Episode))

# Pre-lowercase Aesthetic_Used column for efficiency
Voting <- Voting %>% mutate(Aesthetic_Used_lower = tolower(Aesthetic_Used))

# Initialize temporary score vector
temp_scores <- rep(1, length(aesthetics_lists))

# Loop through aesthetics_list and calculate product scores
for (j in seq_along(aesthetics_lists)) {
  element <- aesthetics_lists[j]
  element_lower <- tolower(element) # Pre-lowercase for comparison
  product_score <- 1
  element_found <- FALSE
  
  for (i in 1:nrow(Voting)) {
    row <- Voting[i, ]
    
    # Check if the element appears in Aesthetic_Used
    if (str_detect(row$Aesthetic_Used_lower, fixed(element_lower, ignore_case = TRUE))) {
      element_found <- TRUE
      
      #Format sy date 
      formatted_date <- format(Sys.Date(), "%d-%m-%Y")
      
      # Calculate weeks difference
      weeks_difference <- as.numeric(difftime(formatted_date, dmy(row$Last_Voted), units = "weeks"))
      
      # Multiplier logic
      multiplier <- get_multiplier(row$Episode, weeks_difference)
      
      # Update product score
      product_score <- product_score * multiplier
    }
  }
  
  # Update temp_scores if the element was found
  if (element_found) {
    temp_scores[j] <- product_score
  }
}

# Update the Value_Score column in Voting_Elements
Voting_Elements$Value_Score <- temp_scores

# Output the updated Voting_Elements
print(Voting_Elements)

# Add Score column to Voting_Options
Voting_Options <- Voting_Options %>%
  rowwise() %>%
  mutate(Score = {
    # Find matching elements
    matching_elements <- Voting_Elements %>%
      filter(str_detect(Aesthetics, fixed(Elements, ignore_case = TRUE)))
    
    # Calculate the average value score if matches are found
    if (nrow(matching_elements) > 0) {
      mean(matching_elements$Value_Score)
    } else {
      NA_real_
    }
  })

# Add the Final_Score column with conditional logic
Voting_Options <- Voting_Options %>%
  mutate(Score = if_else(Can_Artist_Remaining >= 10, Score * 1, Score * 0))

# Perform a left join
Voting_Options <- Voting_Options %>%
  left_join(Voting, by = c("Aesthetics" = "Aesthetic_Used"))

print(Voting_Options)





#I/III Classic pick list
# Create 'Classic' data frame excluding rows with 'Afternoon', 'Evening', or 'Night' in 'Aesthetics'
Classic <- Voting_Options %>%
  filter(!str_detect(Aesthetics, "Afternoon|Evening|Night"))%>%
  filter(is.na(Episode) | Episode == 0)%>%
  select(-Aesthetic_Used_lower)%>%
  filter(Can_Artist_Remaining >= 10)

#time Pick list
# Create 'Field_Time' data frame excluding rows with 'Afternoon', 'Evening', or 'Night' in 'Aesthetics'
Field_Time <- Voting_Options %>%
  filter(str_detect(Aesthetics, "Afternoon|Evening|Night"))%>%
  filter(is.na(Episode) | Episode == 0)%>%
  select(-Aesthetic_Used_lower)%>%
  filter(Can_Artist_Remaining >= 10)


# List of Aesthetics for Recommendations
Aesthetic_Recommendations <- Voting_Options %>%
  filter(Can_Artist_Remaining >= 15) %>%
  select(Aesthetics)

