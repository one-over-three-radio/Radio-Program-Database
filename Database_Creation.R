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




#
#
###creating voting options
#
#




# Aesthetic options
aesthetics_list <- c("Alt", "Americana", "Barnburner", "Beach", "Country", "Dream", 
                     "Electronic", "Folk", "Garage", "Indie", "Lofi", "Pop", "Post", 
                     "Prog", "Psych", "Punk", "R&B", "Rock")

# Function to generate valid combinations
generate_combinations <- function(aesthetics) {
  # Generate 1, 2, and 3 item combinations
  combos_1 <- combinations(length(aesthetics), 1, aesthetics)
  combos_2 <- combinations(length(aesthetics), 2, aesthetics)
  combos_3 <- combinations(length(aesthetics), 3, aesthetics)
  
  # Convert combos to lists of strings
  combos_1_str <- apply(combos_1, 1, function(x) paste(x, collapse = "/"))
  combos_2_str <- apply(combos_2, 1, function(x) paste(x, collapse = "/"))
  combos_3_str <- apply(combos_3, 1, function(x) paste(x, collapse = "/"))
  
  # Combine all combos into a single list
  
  all_combos <- c(combos_1_str, combos_2_str, combos_3_str)
  
  
  
  # Create a data frame with unique aesthetic combinations
  combos_df <- data.frame(Aesthetics = unique(all_combos))
  
  # Remove invalid combinations (e.g., "Barnburner" with "Americana", "Country", or "Folk")
  combos_df <- combos_df %>%
    filter(!(str_detect(Aesthetics, "Barnburner") & 
             str_detect(Aesthetics, "Americana|Country|Folk")))
  return(combos_df)
}

# Create the combinations data
Voting_Options <- generate_combinations(aesthetics_list)


# List of time-of-day combinations
time_combinations <- c("Afternoon", "Evening", "Night")

# Assuming Voting_Options is a data frame with a column 'Aesthetics'
extend_with_times <- function(voting_options, time_combos) {
  
  # Extract the aesthetics column
  aesthetics_list <- Voting_Options$Aesthetics
  
  # Use expand.grid to combine each aesthetic with all time combos
  combinations_df <- expand.grid(Aesthetic = aesthetics_list, Time = time_combinations, 
                                 stringsAsFactors = FALSE)
  
  # Combine the aesthetic and time strings with a '/'
  combinations_df$Combined <- paste(combinations_df$Aesthetic, combinations_df$Time, sep = "/")
  
  # Return the new extended list of options
  return(combinations_df$Combined)
}

Voting_Options_vibe <- extend_with_times(time_combinations)
Voting_Options_Vibe <- data.frame(Voting_Options_vibe)


# Combine both lists into a new data frame (without worrying about matching lengths)
Voting_Options <- data.frame(Aesthetics = c(Voting_Options$Aesthetics, Voting_Options_Vibe$Voting_Options_vibe))









# Define functions for counting unique artists (with the new logic for "Barnburner")
count_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  # Check if "Barnburner" is in the combination
  if ("Barnburner" %in% genres_in_combo) {
    # If "Barnburner" is included, check for "Folk/Rock" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Afternoon" %in% genres_in_combo) {
    # If "Afternoon" is included, check for "Afternoon" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Afternoon"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Evening" %in% genres_in_combo) {
    # If "Evening" is included, check for "Evening" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Evening"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Night" %in% genres_in_combo) {
    # If "Night" is included, check for "Night" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Night")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Night"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    # Otherwise, follow the usual genre matching rules
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}
count_canadian_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  # Check if "Barnburner" is in the combination
  if ("Barnburner" %in% genres_in_combo) {
    # If "Barnburner" is included, check for "Folk/Rock" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Afternoon" %in% genres_in_combo) {
    # If "Afternoon" is included, check for "Afternoon" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Afternoon"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Evening" %in% genres_in_combo) {
    # If "Evening" is included, check for "Evening" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Evening"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Night" %in% genres_in_combo) {
    # If "Night" is included, check for "Night" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Night")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Night"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  }  else {
    # Otherwise, follow the usual genre matching rules
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}

count_canadian_remaining <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  # Check if "Barnburner" is in the combination
  if ("Barnburner" %in% genres_in_combo) {
    # If "Barnburner" is included, check for "Folk/Rock" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
    
  } 
  
  else if ("Afternoon" %in% genres_in_combo) {
    # If "Afternoon" is included, check for "Afternoon" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Afternoon"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Evening" %in% genres_in_combo) {
    # If "Evening" is included, check for "Evening" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Evening"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  } 
  
  else if ("Night" %in% genres_in_combo) {
    # If "Night" is included, check for "Night" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Night")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Night"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  }  else {
    # Otherwise, follow the usual genre matching rules
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}




# Apply the functions to fill in the columns
Voting_Options <- Voting_Options %>%
  mutate(
    Artist = map_int(Aesthetics, count_artists, data = Database),
    Can_Artist = map_int(Aesthetics, count_canadian_artists, data = Database),
    Can_Artist_Remaining = map_int(Aesthetics, count_canadian_remaining, data = Database)
  )
#don't continue until printed

print(Voting_Options)






#combine tags to one list

aesthetics_lists <-c(aesthetics_list, time_combinations)









#
#
#
#######SCORESS
#
#
#


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
  mutate(Score = if_else(Can_Artist_Remaining >= 8, Score * 1, Score * 0))


# Perform a left join
Voting_Options <- Voting_Options %>%
  left_join(Voting, by = c("Aesthetics" = "Aesthetic_Used"))



print(Voting_Options)






#
#
#
#Afternoon/Evening
#
#
#



# Generate combinations and create Voting_Options_Afternoon_Evening
Voting_Options_Afternoon_Evening <- generate_combinations(aesthetics_list)

# Define function to count unique artists, including "Afternoon_Evening"
count_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  if ("Barnburner" %in% genres_in_combo) {
    # Special logic for "Barnburner"
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Afternoon") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Afternoon") & str_detect(Grouping, "Evening")) %>%  # Include "Hits"
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}

# Define function to count Canadian artists, including "Afternoon_Evening"
count_canadian_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  if ("Barnburner" %in% genres_in_combo) {
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Afternoon") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & str_detect(Grouping, "Afternoon") & str_detect(Grouping, "Evening")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}

count_canadian_remaining <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  # Check if "Barnburner" is in the combination
  if ("Barnburner" %in% genres_in_combo) {
    # If "Barnburner" is included, check for "Folk/Rock" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Afternoon") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
    
  }  else {
    # Otherwise, follow the usual genre matching rules
    count <- data %>%
      filter(str_detect(Grouping, "Afternoon") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}


# Apply the functions to fill columns in Voting_Options_Afternoon_Evening
Voting_Options_Afternoon_Evening <- Voting_Options_Afternoon_Evening %>%
  mutate(
    Artist = map_int(Aesthetics, count_artists, data = Database),
    Can_Artist = map_int(Aesthetics, count_canadian_artists, data = Database),
    Can_Artist_Remaining = map_int(Aesthetics, count_canadian_remaining, data = Database)
  )

# Print to confirm the updated data
print(Voting_Options_Afternoon_Evening)


####add Afternoon/Evening to the end of each aesthetic


# Ensure the column exists
if("Aesthetics" %in% colnames(Voting_Options_Afternoon_Evening)) {
  Voting_Options_Afternoon_Evening$Aesthetics <- paste0(Voting_Options_Afternoon_Evening$Aesthetics, "/Afternoon/Evening")
} else {
  stop("Column 'Aesthetics' not found in the data frame")
}

# Print the modified data frame
print(Voting_Options_Afternoon_Evening)









# ---- SCORES ----

# Add Score column to Voting_Options_Afternoon_Evening
Voting_Options_Afternoon_Evening <- Voting_Options_Afternoon_Evening %>%
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

# Add Final_Score column using Can_Artist
Voting_Options_Afternoon_Evening <- Voting_Options_Afternoon_Evening %>%
  mutate(Score = if_else(Can_Artist >= 6, Score * 1, Score * 0))

# Perform a left join with Voting
Voting_Options_Afternoon_Evening <- Voting_Options_Afternoon_Evening %>%
  left_join(Voting, by = c("Aesthetics" = "Aesthetic_Used"))

# Print the final updated data frame
print(Voting_Options_Afternoon_Evening)










#
#
#
#Evening/Night
#
#
#



# Generate combinations and create Voting_Options_Evening_Night
Voting_Options_Evening_Night <- generate_combinations(aesthetics_list)

# Define function to count unique artists, including "Evening_Night"
count_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  if ("Barnburner" %in% genres_in_combo) {
    # Special logic for "Barnburner"
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Night") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Night") & str_detect(Grouping, "Evening")) %>%  # Include "Hits"
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}

# Define function to count Canadian artists, including "Evening_Night"
count_canadian_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  if ("Barnburner" %in% genres_in_combo) {
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Night") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & str_detect(Grouping, "Night") & str_detect(Grouping, "Evening")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}

count_canadian_remaining <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  # Check if "Barnburner" is in the combination
  if ("Barnburner" %in% genres_in_combo) {
    # If "Barnburner" is included, check for "Folk/Rock" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Night") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
    
  }  else {
    # Otherwise, follow the usual genre matching rules
    count <- data %>%
      filter(str_detect(Grouping, "Night") & str_detect(Grouping, "Evening")) %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}




# Apply the functions to fill columns in Voting_Options_Evening_Night
Voting_Options_Evening_Night <- Voting_Options_Evening_Night %>%
  mutate(
    Artist = map_int(Aesthetics, count_artists, data = Database),
    Can_Artist = map_int(Aesthetics, count_canadian_artists, data = Database),
    Can_Artist_Remaining = map_int(Aesthetics, count_canadian_remaining, data = Database)
  )

# Print to confirm the updated data
print(Voting_Options_Evening_Night)





####add Evening/Night to the end of each aesthetic


# Ensure the column exists
if("Aesthetics" %in% colnames(Voting_Options_Evening_Night)) {
  Voting_Options_Evening_Night$Aesthetics <- paste0(Voting_Options_Evening_Night$Aesthetics, "/Evening/Night")
} else {
  stop("Column 'Aesthetics' not found in the data frame")
}

# Print the modified data frame
print(Voting_Options_Evening_Night)





# ---- SCORES ----

# Add Score column to Voting_Options_Evening_Night
Voting_Options_Evening_Night <- Voting_Options_Evening_Night %>%
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

# Add Final_Score column using Can_Artist
Voting_Options_Evening_Night <- Voting_Options_Evening_Night %>%
  mutate(Score = if_else(Can_Artist >= 6, Score * 1, Score * 0))

# Perform a left join with Voting
Voting_Options_Evening_Night <- Voting_Options_Evening_Night %>%
  left_join(Voting, by = c("Aesthetics" = "Aesthetic_Used"))

# Print the final updated data frame
print(Voting_Options_Evening_Night)


#
#
#
#Afternoon/Night
#
#
#



# Generate combinations and create Voting_Options_Afternoon_Night
Voting_Options_Afternoon_Night <- generate_combinations(aesthetics_list)

# Define function to count unique artists, including "Afternoon_Night"
count_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  if ("Barnburner" %in% genres_in_combo) {
    # Special logic for "Barnburner"
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Night") & str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Night") & str_detect(Grouping, "Afternoon")) %>%  # Include "Hits"
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}

# Define function to count Canadian artists, including "Afternoon_Night"
count_canadian_artists <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  if ("Barnburner" %in% genres_in_combo) {
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Night") & str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian")) %>%
      distinct(Artist) %>%
      nrow()
  } else {
    count <- data %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, 
                                         function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & str_detect(Grouping, "Night") & str_detect(Grouping, "Afternoon")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}


count_canadian_remaining <- function(combo, data) {
  genres_in_combo <- str_split(combo, "/")[[1]]
  
  # Check if "Barnburner" is in the combination
  if ("Barnburner" %in% genres_in_combo) {
    # If "Barnburner" is included, check for "Folk/Rock" in Grouping
    count <- data %>%
      filter(str_detect(Grouping, "Folk/Rock") & str_detect(Grouping, "Night") & str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(setdiff(genres_in_combo, "Barnburner"), function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
    
  }  else {
    # Otherwise, follow the usual genre matching rules
    count <- data %>%
      filter(str_detect(Grouping, "Night") & str_detect(Grouping, "Afternoon")) %>%
      filter(map_lgl(Genre, ~ all(sapply(genres_in_combo, function(genre) str_detect(.x, fixed(genre)))))) %>%
      filter(str_detect(Grouping, "Canadian") & !str_detect(Grouping, "I/III")) %>%
      distinct(Artist) %>%
      nrow()
  }
  return(count)
}



# Apply the functions to fill columns in Voting_Options_Afternoon_Night
Voting_Options_Afternoon_Night <- Voting_Options_Afternoon_Night %>%
  mutate(
    Artist = map_int(Aesthetics, count_artists, data = Database),
    Can_Artist = map_int(Aesthetics, count_canadian_artists, data = Database),
    Can_Artist_Remaining = map_int(Aesthetics, count_canadian_remaining, data = Database)
  )

# Print to confirm the updated data
print(Voting_Options_Afternoon_Night)




####add Afternoon/Night to the end of each aesthetic


# Ensure the column exists
if("Aesthetics" %in% colnames(Voting_Options_Afternoon_Night)) {
  Voting_Options_Afternoon_Night$Aesthetics <- paste0(Voting_Options_Afternoon_Night$Aesthetics, "/Afternoon/Night")
} else {
  stop("Column 'Aesthetics' not found in the data frame")
}

# Print the modified data frame
print(Voting_Options_Afternoon_Night)








# ---- SCORES ----

# Add Score column to Voting_Options_Afternoon_Night
Voting_Options_Afternoon_Night <- Voting_Options_Afternoon_Night %>%
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

# Add Final_Score column using Can_Artist
Voting_Options_Afternoon_Night <- Voting_Options_Afternoon_Night %>%
  mutate(Score = if_else(Can_Artist >= 6, Score * 1, Score * 0))

# Perform a left join with Voting
Voting_Options_Afternoon_Night <- Voting_Options_Afternoon_Night %>%
  left_join(Voting, by = c("Aesthetics" = "Aesthetic_Used"))

# Print the final updated data frame
print(Voting_Options_Afternoon_Night)



# Combine data frames into Voting_Options 
Voting_Options <- bind_rows(Voting_Options,
                            Voting_Options_Evening_Night, 
                            Voting_Options_Afternoon_Night, 
                            Voting_Options_Afternoon_Evening)

# Print the modified data frame
print(Voting_Options)


#remove duplicates 
Voting_Options <- Voting_Options %>%
  distinct(Aesthetics, .keep_all = TRUE)


#I/III Classic pick list

# Create 'Classic' data frame excluding rows with 'Afternoon', 'Evening', or 'Night' in 'Aesthetics'
Classic <- Voting_Options %>%
  filter(!str_detect(Aesthetics, "Afternoon|Evening|Night"))%>%
  filter(is.na(Episode) | Episode == 0)%>%
  select(-Aesthetic_Used_lower)%>%
  filter(Can_Artist_Remaining >= 6)



#time Pick list

# Create 'Field_Time' data frame excluding rows with 'Afternoon', 'Evening', or 'Night' in 'Aesthetics'
Field_Time <- Voting_Options %>%
  filter(str_detect(Aesthetics, "Afternoon|Evening|Night"))%>%
  filter(is.na(Episode) | Episode == 0)%>%
  select(-Aesthetic_Used_lower)%>%
  filter(Can_Artist_Remaining >= 6)






