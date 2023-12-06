
# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)

# read file
data <- read_delim(
  "day-5/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
  , skip_empty_rows = FALSE
)

# Part 1 -----------------------------------------------------------------------

# identify where new map begins
blank_rows <- which(is.na(data$input_strs))

# store all maps
seeds <- data$input_strs[1:(blank_rows[1] - 1)] %>% 
  str_split(": ") %>% 
  unlist() %>% 
  .[2] %>% 
  str_split(" ") %>% 
  unlist() %>% 
  as.numeric()

seed_to_soil <- data %>% 
  slice((blank_rows[1] + 2):(blank_rows[2] - 1)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)

soil_to_fertilizer <- data %>% 
  slice((blank_rows[2] + 2):(blank_rows[3] - 1)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)

fertilizer_to_water <- data %>% 
  slice((blank_rows[3] + 2):(blank_rows[4] - 1)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)

water_to_light <- data %>% 
  slice((blank_rows[4] + 2):(blank_rows[5] - 1)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)

light_to_temperature <- data %>% 
  slice((blank_rows[5] + 2):(blank_rows[6] - 1)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)
  
temperature_to_humidity <- data %>% 
  slice((blank_rows[6] + 2):(blank_rows[7] - 1)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)
  
humidity_to_location <- data %>% 
  slice((blank_rows[7] + 2):nrow(data)) %>% 
  mutate(
    dest_range_start = unlist(str_split(input_strs, " "))[
      seq(1, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , source_range_start = unlist(str_split(input_strs, " "))[
      seq(2, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
    , range_length = unlist(str_split(input_strs, " "))[
      seq(3, length(unlist(str_split(input_strs, " "))), 3)
    ] %>% as.numeric()
  ) %>% 
  select(-input_strs)

get_destination <- function(inp, passed_map) {
  destination <- inp
  possible_rows <- passed_map %>% 
    filter(
      source_range_start <= inp
      , inp < source_range_start + range_length
    )
  
  if (nrow(possible_rows) > 0) {
    destination <- possible_rows %>% 
      mutate(
        destination = inp - source_range_start + dest_range_start
      ) %>% 
      pull(destination)
  }
  
  return(destination)
}

locations <- c()
for (seed_no in 1:length(seeds)) {
  seed <- seeds[seed_no]
  soil <- get_destination(seed, seed_to_soil)
  fertilizer <- get_destination(soil, soil_to_fertilizer)
  water <- get_destination(fertilizer, fertilizer_to_water)
  light <- get_destination(water, water_to_light)
  temperature <- get_destination(light, light_to_temperature)
  humidity <- get_destination(temperature, temperature_to_humidity)
  location <- get_destination(humidity, humidity_to_location)
  locations <- c(locations, location)
}

message(glue("Part 1 answer: {min(locations)}"))

# Part 2 -----------------------------------------------------------------------

seeds_tibble <- tibble(
  seed_id = seeds[seq(1, length(seeds), 2)]
  , seed_range = seeds[seq(2, length(seeds), 2)]
) %>% 
  mutate(
    range_start = seed_id
    , range_end = seed_id + seed_range
  )

# default value to optimize against
lowest_location <- Inf

seed_maps <- list(
  "seed_to_soil" = seed_to_soil
  , "soil_to_fertilizer" = soil_to_fertilizer
  , "fertilizer_to_water" = fertilizer_to_water
  , "water_to_light" = water_to_light
  , "light_to_temperature" = light_to_temperature
  , "temperature_to_humidity" = temperature_to_humidity
  , "humidity_to_location" = humidity_to_location
)

for (i in 1:nrow(seeds_tibble)) {
  current_ranges <- list(
    c(
      seeds_tibble$range_start[i]
      , seeds_tibble$range_end[i]
    )
  )
  
  for (j in 1:length(seed_maps)) {
    seed_map <- seed_maps[[j]]
    
    mapped_ranges <- list()
    
    for (k in 1:nrow(seed_map)) {
      start <- seed_map$source_range_start[k]
      end <- seed_map$source_range_start[k] + seed_map$range_length[k]
      
      new_ranges <- list()
      
      while(length(current_ranges) > 0) {
        current_range <- current_ranges[[length(current_ranges)]]
        current_ranges[[length(current_ranges)]] <- NULL
        
        lwr <- c(current_range[1], min(current_range[2], start))
        mid <- c(max(current_range[1], start), min(current_range[2], end))
        upr <- c(max(current_range[1], end), current_range[2])
        
        if (lwr[1] < lwr[2]) {
          new_ranges <- c(
            new_ranges, list(lwr)
          )
        }
        
        if (upr[1] < upr[2]) {
          new_ranges <- c(
            new_ranges, list(upr)
          )
        }
        
        if (mid[1] < mid[2]) {
          mapped_mid <- c(
            mid[1] - start + seed_map$dest_range_start[k]
            , mid[2] - start + seed_map$dest_range_start[k]
          )
          
          mapped_ranges <- c(mapped_ranges, list(mapped_mid))
        }
      }
      current_ranges <- new_ranges
    }
    current_ranges <- c(current_ranges, mapped_ranges)
  }
  lowest_location <- min(lowest_location, min(sapply(current_ranges, `[[`, 1)))
}

message(glue("Part 2 answer: {lowest_location}"))

