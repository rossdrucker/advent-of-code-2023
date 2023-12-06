
# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)
library(tibble)

# read file
data <- read_delim(
  "day-6/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
)

times <- data$input_strs[1] %>% 
  str_split(" +") %>% 
  unlist() %>% 
  .[2:(length(.))]

distances <- data$input_strs[2] %>% 
  str_split(" +") %>% 
  unlist() %>% 
  .[2:(length(.))]

data_tibble <- tibble(
  time = times %>% 
    as.numeric()
  , distance = distances %>% 
    as.numeric()
)

get_n_ways_to_win <- function(race_time, distance_to_beat) {
  tibble(
    possible_hold_times = 1:(race_time - 1)
  ) %>% 
    mutate(
      possible_travel_times = race_time - possible_hold_times
      , speeds = possible_hold_times
      , distance_traveled = speeds * possible_travel_times
    ) %>% 
    filter(
      distance_traveled > distance_to_beat
    ) %>% 
    nrow()
}

# Part 1 -----------------------------------------------------------------------

part_1_ans <- 1

for(i in 1:nrow(data_tibble)) {
  race_time <- data_tibble$time[i]
  distance_to_beat <- data_tibble$distance[i]
  part_1_ans = part_1_ans * get_n_ways_to_win(race_time, distance_to_beat)
}

message(glue("Part 1 answer: {part_1_ans}"))

# Part 2 -----------------------------------------------------------------------

race_time <- glue_collapse(times) %>% as.numeric()
distance_to_beat <- glue_collapse(distances) %>% as.numeric()

part_2_ans <- get_n_ways_to_win(race_time, distance_to_beat)

message(glue("Part 2 answer: {part_2_ans}"))

