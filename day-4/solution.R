
# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)

# read file
data <- read_delim(
  "day-4/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
)

# Part 1 -----------------------------------------------------------------------

data_cleaned <- data %>% 
  mutate(
    card_no = unlist(str_split(input_strs, ": "))[seq(1, 2 * nrow(data), 2)]
    , card_nums = unlist(str_split(input_strs, ": "))[seq(2, 2 * nrow(data), 2)]
    , w_nums = unlist(str_split(card_nums, " \\| "))[seq(1, 2 * nrow(data), 2)]
    , my_nums = unlist(str_split(card_nums, " \\| "))[seq(2, 2 * nrow(data), 2)]
  ) %>% 
  select(-c(input_strs, card_nums))

points <- c()
for (i in 1:nrow(data_cleaned)) {
  w_nums_card <- unlist(str_split(data_cleaned$w_nums[i], " "))
  w_nums_card <- w_nums_card[w_nums_card != ""]
  my_nums_card <- unlist(str_split(data_cleaned$my_nums[i], " "))
  my_nums_card <- my_nums_card[my_nums_card != ""]
  n_matched <- sum(w_nums_card %in% my_nums_card)
  if (n_matched > 0) {
    points_from_card <- 2 ^ (n_matched - 1)
  } else {
    points_from_card <- 0
  }
  points <- c(points, points_from_card)
}

message(glue("Part 1 answer: {sum(points)}"))

# Part 2 -----------------------------------------------------------------------

n_card_reps <- rep(1, nrow(data_cleaned))
for (i in 1:nrow(data_cleaned)) {
  w_nums_card <- unlist(str_split(data_cleaned$w_nums[i], " "))
  w_nums_card <- w_nums_card[w_nums_card != ""]
  my_nums_card <- unlist(str_split(data_cleaned$my_nums[i], " "))
  my_nums_card <- my_nums_card[my_nums_card != ""]
  n_matched <- sum(w_nums_card %in% my_nums_card)
  if (n_matched > 0) {
    card_i <- (i + 1):(i + n_matched)
    card_i <- card_i[card_i <= nrow(data_cleaned)]
    n_card_reps[card_i] <- n_card_reps[card_i] + n_card_reps[i]
  }
}

message(glue("Part 2 answer: {sum(n_card_reps)}"))
