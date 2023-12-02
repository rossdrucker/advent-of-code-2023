
# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)

# read file
data <- read_delim(
  "day-2/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
)

# Part 1 -----------------------------------------------------------------------

# challenge: identify which games would have been possible if bag had 12 red
#            cubes, 13 green cubes, and 14 blue cubes
data <- data %>% 
  mutate(
    input_strs = tolower(input_strs)
    , game_name = unlist(str_split(input_strs, ": "))[seq(1, 2 * nrow(data), 2)]
    , game_info = unlist(str_split(input_strs, ": "))[seq(2, 2 * nrow(data), 2)]
    , n_rounds = str_count(game_info, ";") + 1
    , max_r = 0
    , max_g = 0
    , max_b = 0
  ) %>% 
  select(-input_strs)

for (i in 1:nrow(data)) {
  data$max_r[i] <- str_extract_all(data$game_info[i], "[0-9]+ red") %>% 
    unlist() %>% 
    str_replace_all(" red", "") %>% 
    as.numeric() %>% 
    max()
  data$max_g[i] <- str_extract_all(data$game_info[i], "[0-9]+ green") %>% 
    unlist() %>% 
    str_replace_all(" green", "") %>% 
    as.numeric() %>% 
    max()
  data$max_b[i] <- str_extract_all(data$game_info[i], "[0-9]+ blue") %>% 
    unlist() %>% 
    str_replace_all(" blue", "") %>% 
    as.numeric() %>% 
    max()
}

data %>% 
  filter(
    max_r <= 12
    , max_g <= 13
    , max_b <= 14
  ) %>% 
  mutate(
    game_id_no = str_replace(game_name, "game ", "") %>% 
      as.numeric
  ) %>% 
  summarize(
    sum_game_ids = sum(game_id_no)
  )

# Part 2 -----------------------------------------------------------------------

# challenge: determine minimal number of cubes necessary to play each game
data %>% 
  mutate(
    power = max_r * max_g * max_b
  ) %>% 
  summarize(
    total_power = sum(power)
  )
