
# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)
library(tibble)
library(tidyr)

# read file
data <- read_delim(
  "day-7/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
)

# Part 1 -----------------------------------------------------------------------
total_winnings_1 <- data %>% 
  mutate(
    hand = unlist(str_split(input_strs, " "))[seq(1, 2 * nrow(data), 2)]
    , bid = unlist(str_split(input_strs, " "))[seq(2, 2 * nrow(data), 2)] %>% 
      as.numeric()
    , card_1 = str_sub(hand, 1, 1)
    , card_2 = str_sub(hand, 2, 2)
    , card_3 = str_sub(hand, 3, 3)
    , card_4 = str_sub(hand, 4, 4)
    , card_5 = str_sub(hand, 5, 5)
  ) %>% 
  pivot_longer(
    cols = starts_with("card")
  ) %>% 
  count(hand, bid, value) %>% 
  arrange(
    hand
    , bid
    , -n
  ) %>% 
  mutate(
    colname = "type"
  ) %>% 
  mutate(
    order = row_number()
  ) %>% 
  group_by(
    hand
    , bid
  ) %>% 
  mutate(
    order = row_number()
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c("hand", "bid")
    , names_from = c("colname", "order")
    , values_from = c("n")
  ) %>% 
  mutate(
    priority = case_when(
        type_1 == 5 ~ "1 - Five of a Kind"
      , type_1 == 4 ~ "2 - Four of a Kind"
      , type_1 == 3 & type_2 == 2 ~ "3 - Full House"
      , type_1 == 3 ~ "4 - Three of a Kind"
      , type_1 == 2 & type_2 == 2 ~ "5 - Two pair"
      , type_1 == 2 ~ "6 - One pair"
      , TRUE ~ "7 - High Card"
    )
  ) %>% 
  mutate(
      card_1 = str_sub(hand, 1, 1)
    , card_2 = str_sub(hand, 2, 2)
    , card_3 = str_sub(hand, 3, 3)
    , card_4 = str_sub(hand, 4, 4)
    , card_5 = str_sub(hand, 5, 5)
    , card_1_value = case_when(
        str_sub(hand, 1, 1) == "2" ~ 2
      , str_sub(hand, 1, 1) == "3" ~ 3
      , str_sub(hand, 1, 1) == "4" ~ 4
      , str_sub(hand, 1, 1) == "5" ~ 5
      , str_sub(hand, 1, 1) == "6" ~ 6
      , str_sub(hand, 1, 1) == "7" ~ 7
      , str_sub(hand, 1, 1) == "8" ~ 8
      , str_sub(hand, 1, 1) == "9" ~ 9
      , str_sub(hand, 1, 1) == "T" ~ 10
      , str_sub(hand, 1, 1) == "J" ~ 11
      , str_sub(hand, 1, 1) == "Q" ~ 12
      , str_sub(hand, 1, 1) == "K" ~ 13
      , str_sub(hand, 1, 1) == "A" ~ 14
    )
    , card_2_value = case_when(
        str_sub(hand, 2, 2) == "2" ~ 2
      , str_sub(hand, 2, 2) == "3" ~ 3
      , str_sub(hand, 2, 2) == "4" ~ 4
      , str_sub(hand, 2, 2) == "5" ~ 5
      , str_sub(hand, 2, 2) == "6" ~ 6
      , str_sub(hand, 2, 2) == "7" ~ 7
      , str_sub(hand, 2, 2) == "8" ~ 8
      , str_sub(hand, 2, 2) == "9" ~ 9
      , str_sub(hand, 2, 2) == "T" ~ 10
      , str_sub(hand, 2, 2) == "J" ~ 11
      , str_sub(hand, 2, 2) == "Q" ~ 12
      , str_sub(hand, 2, 2) == "K" ~ 13
      , str_sub(hand, 2, 2) == "A" ~ 14
    )
    , card_3_value = case_when(
        str_sub(hand, 3, 3) == "2" ~ 2
      , str_sub(hand, 3, 3) == "3" ~ 3
      , str_sub(hand, 3, 3) == "4" ~ 4
      , str_sub(hand, 3, 3) == "5" ~ 5
      , str_sub(hand, 3, 3) == "6" ~ 6
      , str_sub(hand, 3, 3) == "7" ~ 7
      , str_sub(hand, 3, 3) == "8" ~ 8
      , str_sub(hand, 3, 3) == "9" ~ 9
      , str_sub(hand, 3, 3) == "T" ~ 10
      , str_sub(hand, 3, 3) == "J" ~ 11
      , str_sub(hand, 3, 3) == "Q" ~ 12
      , str_sub(hand, 3, 3) == "K" ~ 13
      , str_sub(hand, 3, 3) == "A" ~ 14
    )
    , card_4_value = case_when(
        str_sub(hand, 4, 4) == "2" ~ 2
      , str_sub(hand, 4, 4) == "3" ~ 3
      , str_sub(hand, 4, 4) == "4" ~ 4
      , str_sub(hand, 4, 4) == "5" ~ 5
      , str_sub(hand, 4, 4) == "6" ~ 6
      , str_sub(hand, 4, 4) == "7" ~ 7
      , str_sub(hand, 4, 4) == "8" ~ 8
      , str_sub(hand, 4, 4) == "9" ~ 9
      , str_sub(hand, 4, 4) == "T" ~ 10
      , str_sub(hand, 4, 4) == "J" ~ 11
      , str_sub(hand, 4, 4) == "Q" ~ 12
      , str_sub(hand, 4, 4) == "K" ~ 13
      , str_sub(hand, 4, 4) == "A" ~ 14
    )
    , card_5_value = case_when(
        str_sub(hand, 5, 5) == "2" ~ 2
      , str_sub(hand, 5, 5) == "3" ~ 3
      , str_sub(hand, 5, 5) == "4" ~ 4
      , str_sub(hand, 5, 5) == "5" ~ 5
      , str_sub(hand, 5, 5) == "6" ~ 6
      , str_sub(hand, 5, 5) == "7" ~ 7
      , str_sub(hand, 5, 5) == "8" ~ 8
      , str_sub(hand, 5, 5) == "9" ~ 9
      , str_sub(hand, 5, 5) == "T" ~ 10
      , str_sub(hand, 5, 5) == "J" ~ 11
      , str_sub(hand, 5, 5) == "Q" ~ 12
      , str_sub(hand, 5, 5) == "K" ~ 13
      , str_sub(hand, 5, 5) == "A" ~ 14
    )
  ) %>% 
  arrange(
      desc(priority)
    , card_1_value
    , card_2_value
    , card_3_value
    , card_4_value
    , card_5_value
  ) %>% 
  select(hand, bid, priority) %>%
  mutate(
    rank = row_number()
    , winnings = bid * rank
  ) %>%
  summarize(
    total_winnings = sum(winnings)
  ) %>% 
  pull(
    total_winnings
  )

# Part 2 -----------------------------------------------------------------------

total_winnings_2 <- data %>% 
  mutate(
    hand = unlist(str_split(input_strs, " "))[seq(1, 2 * nrow(data), 2)]
    , bid = unlist(str_split(input_strs, " "))[seq(2, 2 * nrow(data), 2)] %>% 
      as.numeric()
    , card_1 = str_sub(hand, 1, 1)
    , card_2 = str_sub(hand, 2, 2)
    , card_3 = str_sub(hand, 3, 3)
    , card_4 = str_sub(hand, 4, 4)
    , card_5 = str_sub(hand, 5, 5)
  ) %>% 
  pivot_longer(
    cols = starts_with("card")
  ) %>% 
  count(hand, bid, value) %>% 
  filter(value != "J") %>% 
  left_join(
    data %>% 
      mutate(
        hand = unlist(str_split(input_strs, " "))[seq(1, 2 * nrow(data), 2)]
        , bid = unlist(str_split(input_strs, " "))[seq(2, 2 * nrow(data), 2)] %>% 
          as.numeric()
        , card_1 = str_sub(hand, 1, 1)
        , card_2 = str_sub(hand, 2, 2)
        , card_3 = str_sub(hand, 3, 3)
        , card_4 = str_sub(hand, 4, 4)
        , card_5 = str_sub(hand, 5, 5)
      ) %>% 
      pivot_longer(
        cols = starts_with("card")
      ) %>% 
      count(hand, bid, value) %>% 
      filter(value == "J") %>% 
      rename(n_jokers = n) %>% 
      select(-value)
    , by = c("hand", "bid")
  ) %>% 
  mutate(
    n_jokers = ifelse(is.na(n_jokers), 0, n_jokers)
  ) %>% 
  arrange(
    hand
    , bid
    , -n
  ) %>% 
  group_by(
    hand
    , bid
  ) %>% 
  mutate(
    n = ifelse(row_number() == 1, n + n_jokers, n)
  ) %>% 
  ungroup() %>% 
  select(-n_jokers) %>% 
  bind_rows(
    data %>% 
      mutate(
        hand = unlist(str_split(input_strs, " "))[seq(1, 2 * nrow(data), 2)]
        , bid = unlist(str_split(input_strs, " "))[seq(2, 2 * nrow(data), 2)] %>% 
          as.numeric()
      ) %>% 
      filter(hand == "JJJJJ") %>% 
      select(
        hand
        , bid
      ) %>% 
      mutate(
        value = "J"
        , n = 5
      )
  ) %>% 
  arrange(
    hand
    , bid
    , -n
  ) %>% 
  mutate(
    colname = "type"
  ) %>% 
  mutate(
    order = row_number()
  ) %>% 
  group_by(
    hand
    , bid
  ) %>% 
  mutate(
    order = row_number()
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c("hand", "bid")
    , names_from = c("colname", "order")
    , values_from = c("n")
  ) %>% 
  mutate(
    priority = case_when(
      type_1 == 5 ~ "1 - Five of a Kind"
      , type_1 == 4 ~ "2 - Four of a Kind"
      , type_1 == 3 & type_2 == 2 ~ "3 - Full House"
      , type_1 == 3 ~ "4 - Three of a Kind"
      , type_1 == 2 & type_2 == 2 ~ "5 - Two pair"
      , type_1 == 2 ~ "6 - One pair"
      , TRUE ~ "7 - High Card"
    )
  ) %>% 
  mutate(
    card_1 = str_sub(hand, 1, 1)
    , card_2 = str_sub(hand, 2, 2)
    , card_3 = str_sub(hand, 3, 3)
    , card_4 = str_sub(hand, 4, 4)
    , card_5 = str_sub(hand, 5, 5)
    , card_1_value = case_when(
      str_sub(hand, 1, 1) == "2" ~ 2
      , str_sub(hand, 1, 1) == "3" ~ 3
      , str_sub(hand, 1, 1) == "4" ~ 4
      , str_sub(hand, 1, 1) == "5" ~ 5
      , str_sub(hand, 1, 1) == "6" ~ 6
      , str_sub(hand, 1, 1) == "7" ~ 7
      , str_sub(hand, 1, 1) == "8" ~ 8
      , str_sub(hand, 1, 1) == "9" ~ 9
      , str_sub(hand, 1, 1) == "T" ~ 10
      , str_sub(hand, 1, 1) == "J" ~ 1
      , str_sub(hand, 1, 1) == "Q" ~ 12
      , str_sub(hand, 1, 1) == "K" ~ 13
      , str_sub(hand, 1, 1) == "A" ~ 14
    )
    , card_2_value = case_when(
      str_sub(hand, 2, 2) == "2" ~ 2
      , str_sub(hand, 2, 2) == "3" ~ 3
      , str_sub(hand, 2, 2) == "4" ~ 4
      , str_sub(hand, 2, 2) == "5" ~ 5
      , str_sub(hand, 2, 2) == "6" ~ 6
      , str_sub(hand, 2, 2) == "7" ~ 7
      , str_sub(hand, 2, 2) == "8" ~ 8
      , str_sub(hand, 2, 2) == "9" ~ 9
      , str_sub(hand, 2, 2) == "T" ~ 10
      , str_sub(hand, 2, 2) == "J" ~ 1
      , str_sub(hand, 2, 2) == "Q" ~ 12
      , str_sub(hand, 2, 2) == "K" ~ 13
      , str_sub(hand, 2, 2) == "A" ~ 14
    )
    , card_3_value = case_when(
      str_sub(hand, 3, 3) == "2" ~ 2
      , str_sub(hand, 3, 3) == "3" ~ 3
      , str_sub(hand, 3, 3) == "4" ~ 4
      , str_sub(hand, 3, 3) == "5" ~ 5
      , str_sub(hand, 3, 3) == "6" ~ 6
      , str_sub(hand, 3, 3) == "7" ~ 7
      , str_sub(hand, 3, 3) == "8" ~ 8
      , str_sub(hand, 3, 3) == "9" ~ 9
      , str_sub(hand, 3, 3) == "T" ~ 10
      , str_sub(hand, 3, 3) == "J" ~ 1
      , str_sub(hand, 3, 3) == "Q" ~ 12
      , str_sub(hand, 3, 3) == "K" ~ 13
      , str_sub(hand, 3, 3) == "A" ~ 14
    )
    , card_4_value = case_when(
      str_sub(hand, 4, 4) == "2" ~ 2
      , str_sub(hand, 4, 4) == "3" ~ 3
      , str_sub(hand, 4, 4) == "4" ~ 4
      , str_sub(hand, 4, 4) == "5" ~ 5
      , str_sub(hand, 4, 4) == "6" ~ 6
      , str_sub(hand, 4, 4) == "7" ~ 7
      , str_sub(hand, 4, 4) == "8" ~ 8
      , str_sub(hand, 4, 4) == "9" ~ 9
      , str_sub(hand, 4, 4) == "T" ~ 10
      , str_sub(hand, 4, 4) == "J" ~ 1
      , str_sub(hand, 4, 4) == "Q" ~ 12
      , str_sub(hand, 4, 4) == "K" ~ 13
      , str_sub(hand, 4, 4) == "A" ~ 14
    )
    , card_5_value = case_when(
      str_sub(hand, 5, 5) == "2" ~ 2
      , str_sub(hand, 5, 5) == "3" ~ 3
      , str_sub(hand, 5, 5) == "4" ~ 4
      , str_sub(hand, 5, 5) == "5" ~ 5
      , str_sub(hand, 5, 5) == "6" ~ 6
      , str_sub(hand, 5, 5) == "7" ~ 7
      , str_sub(hand, 5, 5) == "8" ~ 8
      , str_sub(hand, 5, 5) == "9" ~ 9
      , str_sub(hand, 5, 5) == "T" ~ 10
      , str_sub(hand, 5, 5) == "J" ~ 1
      , str_sub(hand, 5, 5) == "Q" ~ 12
      , str_sub(hand, 5, 5) == "K" ~ 13
      , str_sub(hand, 5, 5) == "A" ~ 14
    )
  ) %>% 
  arrange(
    desc(priority)
    , card_1_value
    , card_2_value
    , card_3_value
    , card_4_value
    , card_5_value
  ) %>% 
  select(hand, bid, priority) %>%
  mutate(
    rank = row_number()
    , winnings = bid * rank
  ) %>%
  summarize(
    total_winnings = sum(winnings)
  ) %>% 
  pull(
    total_winnings
  )

message(glue("Part 1 answer: {total_winnings_1}"))
message(glue("Part 2 answer: {total_winnings_2}"))
