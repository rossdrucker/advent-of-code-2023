
# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)

# read file
data <- read_delim(
  "day-1/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
)

# Part 1 -----------------------------------------------------------------------

# challenge: extract the first and last digits from each line of the input file,
#            combine them into a two-digit number, and get total of all
#            two-digit numbers. If only one digit exists in line, digit should
#            be 

data %>% 
  mutate(
    nums = str_remove_all(input_strs, "[a-zA-Z]")
    , first = str_sub(nums, 1, 1)
    , last = str_sub(nums, nchar(nums), nchar(nums))
    , two_digit = glue("{first}{last}")
    , final_num = as.numeric(two_digit)
  ) %>% 
  summarize(
    total = sum(final_num)
  )

# Part 2 -----------------------------------------------------------------------

# challenge: spelled-out numbers should count as digits (e.g. "one" should be 1)

data %>% 
  mutate(
      input_strs = tolower(input_strs)
    , input_strs = str_replace_all(input_strs, "one", "one1one")
    , input_strs = str_replace_all(input_strs, "two", "two2two")
    , input_strs = str_replace_all(input_strs, "three", "three3three")
    , input_strs = str_replace_all(input_strs, "four", "four4four")
    , input_strs = str_replace_all(input_strs, "five", "five5five")
    , input_strs = str_replace_all(input_strs, "six", "six6six")
    , input_strs = str_replace_all(input_strs, "seven", "seven7seven")
    , input_strs = str_replace_all(input_strs, "eight", "eight8eight")
    , input_strs = str_replace_all(input_strs, "nine", "nine9nine")
    , nums = str_remove_all(input_strs, "[a-zA-Z]")
    , first = str_sub(nums, 1, 1)
    , last = str_sub(nums, nchar(nums), nchar(nums))
    , two_digit = glue("{first}{last}")
    , final_num = as.numeric(two_digit)
  ) %>%
  summarize(
    total = sum(final_num)
  )
