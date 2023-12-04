

# Prep Workspace ---------------------------------------------------------------

# load packages
library(readr)
library(stringr)
library(dplyr)
library(glue)

# read file
data <- read_delim(
  "day-3/input.txt"
  , delim = "\n"
  , col_names = c("input_strs")
  , col_types = c("c")
)

# Part 1 -----------------------------------------------------------------------

# add padding to data frame so as to not worry about edge cases
data_altered <- data %>% 
  mutate(
    input_strs = glue(" {input_strs} ")
    , input_strs = str_replace_all(input_strs, "\\.", " ")
    , n_chars = nchar(input_strs)
  )

# add row of empty spaces at top and bottom of input so no need to worry about
# edge cases
data_padded <- data.frame(
  input_strs = glue_collapse(glue("{rep(' ', unique(data_altered$n_chars))}"))
  , n_chars = unique(data_altered$n_chars)
) %>% 
  bind_rows(
    data_altered
  ) %>% 
  bind_rows(
    data.frame(
      input_strs = glue_collapse(glue("{rep(' ', unique(data_altered$n_chars))}"))
      , n_chars = unique(data_altered$n_chars)
    )
  )

# break apart into individual columns
data_prepped <- data_padded
for (i in 1:data_prepped$n_chars[1]) {
  col_name <- glue("col_{i}")
  data_prepped <- data_prepped %>% 
    mutate(
      new_col = str_sub(input_strs, start = i, end = i)
    )
  data_prepped[[col_name]] <- data_prepped$new_col
  data_prepped <- data_prepped %>% 
    select(-new_col)
}

data_to_check <- data_prepped %>% 
  select(
    -c(input_strs, n_chars)
  )

check_adjacent <- function(row_num, col_num, data = data_to_check) {
  checks <- list(
      c( 0,  1)
    , c( 0, -1)
    , c( 1,  1)
    , c( 1,  0)
    , c( 1, -1)
    , c(-1,  1)
    , c(-1,  0)
    , c(-1, -1)
  )
  
  for (check_no in 1:length(checks)) {
    row_to_check <- row_num + checks[[check_no]][1]
    col_to_check <- col_num + checks[[check_no]][2]
    to_check <- data[row_to_check, col_to_check]
    if (!(to_check %in% as.character(0:9))) {
      if (to_check != " ") {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

values <- c()
for (i in 2:(nrow(data_to_check) - 1)) {
  is_num <- FALSE
  is_adjacent <- FALSE
  num_str <- ""
  row_values <- c()
  
  for (j in 2:(ncol(data_to_check))) {
    d <- data_to_check[i, j]
    
    if (d %in% c(as.character(0:9))) {
      is_num <- TRUE
      num_str <- glue("{num_str}{d}")
    } else if (is_num) {
      is_num <- FALSE
      if (is_adjacent) {
        values <- c(values, num_str)
        row_values <- c(row_values, num_str)
        is_adjacent <- FALSE
      }
      num_str <- ""
    } else {
      num_str <- ""
    }
    if (d %in% c(as.character(0:9))) {
      if (check_adjacent(i, j, data_to_check)) {
        is_adjacent <- TRUE
      }
    }
  }
}

values <- as.numeric(values)

message(glue("Part 1 answer: {sum(values)}"))

# Part 2 -----------------------------------------------------------------------

check_adjacent_gears <- function(row_num, col_num, data = data_to_check) {
  checks <- list(
    c( 0,  1)
    , c( 0, -1)
    , c( 1,  1)
    , c( 1,  0)
    , c( 1, -1)
    , c(-1,  1)
    , c(-1,  0)
    , c(-1, -1)
  )
  
  for (check_no in 1:length(checks)) {
    row_to_check <- row_num + checks[[check_no]][1]
    col_to_check <- col_num + checks[[check_no]][2]
    to_check <- data[row_to_check, col_to_check]
    if (!(to_check %in% as.character(0:9))) {
      if (to_check == "*") {
        return(c(TRUE, row_to_check, col_to_check))
      }
    }
  }
  
  return(c(FALSE, 0, 0))
}

values <- c()
gear_list <- list()
total_gear_ratio <- 0
for (i in 2:(nrow(data_to_check) - 1)) {
  is_num <- FALSE
  is_adjacent <- FALSE
  num_str <- ""
  adjacent_gear <- c(1, 1)
  row_values <- c()
  
  for (j in 2:(ncol(data_to_check))) {
    d <- data_to_check[i, j]
    
    if (d %in% c(as.character(0:9))) {
      is_num <- TRUE
      num_str = glue("{num_str}{d}")
    } else if (is_num) {
      is_num <- FALSE
      if (is_adjacent) {
        values <- c(values, num_str)
        row_values <- c(row_values, num_str)
        is_adjacent <- FALSE
        gear_name <- glue("{adjacent_gear[1]}-{adjacent_gear[2]}")
        if (gear_name %in% names(gear_list)) {
          gear_list[[gear_name]] <- c(gear_list[[gear_name]], num_str)
        } else {
          gear_list[[gear_name]] <- num_str
        }
        if (length(gear_list[[gear_name]]) > 1) {
          gear_ratio <- as.numeric(gear_list[[gear_name]][1]) *
            as.numeric(gear_list[[gear_name]][2])
          total_gear_ratio <- total_gear_ratio + gear_ratio
        }
      }
      num_str <- ""
    } else {
      num_str <- ""
    }
    if (is_num) {
      adjacent_gear_vect <- check_adjacent_gears(i, j, data_to_check)
      if (adjacent_gear_vect[1]) {
        is_adjacent <- TRUE
        adjacent_gear <- c(adjacent_gear_vect[2], adjacent_gear_vect[3])
      }
    }
  }
}

message(glue("Part 2 answer: {total_gear_ratio}"))
