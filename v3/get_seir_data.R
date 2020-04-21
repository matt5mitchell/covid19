# Get data for SEIR model

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)

# Get data functions ----
get_case_data <- function() {
  
  # Daily reports from John Hopkins University
  cases_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v2/data/data_cases.csv" 
  
  # Read data
  read_csv(url(cases_url))
  
}

# Get state population data function ----
get_state_data <- function() {
  
  # Population estimates (US Census Bureau) - 2019 for states
  states_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v2/data/states.csv"
  
  # Read data
  states <- read_csv(url(states_url))
  
}

# Get data ----
covid <- get_case_data()
states <- get_state_data()