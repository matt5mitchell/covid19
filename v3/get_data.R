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

# Estimate recoveries ----
# Prem, et al. 2020 https://doi.org/10.1016/S2468-2667(20)30073-6
# Incubation 6.4 days + infectious 3-7 days -- about 11 on average
t_recovery <- 11 #Assumption also used in SIR model

# Requires Infected, Recovered, and Deaths columns
estimate_recovered <- function(data) {
  for (i in 2:nrow(data)) {
    data$Recovered[i] <- round(data$Infected[i-1] / t_recovery, 0)
    data$Infected[i] <- data$Confirmed[i] - data$Deaths[i] - data$Recovered[i]
  }
  return(data)
}

# Get data ----
covid <- get_case_data()
states <- get_state_data()