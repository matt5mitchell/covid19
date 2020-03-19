## Load data
library(readr)
library(dplyr)
library(tidyr)

# Daily reports from John Hopkins University
confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
recovered_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
deceased_url  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

# Population estimates 2019 (US Census Bureau)
states_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v1/states.csv"

# Read data
confirmed_raw <- read_csv(url(confirmed_url))
recovered_raw <- read_csv(url(recovered_url))
deceased_raw  <- read_csv(url(deceased_url))
states <- read_csv(url(states_url))

# Prepare data
confirmed <- confirmed_raw %>% 
  filter(`Country/Region` == "US",
         `Province/State` %in% states$State) %>%
  rename(State = `Province/State`) %>%
  select(-`Country/Region`, -Lat, -Long) %>%
  gather("Date", "Confirmed", -State)

recovered <- recovered_raw %>% 
  filter(`Country/Region` == "US",
         `Province/State` %in% states$State) %>%
  rename(State = `Province/State`) %>%
  select(-`Country/Region`, -Lat, -Long) %>%
  gather("Date", "Recovered", -State)

deceased <- deceased_raw %>% 
  filter(`Country/Region` == "US",
         `Province/State` %in% states$State) %>%
  rename(State = `Province/State`) %>%
  select(-`Country/Region`, -Lat, -Long) %>%
  gather("Date", "Confirmed", -State)