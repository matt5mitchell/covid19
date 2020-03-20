## Load data ##

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

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
  dplyr::select(-`Country/Region`, -Lat, -Long) %>%
  gather("Date", "Confirmed", -State) %>%
  mutate(Date = mdy(Date))

recovered <- recovered_raw %>% 
  filter(`Country/Region` == "US",
         `Province/State` %in% states$State) %>%
  rename(State = `Province/State`) %>%
  dplyr::select(-`Country/Region`, -Lat, -Long) %>%
  gather("Date", "Recovered", -State) %>%
  mutate(Date = mdy(Date))

deceased <- deceased_raw %>% 
  filter(`Country/Region` == "US",
         `Province/State` %in% states$State) %>%
  rename(State = `Province/State`) %>%
  dplyr::select(-`Country/Region`, -Lat, -Long) %>%
  gather("Date", "Deceased", -State) %>%
  mutate(Date = mdy(Date))

# Final dataset
covid <- confirmed %>%
  left_join(recovered, by = c("Date", "State")) %>%
  left_join(deceased, by = c("Date", "State")) %>%
  arrange(State, Date) %>%
  group_by(State) %>%
  mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
         Incidence = ifelse(Incidence < 0, 0, Incidence)) #Prevent negatives from bad data
  
covid_sir <- covid %>%
  group_by(State) %>%
  left_join(states[, c("State", "Population")], by = "State") %>%
  mutate(Infected = Confirmed - Recovered - Deceased, 
         Removed = Recovered + Deceased,
         Susceptible = Population - Infected - Removed) %>% #For SIR modeling
  dplyr::select(State, Date, Susceptible, Infected, Removed)
