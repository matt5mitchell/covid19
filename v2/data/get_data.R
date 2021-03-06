## Get data 
## COVID-19 data from Johns Hopkins Center for Systems Science and Engineering 

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

setwd("~/R/covid19/v2/data")

## Population data ----

# Population estimates (US Census Bureau) - 2019 for states, 2018  for counties/cities
states_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v2/data/states.csv"
counties_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v2/data/counties.csv"
cities_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v2/data/cities.csv"

states <- read_csv(url(states_url))
counties <- read_csv(url(counties_url))
cities <- read_csv(url(cities_url))

## Johns Hopkins data formatting changed

## Dataset 1 3/10/2020 - 3/21/2020 ----
date_start1 <- ymd(20200310)
date_end1 <- ymd(20200321)

date_vector1 <- as_date(date_start1:date_end1)

# Get daily report function
get_daily_report1 <- function(date) {
  
  # Date as character
  date_char <- as.character(format(date, "%m-%d-%Y"))
  
  # URL for download
  url_daily <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_char,".csv")
  
  # Read data and filter to US
  read_csv(url(url_daily)) %>% 
    filter(`Country/Region` == "US") %>%
    mutate_at(vars("Last Update"), as.character) %>%
    mutate(Date = ymd(date))
  
}

# Read data
data_daily_reports1 <- date_vector1 %>% map(get_daily_report1) %>% bind_rows()

# Prepare data
data_cases_1 <- data_daily_reports1 %>%
  rename(State = `Province/State`) %>%
  inner_join(states, by="State") %>% #filter rows and add state population
  mutate(FIPS = NA,
         County = NA) %>%
  dplyr::select(Date, FIPS, State, County,
                Confirmed, Recovered, Deaths, Population)


## Dataset 2 3/23/2020 onward ----
date_start2 <- ymd(20200322)

if (hour(Sys.time()) < 18) { #If it's before 6pm, use yesterday's data
  date_end2 <- Sys.Date() - days(1)
} else {
  date_end2 <- Sys.Date()
}

date_vector2 <- as_date(date_start2:date_end2)

# Get daily report function
get_daily_report2 <- function(date) {
  
  # Date as character
  date_char <- as.character(format(date, "%m-%d-%Y"))
  
  # URL for download
  url_daily <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_char,".csv")
  
  # Read data and filter to US
  read_csv(url(url_daily)) %>% 
    filter(Country_Region == "US") %>%
    mutate_at(vars(FIPS, Last_Update), as.character) %>%
    mutate(Date = ymd(date))
  
}

# Read data
data_daily_reports2 <- date_vector2 %>% map(get_daily_report2) %>% bind_rows()

# Prepare data
data_cases_2 <- data_daily_reports2 %>%
  rename(County = Admin2,
         State = Province_State) %>%
  mutate(FIPS = ifelse(nchar(FIPS) == 4, paste0("0", FIPS), FIPS)) %>% #Add leading zero
  left_join(counties, by="FIPS") %>% #add county population
  inner_join(states, by="State") %>% #filter rows and add state population
  mutate(FIPS = ifelse(is.na(FIPS.x), FIPS.y, FIPS.x), #if county is blank...
         Population = ifelse(is.na(FIPS.x), Population.y, Population.x)) %>% 
  rename(County = County.y) %>% #use census bureau names
  dplyr::select(Date, FIPS, State, County,
                Confirmed, Recovered, Deaths, Population)

## Final Dataset of Cases ----

# Row bind datasets
data_cases <- data_cases_1 %>% 
  bind_rows(data_cases_2)

# Save data
write_csv(data_cases, path = "data_cases.csv")
