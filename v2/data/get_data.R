## Get data 
## COVID-19 data from Johns Hopkins Center for Systems Science and Engineering 

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

## Population data ----

# Population estimates 2019 (US Census Bureau)
states_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v1/states.csv"
states <- read_csv(url(states_url))


## Johns Hopkins data formatting changed

## Dataset 1 3/10/2020 - 3/21/2020 ----

# Daily reports
confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
recovered_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
deceased_url  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

# Read data
confirmed_raw <- read_csv(url(confirmed_url))
recovered_raw <- read_csv(url(recovered_url))
deceased_raw  <- read_csv(url(deceased_url))


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
  gather("Date", "Deaths", -State) %>%
  mutate(Date = mdy(Date))

# Combine data
data_cases_1 <- confirmed %>%
  left_join(recovered, by = c("Date", "State")) %>%
  left_join(deceased, by = c("Date", "State")) %>%
  filter(Date >= ymd(20200310), #First cases in data
         Date < ymd(20200322)) %>% #New format beginning 2020-3-22
  mutate(FIPS = NA,
         Place = NA) %>%
  select(Date, FIPS, Place, State,
         Confirmed, Recovered, Deaths)

## Dataset 2 3/23/2020 onward ----
date_start <- ymd(20200322)
date_end <- Sys.Date()

date_vector <- as_date(date_start:date_end)

# Get daily report function
get_daily_report <- function(date) {
  
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
data_daily_reports <- date_vector %>% map(get_daily_report) %>% bind_rows()

# Prepare data
data_cases_2 <- data_daily_reports %>%
  rename(Place = Admin2,
         State = Province_State) %>%
  select(Date, FIPS, Place, State,
         Confirmed, Recovered, Deaths)

## Final Dataset of Cases ----

# Row bind datasets
data_cases <- data_cases_1 %>% bind_rows(data_cases_2)