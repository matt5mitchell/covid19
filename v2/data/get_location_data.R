library(readr)
library(tidycensus)

# Get 2018 ACS population data
cities_raw <- get_estimates(geography = "place", 
                            product = "population",
                            year = 2018,
                            state = states$State)

counties_raw <- get_estimates(geography = "county", 
                          product = "population",
                          year = 2018,
                          state = states$State)

# Transform data
cities <- cities_raw %>%
  filter(variable == "POP") %>%
  select(-variable) %>%
  rename(Place = NAME,
         FIPS = GEOID,
         Population = value)

counties <- counties_raw %>%
  filter(variable == "POP") %>%
  select(-variable) %>%
  rename(County = NAME,
         FIPS = GEOID,
         Population = value)

# Save data
write_csv(cities, path="cities.csv")
write_csv(counties, path="counties.csv")