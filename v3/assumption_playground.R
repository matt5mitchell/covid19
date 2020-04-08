## COVID-19 Forecast
## Playground for validating assumptions

library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(R0)
library(imputeTS)
library(purrr)
library(deSolve)

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

# Requires Active, Recovered, and Deaths columns
estimate_recovered <- function(data) {
  for (i in 2:nrow(data)) {
    data$Recovered[i] <- round(data$Active[i-1] / t_recovery, 0)
    data$Active[i] <- data$Confirmed[i] - data$Deaths[i] - data$Recovered[i]
  }
  return(data)
}

# Get data ----
covid <- get_case_data()
states <- get_state_data()

# Select states ----
states_selected <- states %>% 
  # filter(State == "Oregon") %>%
  # filter(State == "New York") %>%
  # filter(State %in% c("Oregon", "Washington")) %>%
  dplyr::select(State) %>%
  deframe() %>%
  unique()

# Summarize data ----
# Filtered and summarized dataset
covid_sum <- covid %>%
  dplyr::filter(State %in%  states_selected) %>%
  dplyr::select(Date, Confirmed, Recovered, Deaths) %>%
  group_by(Date) %>%
  summarize(Confirmed = sum(Confirmed),
            Recovered = 0, #data not reliable - estimate later
            Deaths = sum(Deaths),
            Active = Confirmed) %>% #seed with confirmed - estimate later
  mutate(Population = sum(states$Population[states$State %in% states_selected]))

covid_sum_confirmed <- estimate_recovered(covid_sum)  %>%
  mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
         Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
         Infected = Confirmed - Recovered - Deaths, 
         Removed = Recovered + Deaths,
         Susceptible = Population - Infected - Removed) %>% #For SIR modeling
  slice(min(which(.$Incidence > 0)):n())

# Approximate undetected cases ----
# Approximate 80% undetected cases
covid_sum_80pct <- covid_sum  %>%
  mutate(Confirmed = Confirmed * 4,
         Active = Confirmed) #seed with confirmed

covid_sum_80pct <- estimate_recovered(covid_sum_80pct) %>%
  mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
         Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
         Infected = Confirmed - Recovered - Deaths, 
         Removed = Recovered + Deaths,
         Susceptible = Population - Infected - Removed) %>% #For SIR modeling
  slice(min(which(.$Incidence > 0)):n())

# Approximate 90% undetected cases
covid_sum_90pct <- covid_sum  %>%
  mutate(Confirmed = Confirmed * 9,
         Active = Confirmed) #seed with confirmed

covid_sum_90pct <- estimate_recovered(covid_sum_90pct) %>%
  mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
         Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
         Infected = Confirmed - Recovered - Deaths, 
         Removed = Recovered + Deaths,
         Susceptible = Population - Infected - Removed) %>% #For SIR modeling
  slice(min(which(.$Incidence > 0)):n())

# List of data frames for SIR model
covid_sum_est <- list(covid_sum_confirmed, covid_sum_80pct, covid_sum_90pct)

# Reproduction number ----
# Generation time
# Mean = 4.7 and SD = 2.9 from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
gt_lognormal <- generation.time("lognormal", c(4.7, 2.9)) 
                                               

# Number of days in dataset
n_days <- nrow(covid_sum_confirmed)

# Estimate Rt
Rt_est <- est.R0.TD(epid = covid_sum_confirmed$Incidence, 
                    GT = gt_lognormal, 
                    n.t0 = covid_sum_confirmed$Incidence[1],
                    t = covid_sum_confirmed$Date, 
                    begin = 1L, 
                    end = n_days, 
                    time.step = 1L, 
                    nsim = 1000)

# Data frame of Rt estimate and confidence interval
Rt_df <- data.frame(Rt = Rt_est$R,
           Lower= Rt_est$conf.int$lower,
           Upper= Rt_est$conf.int$upper) %>%
  dplyr::na_if(0) %>%
  imputeTS::na_interpolation(option = "spline")

# SIR Model ----
# SIR Model Function
sir_model <- function(data) {
  # Filter to first date for SIR model input
  # Run model forward in time to attempt to match observations
  sir_input <- data %>%
    filter(Date == min(Date))

  # Create inputs
  t <- 365 #max days to project
  gamma <- 1 / t_recovery #recovery time defined above
  P <- sir_input$Population
  S <- sir_input$Susceptible / P
  I <- sir_input$Infected / P
  R <- sir_input$Removed / P
  Rt <- Rt_df$Rt #effective reproduction number
  beta <- gamma * Rt #remember this is a vector
  
  # Final inputs for model
  init <- c(S=S, I=I, R=R) #initial state
  times <- seq(0, t, by = 1) #time points to integrate
  #parameters <- list(beta=beta, gamm=gamma)
  parameters <- list(gamm=gamma)
  
  beta_func <- approxfun(beta, rule = 2) #function to approximate beta in SIR model
  
  # Define model
  model <- function(t, y, parms) {
    with(as.list(c(t, y, parms)), {
      
      beta_t <- beta_func(t) #time varying beta
      
      dS <- -beta_t * S * I
      dI <-  beta_t * S * I - gamm * I
      dR <-                   gamm * I
      
      return(list(c(dS, dI, dR)))
    })
  }
  
  # Solve using ode
  model_output <- ode(y=init, times=times, model, parms=parameters)

  # Projection from SIR model
  sir_proj <- data.frame(Date = sir_input$Date + days(0:t),
                         Infected = round(as.data.frame(model_output)$I * P, 0))

}

# Run model using infection estimates
sir_model_outputs <- covid_sum_est %>% map(sir_model)

# Plot comparisons ----
plot_data <- bind_cols(sir_model_outputs) %>% 
  left_join(covid_sum_confirmed %>% dplyr::select(Date, Infected), by = "Date") %>%
  dplyr::select(Date, Infected.x, Infected1, Infected2, Infected.y) %>%
  rename(Actual = Infected.y,
         Proj_Base = Infected.x,
         Proj_80pct = Infected1,
         Proj_90pct = Infected2) %>%
  slice(1:29)  %>%
  gather("Series", "Infected", -Date)

d_ends <- plot_data %>%
  filter(Date == max(Date)) %>%
  group_by(Series) %>% 
  top_n(1, Infected) %>% 
  pull(Infected)

plot_data %>%
  # filter(Series %in% c("Actual", "Proj_Base")) %>%
  ggplot(aes(x = Date, y = Infected, color = Series)) +
  geom_line() +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends, labels = comma)) +
  scale_x_date(expand = c(0, 0)) +
  theme_minimal()
