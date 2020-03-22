## COVID-19 Forecast
## Shiny app server script

library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(R0)
library(ggplot2)
library(scales)
library(deSolve)

# Get data function ----
get_data <- function() {

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
  confirmed %>%
    left_join(recovered, by = c("Date", "State")) %>%
    left_join(deceased, by = c("Date", "State")) %>%
    left_join(states[, c("State", "Population")], by = "State") %>%
    arrange(State, Date) %>%
    group_by(State) %>%
    mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
           Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
           Infected = Confirmed - Recovered - Deceased, 
           Removed = Recovered + Deceased,
           Susceptible = Population - Infected - Removed) #For SIR modeling

}

# Shiny server function ----
function(input, output, session) {
  
  # Load data ----
  covid <- reactive({
    # Prevent data from reloading
    invalidateLater(86400000, session) #approximately 24 hours
    
    # Get data (defined above)
    get_data()
    
  })
  
  # Final data ----
  covid_sum <- reactive({
    
    #First detection
    first_detection <- min(which(covid()$Incidence > 0))
    
    #States selected
    states_selected <- if(is.null(input$input_states)) {
      covid()$State
    } else { as.vector(input$input_states) }
    
    #Filtered and summarized dataset
    covid() %>%
      dplyr::filter(State %in% states_selected) %>%
      group_by(Date) %>%
      summarize_at(c("Incidence", "Population", "Susceptible", "Infected", "Removed"), sum) %>%
      slice(first_detection:n()) #First detection onward
    
  })

  # Rt plot ----
  output$output_Rt_plot <- renderPlot({
    
    # Generation time
    # Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
    gt_lognormal <- generation.time("lognormal", c(4.7, 2.9))
    
    # Estimate Rt
    n_days <- nrow(covid_sum())

    Rt_est <- est.R0.TD(epid = covid_sum()$Incidence, 
                        GT = gt_lognormal, 
                        n.t0 = covid_sum()$Incidence[1],
                        t = covid_sum()$Date, 
                        begin = 1L, 
                        end = n_days, 
                        time.step = 1L, 
                        nsim = 1000)
    
    Rt_est_df <- data.frame(Rt = Rt_est$R,
                            Lower= Rt_est$conf.int$lower,
                            Upper= Rt_est$conf.int$upper)
    
    # Average over last 7 days
    Rt_7days <- Rt_est_df %>%
      slice((n() - 6):n()) %>%
      summarize_all(mean)
    
    # Plot effective reproduction number
    covid_sum() %>%
      bind_cols(Rt_est_df) %>%
      slice(1:(n_days - 1)) %>% #remove last day--sometimes returns 0
      ggplot(aes(x = Date)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .2, fill = colors[3]) +
      geom_line(aes(y=Rt)) +
      geom_hline(yintercept = 1, linetype ="dashed") +
      scale_y_continuous(limits = c(0, NA)) +
      ylab("Effective Reproduction Number")
    
  })
}