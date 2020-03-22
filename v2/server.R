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
  
  # Theme for plots
  colors <- c("#4b4b4b", "#F26F32", "#2585C7", "#96D05C")
  theme_set(theme_minimal() + 
              theme(panel.background = element_blank(),
                    axis.title = element_text(color=colors[1], size= 12),
                    axis.text = element_text(color=colors[1], size = 10),
                    panel.grid.major = element_line(size=.5),
                    panel.grid.minor = element_line(size=.5),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank()
              ))
  
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
  
  # Estimate Rt ----
  Rt_df <- reactive({
    
    # Generation time
    # Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
    gt_lognormal <- generation.time("lognormal", c(4.7, 2.9))
    
    # Number of days in dataset
    n_days <- nrow(covid_sum())
    
    # Estimate Rt
    Rt_est <- est.R0.TD(epid = covid_sum()$Incidence, 
                        GT = gt_lognormal, 
                        n.t0 = covid_sum()$Incidence[1],
                        t = covid_sum()$Date, 
                        begin = 1L, 
                        end = n_days, 
                        time.step = 1L, 
                        nsim = 1000)
    
    data.frame(Rt = Rt_est$R,
               Lower= Rt_est$conf.int$lower,
               Upper= Rt_est$conf.int$upper)
    
  })

  # Average Rt over last 7 days ----
  Rt_7days <- reactive({
    
    # Average over last 7 days
    Rt_df() %>%
      slice((n() - 6):n()) %>%
      summarize_all(mean)
    
  })
  
  # Rt plot ----
  output$output_Rt_plot <- renderPlot({
    
    # Plot effective reproduction number
    covid_sum() %>%
      bind_cols(Rt_df()) %>%
      slice(1:(n() - 1)) %>% #remove last day--sometimes returns 0
      ggplot(aes(x = Date)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .2, fill = colors[3]) +
      geom_line(aes(y=Rt)) +
      geom_hline(yintercept = 1, linetype ="dashed") +
      scale_y_continuous(limits = c(0, NA)) +
      ylab("Effective Reproduction Number")
    
  })
  
  # SIR Plot ----
  output$output_SIR_plot <- renderPlot({
    
    ## SIR model ----
    
    #Filter to last date for SIR model input
    covid_sir_input <- covid_sum() %>%
      filter(Date == max(Date))
    
    # Static inputs
    t <- 365 #max days to project
    T_r <- 14 #recovery time (assumed to be 14 days)
    gamma <- 1 / T_r
    P <- covid_sir_input$Population
    S <- covid_sir_input$Susceptible / P
    I <- covid_sir_input$Infected / P
    R <- covid_sir_input$Removed / P
    init <- c(S=S, I=I, R=R)
    times <- seq(0, t, by = 1)
    
    # Loop for mean, lower, and upper Rt estimates
    sir <- list()
    for (i in 1:3) {
      # Additional inputs
      Rt  <- as.data.frame(Rt_7days())[,i] #effective reproduction number
      beta  <- gamma * Rt
      parameters <- c(bet=beta, gamm=gamma)
      
      # Define model
      sir_model <- function(time, state, parameters) {
        with(as.list(c(state, parameters)), {
          dS <- -bet * S * I
          dI <-  bet * S * I - gamm * I
          dR <-                gamm * I
          return(list(c(dS, dI, dR)))
        })
      }
      
      #Solve using ode
      sir[[i]] <- ode(y=init, times=times, sir_model, parms=parameters)
    }
    
    #Projection from SIR model
    sir_proj <- data.frame(Date = max(covid_sum()$Date) + days(0:t),
                           Mean = round(as.data.frame(sir[[1]])$I * P, 0),
                           Lower = round(as.data.frame(sir[[2]])$I * P, 0),
                           Upper = round(as.data.frame(sir[[3]])$I * P, 0))
    
    # Vector of all dates
    dates <- as_date(min(covid_sum()$Date):(max(covid_sum()$Date) + days(t)))
    
    # Combine actuals and SIR output
    plot_data <- data.frame(Date=dates) %>%
      left_join(covid_sum(), by="Date") %>%
      left_join(sir_proj, by="Date") %>%
      slice(1:(nrow(covid_sum()) + 14)) %>%
      rename(Cases = Infected, #from covid_sum
             Projection = Mean) #from sir_proj
    
    # SIR plot
    y_max <- max(plot_data$Projection, na.rm = TRUE)
    y_max_x <- min(plot_data$Date[plot_data$Projection == y_max], na.rm = TRUE)
    
    plot_data %>%
      ggplot(aes(x = Date)) +
      geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .2, fill = colors[3]) +
      geom_line(aes(y = Projection)) +
      geom_bar(aes(y = Cases), stat = "identity", fill = colors[3]) + 
      scale_y_continuous(limits = c(0, NA), labels = comma) +
      xlab("Date") +
      ylab("Active Cases")
    
  })
  
}