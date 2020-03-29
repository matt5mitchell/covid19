## COVID-19 Forecast
## Shiny app server script

library(shiny)
library(shinyWidgets)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(R0)
library(imputeTS)
library(highcharter)
library(purrr)
library(deSolve)

#### Define functions ####

# Get COVID-19 data function ----
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

# Get county population data function ----
get_county_data <- function() {
  
  # Population estimates (US Census Bureau) - 2018  for counties
  counties_url <- "https://raw.githubusercontent.com/matt5mitchell/covid19/master/v2/data/counties.csv"
  
  # Read data
  counties <- read_csv(url(counties_url))
  
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

#### Shiny App ####

# Shiny server function ----
function(input, output, session) {
  
  # Colors for plots ----
  colors <- c("#4b4b4b", "#F26F32", "#2585C7", "#96D05C")

  # Load COVID-19 data ----
  covid <- reactive({
    # Prevent data from reloading
    invalidateLater(86400000, session) #approximately 24 hours

    # Get COVID-19 data (defined above)
    get_case_data()
    
  })

  # Load state population data ----
  states <- reactive({
    # Prevent data from reloading
    invalidateLater(86400000, session) #approximately 24 hours
    
    # Get state data (defined above)
    get_state_data()
    
  })
  
  # Load county population data ----
  counties <- reactive({
    # Prevent data from reloading
    invalidateLater(86400000, session) #approximately 24 hours
    
    # Get county data (defined above)
    get_county_data()
    
  })
  
  # Unique states & counties ----
  states_counties <- reactive({
    
    unique(covid() %>% 
             dplyr::select(State, County) %>% 
             na.omit() %>%
             arrange(State, County))
    
  })
  
  # UI State Picker Output ----
  output$output_states <- renderUI({
    pickerInput(inputId = "input_states",
                label = "States to forecast:",
                choices = list( "United States" = unique(states_counties()$State)),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  noneSelectedText = "United States"
                )
    )
  })
  
  # States selected ----
  states_selected <- reactive({
    
    if(is.null(input$input_states)) {
      unique(states_counties()$State)
      } else { as.vector(input$input_states) }
    
  })

  # UI County Picker Output ----
  output$output_counties <- renderUI({
    pickerInput(inputId = "input_counties",
                label = "Counties to forecast:",
                choices = states_counties()$County[states_counties()$State %in% states_selected()],
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  noneSelectedText = "All Counties"
                )
    )
  })
  
  # Counties selected ----
  counties_selected <- reactive({

    if(is.null(input$input_counties)) {
      unique(covid()$County)
      } else { as.vector(input$input_counties) }

  })
  
  # Summarized data ----
  covid_sum <- reactive({
    
    # Filtered and summarized dataset
    covid_sum <- covid() %>%
      dplyr::filter(State %in% states_selected(),
                    County %in% counties_selected()) %>%
      dplyr::select(Date, Confirmed, Recovered, Deaths) %>%
      group_by(Date) %>%
      summarize(Confirmed = sum(Confirmed),
                Recovered = 0, #data not reliable - estimate later
                Deaths = sum(Deaths),
                Active = Confirmed) #seed with confirmed - estimate later
    
    # Add population data
    if(is.null(input$input_counties)) {
      covid_sum %>% mutate(Population = sum(states()$Population[states()$State %in% states_selected()]))
    } else {
      covid_sum %>% mutate(Population = sum(counties()$Population[counties()$County %in% counties_selected()]))
    }
    
  })
  
  # Confirmed cases only ----
  covid_sum_confirmed <- reactive({
    
    estimate_recovered(covid_sum())  %>%
      mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
             Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
             Infected = Confirmed - Recovered - Deaths, 
             Removed = Recovered + Deaths,
             Susceptible = Population - Infected - Removed) %>% #For SIR modeling
      slice(min(which(.$Incidence > 0)):n())
    
  })
  
  # Estimate Rt ----
  Rt_df <- reactive({
    
    # Generation time
    # Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
    gt_lognormal <- generation.time("lognormal", c(4.7, 2.9))
    
    # Number of days in dataset
    n_days <- nrow(covid_sum_confirmed())
    
    # Estimate Rt
    Rt_est <- est.R0.TD(epid = covid_sum_confirmed()$Incidence, 
                        GT = gt_lognormal, 
                        n.t0 = covid_sum_confirmed()$Incidence[1],
                        t = covid_sum_confirmed()$Date, 
                        begin = 1L, 
                        end = n_days, 
                        time.step = 1L, 
                        nsim = 1000)
    
    data.frame(Rt = Rt_est$R,
               Lower= Rt_est$conf.int$lower,
               Upper= Rt_est$conf.int$upper) %>%
      dplyr::na_if(0) %>%
      imputeTS::na_interpolation(option = "spline")
    
  })

  # Average Rt over last 7 days ----
  Rt_7days <- reactive({
    
    # Average over last 7 days
    Rt_df() %>%
      slice((n() - 6):n()) %>%
      summarize_all(mean)
    
  })
  
  # Rt plot ----
  output$output_Rt_plot <- renderHighchart({
    
    plot_data <- covid_sum_confirmed() %>%
      bind_cols(Rt_df()) %>%
      mutate_all(function(x) {round(x, 2)}) %>%
      mutate(Target = 1) %>%
      slice(1:(n() - 1)) #remove last day--sometimes returns 0
    
    highchart() %>%
      hc_title(text = "Effective Reproduction Number: Lower is Better",
               align = "left") %>%
      hc_xAxis(type = "datetime") %>%
      hc_yAxis(min = 0,
               title = list(text = "Effective Reproduction Number")) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Target),
                    id = "Target",
                    name = "Target Line",
                    type = "line",
                    marker = list(enabled = FALSE),
                    color = colors[1],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    dashStyle = "dash",
                    animation = FALSE,
                    enableMouseTracking = FALSE
                    ) %>%
      hc_add_series(plot_data, 
                    hcaes(x = Date, low = Lower, high = Upper),     
                    id = "Range", 
                    name = "Confidence Interval",
                    type = "arearange",
                    marker = list(enabled = FALSE),
                    color = colors[3],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Rt),
                    id = "Rt", 
                    name = "Effective Reproduction Number", 
                    type = "line",
                    marker = list(enabled = FALSE),
                    color = colors[1],
                    showInLegend = FALSE,
                    animation = FALSE)
    
  })

  # 86% (CI: 82% - 90%) of cases go undetected
  # Li, et al., 2020 https://doi.org/10.1126/science.abb3221
  # For the forecasts, round to 80% and 90%
  
  covid_sum_est <- reactive ({
    
    # Approximate 80% undetected cases ----
    covid_sum_80pct <- covid_sum()  %>%
      mutate(Confirmed = Confirmed * 4,
             Active = Confirmed) #seed with confirmed
    
    covid_sum_80pct <- estimate_recovered(covid_sum_80pct) %>%
      mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
             Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
             Infected = Confirmed - Recovered - Deaths, 
             Removed = Recovered + Deaths,
             Susceptible = Population - Infected - Removed) %>% #For SIR modeling
      slice(min(which(.$Incidence > 0)):n())

    # Approximate 90% undetected cases ----
    covid_sum_90pct <- covid_sum()  %>%
      mutate(Confirmed = Confirmed * 9,
             Active = Confirmed) #seed with confirmed
    
    covid_sum_90pct <- estimate_recovered(covid_sum_90pct) %>%
      mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
             Incidence = ifelse(Incidence < 0, 0, Incidence), #Prevent negatives from bad data
             Infected = Confirmed - Recovered - Deaths, 
             Removed = Recovered + Deaths,
             Susceptible = Population - Infected - Removed) %>% #For SIR modeling
      slice(min(which(.$Incidence > 0)):n())
    
    list(covid_sum_80pct, covid_sum_90pct)
    
  })
  
  
  # SIR Model ----
  sir_proj <- reactive({
    
    # SIR Model Function
    sir_model <- function(data) {
      #Filter to last date for SIR model input
      sir_input <- data %>%
        filter(Date == max(Date))
      
      # Static inputs for model
      t <- 365 #max days to project
      gamma <- 1 / t_recovery #recovery time defined above
      P <- sir_input$Population
      S <- sir_input$Susceptible / P
      I <- sir_input$Infected / P
      R <- sir_input$Removed / P
      init <- c(S=S, I=I, R=R)
      times <- seq(0, t, by = 1)
      
      # Additional inputs
      Rt  <- Rt_7days()$Rt #effective reproduction number
      beta  <- gamma * Rt
      parameters <- c(bet=beta, gamm=gamma)
      
      # Define model
      model <- function(time, state, parameters) {
        with(as.list(c(state, parameters)), {
          dS <- -bet * S * I
          dI <-  bet * S * I - gamm * I
          dR <-                gamm * I
          return(list(c(dS, dI, dR)))
        })
      }
      
      #Solve using ode
      model_output <- ode(y=init, times=times, model, parms=parameters)
      
      #Projection from SIR model
      data.frame(Date = sir_input$Date + days(0:t),
                 Infected = round(as.data.frame(model_output)$I * P, 0))
    }
    
    # Run model using infection estimates
    sir_model_outputs <- covid_sum_est() %>% map(sir_model)
    
    # Combine infection estimates with model projections
    sir_data <- list() 
    for (i in 1:2) {
      sir_data[[i]] <- covid_sum_est()[[i]] %>%
        dplyr::select(Date, Infected) %>%
        bind_rows(sir_model_outputs[[i]]) %>%
        unique() %>% #Remove duplicate date
        arrange(Date)
    }
    
    # Convert to data frame
    bind_cols(sir_data) %>%
      rename(proj_80 = Infected,
             proj_90 = Infected1) %>%
      dplyr::select(Date, proj_80, proj_90)
    
  })
  
  # Forecasted peak ----
  output$output_peak <- renderText({
    
    # Vectors of maxima and dates of maxima
    y_max <- c(max(sir_proj()$proj_80), max(sir_proj()$proj_90))
    y_max_date <- c(min(sir_proj()$Date[sir_proj()$proj_80 == y_max[1]]), min(sir_proj()$Date[sir_proj()$proj_90 == y_max[2]]))
    
    # Text out put
    # If Month Year are the same ... else ...
    if (identical(format(y_max_date[1], "%B %Y"), format(y_max_date[2], "%B %Y"))) {
      paste0("At the current rate of infection, the outbreak is forecasted to peak in ", 
             format(y_max_date[1], "%B %Y"), 
             " with about ", 
             prettyNum(round(max(y_max), -3), big.mark = ","), 
             " active infections.")
    } else {
      paste0("At the current rate of infection, the outbreak is forecasted to peak between ", 
             format(min(y_max_date), "%B %Y"), 
             " and ",
             format(max(y_max_date), "%B %Y"),
             " with about ", 
             prettyNum(round(max(y_max), -3), big.mark = ","), 
             " active infections.")
    }
    
    })

  # SIR Plot ----
  output$output_SIR_plot <- renderHighchart({
    
    # Vector of all dates
    dates <- sir_proj()$Date
    
    # Days of forecast to plot
    forecast_days <- input$input_days
    
    # Combine actuals and SIR output
    plot_data <- data.frame(Date=dates) %>%
      left_join(covid_sum_confirmed(), by="Date") %>%
      left_join(sir_proj(), by="Date") %>%
      slice(1:(nrow(covid_sum_confirmed()) + forecast_days))
    
    # SIR plot
    highchart() %>%
      hc_title(text = "Forecast of Active Infections",
               align = "left") %>%
      hc_xAxis(type = "datetime") %>%
      hc_yAxis(min = 0,
               title = list(text = "Active Infections")) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = proj_90),
                    name = "90% Undetected",
                    type = "area",
                    marker = list(enabled = FALSE),
                    color = colors[2],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = proj_80),
                    name = "80% Undetected",
                    type = "area",
                    marker = list(enabled = FALSE),
                    color = colors[3],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Infected),
                    name = "Confirmed Active Infections",
                    type = "column",
                    groupPadding = 0,
                    color = colors[1],
                    showInLegend = FALSE,
                    animation = FALSE)
    
  })
  
}