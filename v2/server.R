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
library(deSolve)

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
# Requires Active, Recovered, and Deaths columns
t_recovery <- 10 #Assumption also used in SIR model
estimate_recovered <- function(data) {
  for (i in 2:nrow(data)) {
    data$Recovered[i] <- round(data$Active[i-1] / t_recovery, 0)
    data$Active[i] <- data$Confirmed[i] - data$Deaths[i] - data$Recovered[i]
  }
  return(data)
}

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

  observe({print(min(covid_sum()$Date))})
  
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
    
    #Filter to last date for SIR model input
    covid_sir_input <- covid_sum() %>%
      filter(Date == max(Date))
    
    # Static inputs for model
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
    data.frame(Date = max(covid_sum()$Date) + days(0:t),
               Mean = round(as.data.frame(sir[[1]])$I * P, 0),
               Lower = round(as.data.frame(sir[[2]])$I * P, 0),
               Upper = round(as.data.frame(sir[[3]])$I * P, 0))
    
  })
  
  # Forecasted peak ----
  output$output_peak <- renderText({
    
    y_max <- max(sir_proj()$Mean)
    y_max_date <- min(sir_proj()$Date[sir_proj()$Mean == y_max])
    paste0("At the current rate of infection, the outbreak is forecasted to peak in ", format(y_max_date, "%B %Y"), " with ", prettyNum(round(y_max, -3), big.mark = ","), " active infections.")
  
    })

  # SIR Plot ----
  output$output_SIR_plot <- renderHighchart({
    
    # Vector of all dates
    dates <- as_date(min(covid_sum()$Date):(max(covid_sum()$Date) + days(365)))
    
    # Days of forecast to plot
    forecast_days <- input$input_days
    
    # Combine actuals and SIR output
    plot_data <- data.frame(Date=dates) %>%
      left_join(covid_sum(), by="Date") %>%
      left_join(sir_proj(), by="Date") %>%
      slice(1:(nrow(covid_sum()) + forecast_days)) %>%
      rename(Cases = Infected, #from covid_sum
             Projection = Mean) #from sir_proj
    
    # SIR plot
    highchart() %>%
      hc_title(text = "Forecast of Active Infections",
               align = "left") %>%
      hc_xAxis(type = "datetime") %>%
      hc_yAxis(min = 0,
               title = list(text = "Active Infections")) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Upper),
                    id = "Higher",
                    name = "Higher Estimate",
                    type = "area",
                    marker = list(enabled = FALSE),
                    color = colors[2],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Projection),
                    id = "Projection",
                    name = "Projected Infections",
                    type = "area",
                    marker = list(enabled = FALSE),
                    color = colors[3],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Lower),
                    id = "Lower",
                    name = "Lower Estimate",
                    type = "area",
                    marker = list(enabled = FALSE),
                    color = colors[4],
                    fillOpacity = 0.2,
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Cases),
                    id = "Cases",
                    name = "Active Infections",
                    type = "column",
                    groupPadding = 0,
                    color = colors[1],
                    showInLegend = FALSE,
                    animation = FALSE)
    
  })
  
}