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
  
  # Cases Plot ----
  output$output_cases_plot <- renderHighchart({
    
    plot_data <- covid_sum_inc() %>%
      dplyr::select(Date, Confirmed, Deaths) %>%
      group_by(Date) %>%
      summarize(Confirmed = sum(Confirmed),
                Deaths = ifelse(sum(Deaths) > 0, sum(Deaths), NA))
    
    # Cases plot
    highchart() %>%
      hc_title(text = "Confirmed Cases and Deaths",
               align = "left") %>%
      hc_xAxis(type = "datetime") %>%
      hc_yAxis(type = "logarithmic",
               minorTickInterval = 0.1) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Confirmed),
                    name = "Confirmed Cases",
                    type = "line",
                    marker = list(enabled = TRUE),
                    color = colors[3],
                    showInLegend = FALSE,
                    animation = FALSE) %>%
      hc_add_series(plot_data,
                    hcaes(x = Date, y = Deaths),
                    name = "Reported Deaths",
                    type = "line",
                    marker = list(enabled = TRUE),
                    color = colors[2],
                    showInLegend = FALSE,
                    animation = FALSE)
    
  })
  
  # Add incidence ----
  covid_sum_inc <- reactive({
    
    estimate_recovered(covid_sum())  %>%
      mutate(Incidence = Confirmed - lag(Confirmed, n = 1L, default = 0),
             Incidence = ifelse(Incidence < 0, 0, Incidence)) %>% #Prevent negatives from bad data
      slice(min(which(.$Incidence > 0)):n())
    
  })
  
  # Estimate Rt ----
  Rt_df <- reactive({
    
    # Generation time
    # Updated mean and SD from: Du, et al, 2020, https://doi.org/10.3201/eid2606.200357
    # Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
    gt_lognormal <- generation.time("gamma", c(3.96, 4.75)) # previous: c(4.7, 2.9))
    
    # Number of days in dataset
    n_days <- nrow(covid_sum_inc())
    
    # Estimate Rt
    Rt_est <- est.R0.TD(epid = covid_sum_inc()$Incidence, 
                        GT = gt_lognormal, 
                        n.t0 = covid_sum_inc()$Incidence[1],
                        t = covid_sum_inc()$Date, 
                        begin = 1L, 
                        end = n_days, 
                        time.step = 1L, 
                        nsim = 1000)
    
    data.frame(Rt = Rt_est$R,
               Lower= Rt_est$conf.int$lower,
               Upper= Rt_est$conf.int$upper) %>%
      dplyr::na_if(0) %>%
      imputeTS::na_interpolation(option = "spline") %>%
      #Rolling 3 day averages to smooth data
      mutate(Rt = (Rt + lag(Rt, 1) + lag(Rt, 2)) / 3,
             Lower = (Lower + lag(Lower, 1) + lag(Lower, 2)) / 3,
             Upper = (Upper + lag(Upper, 1) + lag(Upper, 2)) / 3)
    
  })
  
  # Rt plot ----
  output$output_Rt_plot <- renderHighchart({
    
    plot_data <- covid_sum_inc() %>%
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
  
}