## COVID-19 Forecast
## Shiny app server script

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

function(input, output) {
  
  # Summarize data and filter
  covid_sum <- observe({
    if(length(input$input_state) > 0){
      covid %>%
        ungroup() %>%
        filter(State %in% input$input_state) %>%
        group_by(Date) %>%
        summarize_at(c("Incidence", "Population", "Susceptible", "Infected", "Removed"), sum) %>%
        slice(min(which(.$Incidence > 0)):nrow(.)) #First detection onward
    } else {
      covid %>%
        ungroup() %>%
        group_by(Date) %>%
        summarize_at(c("Incidence", "Population", "Susceptible", "Infected", "Removed"), sum) %>%
        slice(min(which(.$Incidence > 0)):nrow(.)) #First detection onward
    }
  })
  
  
  output$output_Rt_plot <- renderPlot({
    
  })
}