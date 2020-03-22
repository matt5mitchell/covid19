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

function(input, output) {
  
  # Summarize data and filter
  covid_sum <- reactive({
      covid %>%
        ungroup() %>%
        filter(State %in% input$input_state) %>%
        group_by(Date) %>%
        summarize_at(c("Incidence", "Population", "Susceptible", "Infected", "Removed"), sum) %>%
        slice(min(which(.$Incidence > 0)):nrow(.)) #First detection onward
  })
  
  output$output_Rt_plot <- renderPlot({
    
    # Generation time
    # Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
    gt_lognormal <- generation.time("lognormal", c(4.7, 2.9))
    
    # Estimate Rt
    n_days <- nrow(covid_sum())
    
    Rt_est <- est.R0.TD(epid = covid_sum()$Incidence, t = covid_sum()$Date, gt_lognormal, begin = 1L, end = n_days, nsim = 1000)
    
    Rt_est_df <- data.frame(Rt = Rt_est$R,
                            Lower= Rt_est$conf.int$lower,
                            Upper= Rt_est$conf.int$upper)
    
    # Average over last 7 days
    Rt_7days <- Rt_est_df %>%
      slice((n() - 6):n()) %>%
      summarize_all(mean)
    
    ## Plot effective reproduction number
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