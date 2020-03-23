## COVID-19 Forecast
## Shiny app ui script

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(highcharter)

fluidPage(theme = shinytheme("yeti"),
  
  # Title ----
  titlePanel("COVID-19 Forecast"),
  
  # Sidebar layout ----
  sidebarLayout(
    
    # Sidebar panel ----
    sidebarPanel(

      uiOutput("output_states"),
      
      sliderInput("input_days", 
                  label = "Length of forecast (days):", 
                  min = 1,
                  max = 90, 
                  value = 14)
      
    ),
    
    # Main panel ----
    mainPanel(
      
      # Rt plot ----
      h3("How many people are infected by each case of COVID-19?"),
      p("The number of people infected by each case over time is called the ", em("effective reproduction number."), " We can estimate this number from new cases of the disease."),
      p("Bringing the ", em("effective reproduction number"), " to less than one would stop the spread of the disease.  Interventions like social distancing, quarantines, and vaccines make this posible."),
      highchartOutput("output_Rt_plot"),
      br(),
      
      # SIR model plot ----
      h3("How fast will COVID-19 spread?"),
      p("The spread of an epidemic can be modeled by categorizing people as either ", em("Susceptible (S),"), " ", em("Infected (I),"), " or ", em("Removed (R)."), " The epidemic proceeds through a growth and decline process, which can be modeled using these three groups.  The ", em("SIR model"), " is a core model in epidemiology."),
      p("The forecast below is based on the latest surveillance data from government authorities and a rolling 7-day average of the ", em("effective reproduction number"), " from the analysis above."),
      highchartOutput("output_SIR_plot"),
      br(),
      
      # Acknowledgements ----
      h3("Acknowledgements"),
      p("Analysis and forecasts are based on data from ",
        a(href="https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins University Center for Systems Science and Engineering."),
        " Data are derived from WHO, CDC, and other government sources and are updated daily."),
      p("Analysis was inspired by:"), 
      tags$ul(
        tags$li("Kucharski, Adam J., et al. \"Early dynamics of transmission and control of COVID-19: a mathematical modelling study.\" The Lancet Infectious Diseases (2020). ",
           a(href = "https://doi.org/10.1016/S1473-3099(20)30144-4", "https://doi.org/10.1016/S1473-3099(20)30144-4")),
        tags$li("The COVID-19 Hospital Impact Model for Epidemics from Penn Medicine Predictive Healthcare.", 
           a(href = "https://penn-chime.phl.io/", "https://penn-chime.phl.io/")),
      p("Packages used in the analysis and visualization include:"),
      tags$ul(
        tags$li(a(href = "https://cran.r-project.org/package=R0", "R0: a toolbox to estimate reproduction numbers for epidemic outbreaks")),
        tags$li(a(href = "https://www.highcharts.com", "Highcharts: Javascript charting library")),
        tags$li(a(href = "http://jkunst.com/highcharter", "Highcharter: R package"))
      )
      ),
      
      # Limitations ----
      h3("Limitations"),
      p("These analyses and forecasts of COVID-19 are best estimates based on the latest information, and they are dependent on data quality. Under-testing in some regions and under-reporting of results can skew forecasts. We use the latest data reported by government authorities, but all data are subject to change as better information becomes available.")
    )
  ),
  
  # Footer ----
  hr(),
  p("Created by ", 
    a(href="https://www.linkedin.com/in/matt5mitchell", "Matthew Mitchell"), 
    ", in March 2020. Source code is available on ", 
    a(href="https://github.com/matt5mitchell/covid19", "GitHub"),
    ". Data and analyses are as accurate as feasible. Content on this site should be considered preliminary and as a starting point for further research.")
)
