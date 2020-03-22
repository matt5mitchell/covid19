## COVID-19 Forecast
## Shiny app ui script

library(shiny)
library(shinyWidgets)

fluidPage(
  
  # Title ----
  titlePanel("COVID-19 Forecast"),
  
  # Sidebar layout ----
  sidebarLayout(
    
    # Sidebar panel ----
    sidebarPanel(

      uiOutput("output_states"),
      
      sliderInput("input_days", 
                  label = "Select length of forecast (days)", 
                  min = 1,
                  max = 90, 
                  value = 14)
      
    ),
    
    # Main panel ----
    mainPanel(
      h2("How many people will be infected by each case?"),
      plotOutput("output_Rt_plot"),
      br(),
      h2("How fast will COVID-19 spread?"),
      plotOutput("output_SIR_plot"),
      br(),
      tableOutput("output_results_table"),
      br(),
      h3("Acknowledgements"),
      "Analysis and forecasts are based on data from ",
      tags$a(href="https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins University Center for Systems Science and Engineering."),
      " Data are updated daily and are derived from WHO, CDC, and other government sources.",
      br(),
      h3("Limitations"),
      "Limitations here",
      br(),
      br()
    )
  )
)
