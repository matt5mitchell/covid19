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

      pickerInput(inputId = "input_states",
                  label = "Select states to forecast",
                  choices = list( "United States" = unique(covid$State)),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    noneSelectedText = "United States"
                  )
                  )
      
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
      "Acknowledgements here",
      br(),
      h3("Limitations"),
      "Limitations here",
      br(),
      br()
    )
  )
)
