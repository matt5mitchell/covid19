## Fit SEIR model

# Fit SEIR model to US data

# Fixed population size, full population in S1 initially
# Movement to S2 based on Gallup polling data
# Use known nu (E to I), gamma (I to R), and mu (I to D) values
# Use R0 values from est_R0_two_compartments.R and gamma to calculate beta values

# Use optimizer to fit cumulative deaths
# Parameters to fit: beta1, beta2, mu, and possibly gamma

## Functions ## ----

# Approximate social distancing adoption ----
# Use function in SEIR model to allow time-dependent increase in social distancing
sigmoid <- function(x, pct = 1, days = 1) {
  s <- 5 / days #approximate days to reaching pct
  2 / (1 + exp(-(x * s))) * pct - pct
}


# SEIR Model ----

# SEIR Model Function
# Function inputs:
# data - data frame with 5 columns: Population, Susceptible, Infected, Removed, Deaths
# par - vector with 7 parameters: soc_dist_pct, soc_dist_days, beta1, beta2, nu, gamma, mu
seir_model <- function(data, par, t) {
  # Filter to first date for SEIR model input
  # Run model forward in time to attempt to match observations
  seir_input <- data %>%
    filter(Date == min(Date))
  
  # Create inputs
  t <- t #days to project
  P <- seir_input$Population
  S <- seir_input$Susceptible / P
  S1 <- S #not social distancing
  S2 <- 0 #social distancing
  E <- 1 / P #data not available, so start with 1
  I <- seir_input$Infected / P
  R <- seir_input$Removed / P
  D <- seir_input$Deaths / P
  
  pct <- par[1]
  days <- par[2]
  beta1 <- par[3]
  beta2 <- par[4]
  nu <- par[5]
  gamma <- par[6]
  mu <- par[7]
  
  # Final inputs for model
  init <- c(S1=S1, S2=S2, E=E, I=I, R=R, D=D) #initial state
  times <- seq(1, t, by = 1) #time points to integrate
  parameters <- list(pct=pct, days=days, beta1=beta1, beta2=beta2, nu=nu, gamma=gamma, mu=mu)
 
  # Define model
  model <- function(t, y, parms) {
    with(as.list(c(t, y, parms)), {
      
      soc_dist <- sigmoid(t, pct, days) #time-dependent increase in social distancing
      
      # Note: I + E assumes infectiousness during latent period
      # dS1 <- -(beta1 * (S1 * (1 - soc_dist)) * (E + I))
      # dS2 <- -(beta2 * (S1 * soc_dist) * (E + I))
      # dE <- (beta1 * (S1 * (1 - soc_dist)) * (E + I)) + (beta2 * (S2 * soc_dist) * (E + I)) - (nu * E)
      dS1 <- -(beta1 * (S1 * (1 - soc_dist)) * (I))
      dS2 <- -(beta2 * (S1 * soc_dist) * (I))
      dE <- (beta1 * (S1 * (1 - soc_dist)) * (I)) + (beta2 * (S2 * soc_dist) * (I)) - (nu * E)
      dI <- (nu * E) - (gamma * I) - (mu * I)
      dR <- (gamma * I)
      dD <- (mu * I)
      
      return(list(c(dS1, dS2, dE, dI, dR, dD)))
    })
  }
  
  # Solve using ode
  model_output <- ode(y=init, times=times, model, parms=parameters)
  
  # Projection from seir model
  seir_proj <- data.frame(Date = seir_input$Date + days(1:t)) %>%
    bind_cols(as.data.frame(model_output[,2:7]) * P)
  
}



# Fit SEIR to Deaths ----
# Function to minimize sum of squared residuals
seir_rss <- function(data, par) {
  with(data, {
    t <- nrow(data)
    model_output <- seir_model(data, par, t)
    # sum((model_output$D - Deaths) ^ 2)
    sum((model_output$I - Infected) ^ 2) + sum((model_output$D - Deaths) ^ 2)
  })
}

# Optimize SEIR parameters ----
opt <- optim(par = c(.6, 25, .5, .1, 1/6.4, 1/5, .005), 
             fn = seir_rss, 
             data = covid_sum_confirmed, 
             method = "L-BFGS-B",
             lower = c(0.3, 10, .1, .01, 1/7.7, 1/7, .0001),
             upper = c(0.9, 50, 3, 1, 1/5.6, 1/3, .01))

# Fit SEIR with optimized parameters ----
model_output <- seir_model(covid_sum_confirmed, opt$par, 365)

model_output %>%
  dplyr::select(-S1, -S2, -R) %>%
  left_join(covid_sum_confirmed %>% dplyr::select(Date, Infected, Deaths), by = "Date") %>%
  filter(Date <= Sys.Date()) %>%
  gather("var", "value", -Date) %>%
  ggplot(aes(x=Date, y=value, color=var)) +
  geom_line()

print(c(pct = opt$par[1], days = opt$par[2], beta1 = opt$par[3], beta2 = opt$par[4], nu = opt$par[5], gamma = opt$par[6], mu = opt$par[7],
        R0_1 = opt$par[3]/opt$par[6], R0_2 = opt$par[4]/opt$par[6],
        R0 = (opt$par[3]/opt$par[6] * (1 - opt$par[1])) + opt$par[4]/opt$par[6] * opt$par[1]))
