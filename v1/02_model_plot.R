## Forecast ##

library(dplyr)
library(R0)
library(ggplot2)
library(scales)
library(deSolve)

## Data --------------------------------

# Summarize data and filter
covid_sum <- covid %>% 
  ungroup() %>%
  group_by(Date) %>%
  summarize_at(c("Incidence", "Population", "Susceptible", "Infected", "Removed"), sum) %>%
  slice(min(which(.$Incidence > 0)):nrow(.)) #First detection onward

## Rt estimate --------------------------------

# Generation time
# Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
gt_lognormal <- generation.time("lognormal", c(4.7, 2.9))

# Estimate Rt
Rt_est <- est.R0.TD(covid_sum$Incidence, gt_lognormal, nsim = 1000)

# Average over last 7 days
Rt_7days <- data.frame(mean = mean(Rt_est$R[(length(Rt_est$R)-6):length(Rt_est$R)]),
                       lower = mean(Rt_est$conf.int$lower[(length(Rt_est$conf.int$lower)-6):length(Rt_est$conf.int$lower)]),
                       upper = mean(Rt_est$conf.int$upper[(length(Rt_est$conf.int$upper)-6):length(Rt_est$conf.int$upper)]))

## SIR model --------------------------------

#Filter to last date for SIR model input
covid_sir_input <- covid_sum %>%
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
  Rt  <- Rt_7days[,i] #effective reproduction number
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
sir_proj <- data.frame(Date = max(covid_sum$Date) + days(0:t),
                       Mean = round(as.data.frame(sir[[1]])$I * P, 0),
                       Lower = round(as.data.frame(sir[[2]])$I * P, 0),
                       Upper = round(as.data.frame(sir[[3]])$I * P, 0))


## Plots --------------------------------

# Theme
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

## Plot effective reproduction number
n_days <- length(Rt_est$R)

data.frame(covid_sum[1:n_days,],
           Rt = Rt_est$R,
           Lower= Rt_est$conf.int$lower,
           Upper= Rt_est$conf.int$upper) %>%
  ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .2, fill = colors[3]) +
  geom_line(aes(y=Rt_est$R)) +
  geom_hline(yintercept = 1, linetype ="dashed") +
  scale_y_continuous(limits = c(0, NA)) +
  ylab("Effective Reproduction Number")

## Plot SIR model projection

# Vector of all dates
dates <- as_date(min(covid_sum$Date):(max(covid_sum$Date) + days(t)))

# Combine actuals and SIR output
plot_data <- data.frame(Date=dates) %>%
  left_join(covid_sum, by="Date") %>%
  left_join(sir_proj, by="Date") %>%
  slice(1:(nrow(covid_sum) + 14)) %>%
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
