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
Rt_7days <- mean(Rt_est$R[(length(Rt_est$R)-6):length(Rt_est$R)])

## SIR model --------------------------------

#Filter to last date for SIR model input
covid_sir_input <- covid_sum %>%
  filter(Date == max(Date))

# Parameter inputs
t   <- 365 #max days to project
Rt  <- Rt_7days #effective reproduction number
T_r <- 14 #recovery time (assumed to be 14 days)
gamma <- 1 / T_r
beta  <- gamma * Rt
P <- covid_sir_input$Population
S <- covid_sir_input$Susceptible / P
I <- covid_sir_input$Infected / P
R <- covid_sir_input$Removed / P
init <- c(S=S, I=I, R=R)
parameters <- c(bet=beta, gamm=gamma)
times <- seq(0, t, by = 1)

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
sir <- ode(y=init, times=times, sir_model, parms=parameters)

#Output
sir_df <- as.data.frame(sir) %>%
  mutate(S = round(S * P, 0),
         I = round(I * P, 0),
         R = round(R * P, 0))

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

## Effective reprouction number
n_days <- length(Rt_est$R)

data.frame(covid_sum[1:n_days,],
           Rt = Rt_est$R,
           Lower= Rt_est$conf.int$lower,
           Upper= Rt_est$conf.int$upper) %>%
  ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .2, fill = colors[3]) +
  geom_line(aes(y=Rt_est$R)) +
  geom_hline(yintercept = 1, linetype ="dashed") +
  scale_y_continuous(limits = c(0, NA))

## SIR model projection

# Vector of all dates from covid_sum plus sir_df
max_date <- max(covid_sum$Date)
sir_df$Date <- max_date + days(sir_df$time)

# Combine datasets
cases <- covid_sum %>%
  dplyr::select(Date, Infected) %>%
  mutate(Type = "Cases")

projection <- sir_df %>%
  dplyr::select(Date, I) %>%
  rename(Infected = I) %>%
  mutate(Type = "Projection") 

plot_data <- cases %>%
  bind_rows(projection)

# SIR plot
min_val <- min(plot_data$Infected)
max_val <- max(plot_data$Infected)
max_val_t <- min(plot_data$Date[plot_data$Infected == max_val])
x_min <- min(plot_data$Date)
x_max <- min(min(plot_data$Date[plot_data$Infected < (max_val / 20) & plot_data$Date > max_val_t]), max(plot_data$Date)) #dynamic x axis

plot_data %>%
  ggplot(aes(x = Date, y = Infected, color = Type)) +
  geom_line() +
  scale_x_date(limits = c(x_min, x_max)) +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = max_val_t, linetype = "dashed") +
  geom_text(aes(x=max_val_t, y=median(c(min_val, max_val)), label=paste0(comma(max_val)," infected on ", max_val_t)),
            size = 4, color = "gray40", angle = 90, vjust = -1) +
  xlab("Date") +
  ylab("Active Cases")
