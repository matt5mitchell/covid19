## Project epidemic curve

## Inputs ##

# Parameter inputs
t   <- 365 #max days to project
Rt  <- Rt_7days #effective reproduction number
T_r <- 14 #recovery time (assumed to be 14 days)
gamma <- 1 / T_r
beta  <- gamma * Rt

# SIR inputs
covid_sir_last <- covid_sir %>%
  ungroup() %>%
  group_by(Date) %>%
  summarize_at(c("Susceptible", "Infected", "Removed"), sum) %>%
  ungroup() %>%
  filter(Date == max(Date))

P <- rowSums(covid_sir_last[c("Susceptible", "Infected", "Removed")]) #total population
S <- covid_sir_last$Susceptible/P
I <- covid_sir_last$Infected/P
R <- covid_sir_last$Removed/P

## Model ##

# Define model
sir_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -bet * S * I
    dI <-  bet * S * I - gamm * I
    dR <-                gamm * I
    return(list(c(dS, dI, dR)))
  })
}

#Inputs
init <- c(S=S, I=I, R=R)
parameters <- c(bet=beta, gamm=gamma)
times <- seq(0, t, by = 1)

#Solve using ode
out <- ode(y=init, times=times, sir_model, parms=parameters)

#Output
out.df <- as.data.frame(out) %>%
  mutate(S = round(S * P, 0),
         I = round(I * P, 0),
         R = round(R * P, 0))

## Combine historicals and projections ##

#Historicals
covid_sir_sum <- covid_sir %>%
  ungroup() %>%
  group_by(Date) %>%
  summarize_at(c("Susceptible", "Infected", "Removed"), sum) %>%
  slice(min(which(.$Infected > 0)):nrow(.)) #First detection onward

#Vector of all dates from covid_sir_sum plus out.df
max_date <- max(covid_sir_sum$Date)
out.df$Date <- max_date + days(out.df$time)

#Combine datasets
cases <- covid_sir_sum %>%
  dplyr::select(Date, Infected) %>%
  mutate(Type = "Cases")

projection <- out.df %>%
  dplyr::select(Date, I) %>%
  rename(Infected = I) %>%
  mutate(Type = "Projection") 

plot_data <- cases %>%
  bind_rows(projection)

#Plot
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

min_val <- min(plot_data$Infected)
max_val <- max(plot_data$Infected)
max_val_t <- min(plot_data$Date[plot_data$Infected == max_val])
x_min <- min(plot_data$Date)
x_max <- min(min(plot_data$Date[plot_data$Infected < (max_val / 20) & plot_data$Date > max_val_t]), max(plot_data$Date)) #dynamic x axis


plot_data %>%
  ggplot(aes(x = Date, y = Infected, color = Type)) +
  geom_line(size = 1.2) +
  scale_x_date(limits = c(x_min, x_max)) +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = max_val_t, size = 1.2, linetype = "dashed", alpha = 0.5) +
  geom_text(aes(x=max_val_t, y=median(c(min_val, max_val)), label=paste0(comma(max_val)," infected on ", max_val_t)),
            size = 4, color = "gray40", angle = 90, vjust = -1) +
  xlab("Date") +
  ylab("Active Cases")


