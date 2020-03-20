## Project epidemic curve

library(deSolve)
library(dplyr)
library(ggplot2)
library(scales)

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

#Plot
min_val <- min(out.df$I)
max_val <- max(out.df$I)
max_val_t <- min(out.df$time[out.df$I == max_val])
plot_limit <- min(min(out.df$t[out.df$I < (max_val / 20) & out.df$t > max_val_t]), 365) #dynamic x axis

colors <- c("#4b4b4b", "#F26F32", "#2585C7", "#96D05C")
theme_set(theme_minimal() + 
            theme(panel.background = element_blank(),
                  axis.title = element_text(color=colors[1], size=16),
                  axis.text = element_text(color=colors[1], size = 12),
                  panel.grid.major = element_line(size=.5),
                  panel.grid.minor = element_line(size=.5),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()
            ))

out.df %>%
  ggplot(aes(x=time, y=I)) +
  geom_line(size = 1.5, color = colors[3]) +
  scale_x_continuous(limits = c(0, plot_limit), breaks = seq(0, t %/% 30 * 30, by = 30)) +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept = max_val_t, size = 1.5, linetype = "dashed", color = colors[1], alpha = 0.5) +
  geom_text(aes(x=max_val_t, y=median(c(min_val, max_val)), label=paste0(comma(max_val)," infected on day ", max_val_t)),
            size = 6, color = colors[1], angle = 90, vjust = -1) +
  xlab("Days") +
  ylab("People Infected")