## Estimate R0 for two "susceptible" compartments for SEIRD model
# Compartments represent baseline and intervention groups

# Note: run assumption_playground.R first to get data sets



# Proportion of US population practicing social distancing
# Gallup social distancing data 3/16 - 4/3: https://news.gallup.com/poll/307760/three-four-self-isolated-household.aspx
soc_dist <- c(0, NA, NA, NA, NA, NA, .51, NA, NA, NA, .64, NA, NA, .69, NA, NA, NA, .73, NA, NA, .74, NA, NA, NA, .75)

# Linear extrapolation
soc_dist_fun <- approxfun(soc_dist, rule = 2)

# Remove first x-1 days due to high variability and implausible R0 (R0 > 3.5???)
x_days <- 1
dat <- Rt_df[x_days:nrow(Rt_df),]
len <- nrow(dat)

# Delay social distancing effect by length of incubation period
# 6.4 days: Prem, et al. 2020 https://doi.org/10.1016/S2468-2667(20)30073-6
# 5.1 days: Luer, et al. 2020 https://doi.org/10.7326/M20-0504
incubation_days <- 6.4 #5.1

# Function to minimize sum of squared residuals
min_RSS <- function(data, par) {
  with(data, {
    len <- length(Rt)
    x <- vector(mode = "numeric", length = len)
    for (i in 1:len) {
      S1 <- par[1] #baseline group
      S2 <- par[2] #intervention group
      wt <- soc_dist_fun(i + x_days - 1 - incubation_days)
      x[i] <- S1 * (1 - wt) + S2 * wt
      }
    sum((x - Rt) ^ 2)
    })
}

# Fit Rt data with optimizer to get both R0 values
opt_R0 <- optim(par = c(3,1), 
                fn = min_RSS, 
                data = dat, 
                method = "L-BFGS-B",
                lower = 0,
                upper = 4)

# Get predicted R0 values over time
pred <- vector(mode = "numeric", length = len)
for (i in 1:len) {
  S1 <- opt_R0$par[1]
  S2 <- opt_R0$par[2]
  wt <- soc_dist_fun(i + x_days - 1 - incubation_days)
  pred[i] <- ((S1 * (1 - wt)) + (S2 * wt))
}

# Plot
dat %>%
  mutate(x = row_number(),
         pred = pred) %>%
  dplyr::select(x, Rt, pred) %>%
  gather("var", "value", -x) %>%
  ggplot(aes(x = x, y = value, color = var)) +
  geom_line()

#Print R0 values for both compartments
print(opt_R0$par)