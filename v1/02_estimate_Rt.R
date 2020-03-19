## Estimate Rt - Effective (time-dependent) reproductive rate

library(R0)
library(dplyr)
library(ggplot2)

# Generation time
# Mean and SD from: Nishiura, et al., 2020, https://doi.org/10.1016/j.ijid.2020.02.060
gt_lognormal <-generation.time("lognormal", c(4.7, 2.9))

#Summarize data and filter
incid_sum <- covid %>% 
  group_by(Date) %>%
  summarize(Incidence = sum(Incidence)) %>%
  slice(min(which(.$Incidence > 0)):nrow(.)) #First detection onward

# Estimate Rt
Rt <- est.R0.TD(incid_sum$Incidence, gt_lognormal, nsim = 1000)

#Plot 
data.frame(incid_sum[-1,], #No Rt for first date
           Rt = Rt$R,
           Lower= Rt$conf.int$lower,
           Upper= Rt$conf.int$upper) %>%
  ggplot(aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = .2, fill = "blue") +
  geom_line(aes(y=Rt)) +
  geom_hline(yintercept = 0)
