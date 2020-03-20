## COVID-19 Forecasting Tool ##

library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(R0)
library(ggplot2)
library(scales)

#Get data
source("01_load_data.R")

#Estimate effective reproduction rate
source("02_estimate_Rt.R")

#Project epidemic curve
source("03_project_curve.R")