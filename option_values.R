rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(gridExtra)
library(reshape2)

T0 = 0 # Value of an hectare of forest today
D1 = 20 # Value of Palm oil production in STATE 1 
A0 = 10 # Value of ecosystem services today
T1 = 25 # Value of an hectare of forest in STATE 1
A2 =  10 # Value of ecosystem services in STATE 2
delta = 0.05 # discount rate
pi = seq(0,1,0.01)  # probability of STATE 1

Develop = rep(T0 + (D1/delta),101) # present value of cutting the forest and planting palm oil today
Preserve = A0 + (pi*T1 + (1-pi)*A2)/(delta + pi) + (pi*D1)/(delta*(delta+pi)) # present value of waiting for next period to cut the forest
diff_DP = Develop-Preserve
DP_ratio = Develop/Preserve
option_value = (pi*T1 + (1-pi)*A2)/(delta + pi) + (pi*D1)/(delta*(delta+pi))
A2 = (Develop - A0 - ((pi*T1)/(delta + pi)) - (pi*D1)/(delta*(delta+pi)) )*((delta+pi)/(1-pi))
A2A0 = A2/A0

data = cbind.data.frame(pi, Develop,Preserve, diff_DP,DP_ratio, option_value, A2, A2A0) %>%
dplyr::filter( A2A0>=-10)


ggplot(data = data) + 
  geom_point(aes(x = pi, y =A2A0, color='red')) +
  geom_point(aes(x = pi, y =A2, color='blue')) +
  theme(legend.position="none")

ggplot(data = data) + 
  geom_line(aes(x = pi, y =DP_ratio, color='red')) +
  theme(legend.position="none")

r = (D1/A0) - (delta-pi)/(1-pi) - 1