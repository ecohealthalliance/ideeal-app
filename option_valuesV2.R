rm(list=ls())
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(gridExtra)
library(reshape2)

#T0 = 0 # Value of an hectare of forest today
beta = 0.2
price_PO0 = 30 # Value of Palm oil production in STATE 1 
quantity_PO0 = 100
Revenues0 = price_PO0*quantity_PO0
Costs0 = beta*quantity_PO0^2
Profits0 = Revenues0 - Costs0
price_PO1 = 30 # Value of Palm oil production in STATE 1 
quantity_PO1 = 100
Revenues1 = price_PO1*quantity_PO1
Costs1 = beta*quantity_PO1^2
Profits1 = Revenues1 - Costs1
ESV0 = 900 # Value of ecosystem services today
#T1 = 25 # Value of an hectare of forest in STATE 1
ESV2 =  1000 # Value of ecosystem services in STATE 2
delta = 0.05 # discount rate
pi = seq(0,1,0.01)  # probability of STATE 1

Develop = rep(Profits0/delta,101) # present value of cutting the forest and planting palm oil today
Preserve = ESV0 +  (1-pi)*ESV2/(delta + pi) + (pi*Profits1)/(delta*(delta+pi)) # present value of waiting for next period to develop land
diff_DP = Develop-Preserve
DP_ratio = Develop/Preserve
option_value = (1-pi)*ESV2/(delta + pi) + (pi*price_PO1)/(delta*(delta+pi))
#ESV2 = (ESV0 - Develop - price_PO1/delta*(1-pi/(delta*(delta+pi))) )* ((delta+pi)/(1-pi))



data = cbind.data.frame(pi, Develop,Preserve, diff_DP,DP_ratio, option_value, ESV2, ESV2ESV0) %>%
  dplyr::filter( ESV2ESV0>=-10)


ggplot(data = data) + 
  geom_point(aes(x = pi, y =ESV2ESV0, color='red')) +
  geom_point(aes(x = pi, y =ESV2, color='blue')) +
  theme(legend.position="none")

ggplot(data = data) + 
  geom_line(aes(x = pi, y =DP_ratio, color='red')) +
  theme(legend.position="none")

## How much  ES should increase in the future to compensate for 
ESV2 = Profits0*(delta+pi)/(delta*(1-pi)) - ESV0*(delta+pi)/(1-pi) - pi*Profits1/(delta*(1-pi))
ESV2ESV0 = ESV2/ESV0
