rm(list=ls())
library(ggplot2)
library(tidyr)
library(purrr)
library(gridExtra)
library(reshape2)
reps = 100
years = 30
dt = 1 
POo = 692*4.2-2040
muPO = 2/100
sigmaPO = 7/100
muES = 5/100
sigmaES = 3/100
conv_cost = 1040*10
delta = 10/100
alphaCI = 1.96
time = seq(0,years,1)

beta = (0.5-(muPO/(sigmaPO^2))) + (((muPO/(sigmaPO^2))-0.5)^2 + ((2*delta)/(sigmaPO^2)))^(1/2)
CAP_deltaPO = (beta*(delta-muPO))/(beta-1)
POstar = conv_cost*CAP_deltaPO  
POstar = rep(POstar, years+1)   

alpha = -(0.5-(muES/(sigmaES^2))) + (((muES/(sigmaES^2))-0.5)^2 + ((2*delta)/(sigmaES^2)))^(1/2)
CAP_deltaES = (alpha*(delta-muES))/(alpha+1)
ESstar = POo*CAP_deltaES
ESstar = rep(ESstar, years+1)   

PO = matrix(NA, reps, years+1)   
PO[, 1] <- POo 
# Simulate geometric brownian motion with m2 as drift and s2 as SD of drift
for(i in 1:reps) {
  for (t in 1:years) {
    PO[i,t+1] = PO[i,t]*(1 + muPO*dt + sigmaPO*sqrt(dt)*rnorm(1, 0, 1))
  }
}

UB_PO = (1+muPO)^(time+1)*(1+alphaCI*sigmaPO)^((time^(1/2)))*POo 
LB_PO = (1+muPO)^(time+1)*(1-alphaCI*sigmaPO)^((time^(1/2)))*POo
max_PO = max(UB_PO)
min_PO = max(LB_PO)
avgPO = mean(UB_PO)

data <- data.frame(cbind( time, t(PO),UB_PO, LB_PO, POstar, ESstar )) 
df2 <- melt(data = data, id.vars = "time")

ggplot(data = df2, aes(x = time, y = value, color=variable)) + 
  geom_line() +
  theme(legend.position="none")



# Function for multiple plots
# lapply(names(data), function(i) {
#   p1<- ggplot(data) +
#    geom_line(aes(time, data[ ,i])) 
#   print(i)
#   print(p1)
#   myplots[[i]] <<- p1
# })


# Work on real option
# What is the price where is not better to plant palm oil? 
# What is the max ES value that makes bad to plant palm oil? 


