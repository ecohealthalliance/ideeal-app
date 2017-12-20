
### Estimate the true cost of a metric ton of palm oil 
### and the difference between the international CPO price 
### and the true cost (CPO price + social cost = true cost)  

rm(list=ls())
library(optimx)
library(ggplot2)

time <- seq(0,50) # Set time periods
time2 = seq(1,81,1)

  #### INPUTS ####
  total_land      = 7363000  # total land 
  forest_land     = 0.68 # total forest land 
  cost_per_HA     = 1410
  ES_value        = 2500
  CPO_price       = 658 # Use 517 for the historic average
  CPO_yield       = 4.20
  kerneloil_price = 1382 # Use 832 for the historic average
  kerneloil_yield = 4.20
  rho             = 0.05
  expenditures    = 9e6
  infections      = 2000
  population      = 3.55e6
  prop_CPO_total  = 0.8
  #### INPUTS ####
  
  developed_land = total_land*(1- forest_land)  #total already developed land
  prop_developed_land = developed_land/total_land # proportion of developed land from total (%)
  prop_forest_land = (total_land - developed_land)/total_land  # proportion of forest land from total (%)
  HA_in_1_percent = (1/100)*total_land
  
  pconv = cost_per_HA/total_land
  you = (pconv + rho/(1+prop_developed_land))/((1-prop_developed_land))
  #factor1 = 1/you^2
  factor1 = 15000
  
  # Production and cost parameters
  theta = 1 # effectiveness of land conversion
  C_coeff = 2 # Coefficient for C(u) curve
  g1 = factor1*cost_per_HA*HA_in_1_percent 
  g2 = cost_per_HA*HA_in_1_percent  
  
  
  ESTV = ES_value*100*HA_in_1_percent 
  b = 4 # curvature of value for ecosystem services as a function of land covnerted - as higher number higher curvature (if b =1, linear relationship)
  r = 0.00 # growth rate of ecosystem 
  
  #Health Damages 
  n1 = 1/14500 # denominator factor for relationship between Xt and St
  omega = 1 #rate of reduction of indcidence (if equal to 1, 100% reduction from period to period)
  cost_public_health = expenditures*3.33  #total public expenditures on prevention and control of malaria = 9 millions. We assume that 30% is public expenditures and 70% is private expenditures that we don't observe
  infected = infections
  pop = population
  d1 = cost_public_health/infected
  d2 = 1.1 # curvature of simple damage function 
  d4 = -11 # curvature for complex damage function
  d5 = 0.1 # inflection point for complex damage function
  d6 = 0.01  # level point for complex damage function
  d3 = cost_public_health/(1/(exp(d4*(0.25-d5))+ d6 )) 
  
  
  CPOprice = CPO_price*CPO_yield*HA_in_1_percent
  kernelprice = kerneloil_price*kerneloil_yield*HA_in_1_percent
  prop_kernel_total= 1 - prop_CPO_total
  
  #  Calibration parameters 
  Uo = 0.00000001 # Initial effort of land conversion at time 0
  Xo =  prop_developed_land*100# Initial proportion of total landscape developed at time 0
  FXo = r/(1+Xo) # Initial value for function of 
  So = (Xo/100)^2/n1
  Zo = So 
  Ro = CPOprice*Xo*prop_CPO_total + kernelprice*Xo*prop_kernel_total # Initial revenue at time 0
  Co = g1*Uo^C_coeff + g2*Xo    # Initial cost at time 0
  profitso = Ro - Co
  Bo = ESTV*((exp(b) - exp(b*(Xo/100)))/(exp(b)-1))
  #Do = d1*Zo^d2   # Initial damages at time 0 (simple function)
  Do = d3*(exp(d4 * ((Zo*n1)^(1/2)- d5)) + d6 )**(-1)  # Initial damages at time 0 (complex function)  
  Wo = profitso + Bo - Do
  
  Private_Vo = Ro - Co
  Social_Vo = profitso + Bo - Do
  
  # Vectors for function
  U <- rep(Uo, length(time))                 # vector for effort in land conversion
  FX <- rep(FXo,length(time))                # vector for function of ecosystem growth
  X <- rep(Xo,length(time))                  # vector for stock of developed land X(t)
  S <- rep(So,length(time))
  Z <- rep(Zo,length(time))
  R <- rep(Ro,length(time))                  # vector for the revenue function R(X(t))
  C <- rep(Co, length(time))                 # vector for the cost function C(U(t))
  profits  = rep(profitso,length(time))
  disc_profits = rep(profitso,length(time))
  B <- rep(Bo, length(time))                 # vector for benefit function B(X(t))
  disc_B = rep(Bo, length(time))      
  D <- rep(Do, length(time))                 # vector for the damage function D(X(t))
  disc_D = rep(Do, length(time))    
  W <- rep(Wo, length(time)) 
  W_dis <- rep(Wo, length(time)) 
  result <- 0 
  
  # Function to be optimized
  opt_W <- function (U, npar = 1) {
    for (i in 1:(length(time)-1)){
      X[i+1] = X[i] + theta*U[i]*(100-X[i]) - r/(1+X[i])  
      S[i] = (X[i]/100)^2/n1
      Z[i+1] = (1- omega)*Z[i] + S[i]
      R[i] = CPOprice*X[i]*prop_CPO_total + kernelprice*X[i]*prop_kernel_total         # Revenue function for developed land
      C[i] = g1*U[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
      profits[i] = R[i] - C[i]
      disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
      B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
      disc_B[i] = B[i]*exp(-rho*(i-1)) 
      #D[i] = d1*Z[i]^d2
      D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
      disc_D[i] = D[i]*exp(-rho*(i-1))
      W[i] = profits[i] + B[i] - D[i] 
      W[length(time)] = ((profits[length(time)] + B[length(time)] - D[length(time)]  )*exp(-rho *(time[length(time)]-1)))/rho
    }
    W_dis = W*exp(-rho*time) # Discounted value function
    NPV_W <- sum(W_dis)
    return(NPV_W)
  }
  
  max_W <- optimx(par = U, fn = opt_W, lower = -1, upper = 1,
                  method = ("L-BFGS-B"),
                  control = list(maximize = TRUE, trace=6))
  
  U_max <- t(data.frame(coef(max_W)))
  

  ## Estimate optimal stock of land each period (X)
  for (i in 1:(length(time)-1)){
    X[i+1] = X[i] + theta*U_max[i]*(100-X[i]) - r/(1+X[i])  
    S[i] = (X[i]/100)^2/n1
    Z[i+1] = (1- omega)*Z[i] + S[i]
    R[i] = CPOprice*X[i]*prop_CPO_total + kernelprice*X[i]*prop_kernel_total        # Revenue function for developed land
    C[i] = g1*U_max[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
    profits[i] = R[i] - C[i]
    disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
    B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
    disc_B[i] = B[i]*exp(-rho*(i-1)) 
    #D[i] = d1*Z[i]^d2 # simple damage function
    D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
    disc_D[i] = D[i]*exp(-rho*(i-1))
    W[i] = profits[i] + B[i] - D[i] 
    W[length(time)] = ((profits[length(time)] + B[length(time)] - D[length(time)]  )*exp(-rho *(time[length(time)]-1)))/rho
  }
  W_dis = W*exp(-rho*time) # Discounted value function
  NPV_W_social <- sum(W_dis)/1e9
  NPV_profits_social <- sum(disc_profits)/1e9
  NPV_ES_social <- sum(disc_B)/1e9
  NPV_D_social <- sum(disc_D)/1e9

  
Rsocial = R
Xsocial = X
Usocial = U_max

  ggplot() + 
    geom_line(aes(time, Usocial, color="blue"), size=2) +
    xlim(0, 50) +
    ylim(0, 0.01) +
    labs( x = "Time" , y = "Optimal rate of land conversion to palm oil (%)") + 
    theme(legend.position="none")
  
##############################################
  ### Find the optimal price for private optimal 
  opt_W2 <- function (U, npar = 1) {
    for (i in 1:(length(time)-1)){
      X[i+1] = X[i] + theta*U[i]*(100-X[i]) - r/(1+X[i])  
      #S[i] = (X[i]/100)^2/n1
      #Z[i+1] = (1- omega)*Z[i] + S[i]
      R[i] = ( CPOprice*X[i]*prop_CPO_total + kernelprice*X[i]*prop_kernel_total )
      #R[i] = G*CPO_yield*HA_in_1_percent*X[i]        # Revenue function for developed land
      C[i] = g1*U[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
      profits[i] = R[i] - C[i]
      #disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
      #B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
      #disc_B[i] = B[i]*exp(-rho*(i-1)) 
      #D[i] = d1*Z[i]^d2
      #D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
      #disc_D[i] = D[i]*exp(-rho*(i-1))
      W[i] = profits[i]  
      W[length(time)] = ((profits[length(time)] )*exp(-rho *(time[length(time)]-1)))/rho
    }
    W_dis = W*exp(-rho*time) # Discounted value function
    NPV_W <- sum(W_dis)
    return(NPV_W)
  }
  
  max_W2 <- optimx(par = U, fn = opt_W2, lower = -1, upper = 1,
                  method = ("L-BFGS-B"),
                  control = list(maximize = TRUE, trace=6))
  
  U_max2 <- t(data.frame(coef(max_W2)))
  

    for (i in 1:(length(time)-1)){
      X[i+1] = X[i] + theta*U_max2[i]*(100-X[i]) - r/(1+X[i])  
      #S[i] = (X[i]/100)^2/n1
      #Z[i+1] = (1- omega)*Z[i] + S[i]
      R[i] = ( CPOprice*X[i]*prop_CPO_total + kernelprice*X[i]*prop_kernel_total )       # Revenue function for developed land
      #R[i] = G*CPO_yield*HA_in_1_percent*X[i]        # Revenue function for developed land
      C[i] = g1*U_max2[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
      profits[i] = R[i] - C[i]
      #disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
      #B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
      #disc_B[i] = B[i]*exp(-rho*(i-1)) 
      #D[i] = d1*Z[i]^d2
      #D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
      #disc_D[i] = D[i]*exp(-rho*(i-1))
      W[i] = profits[i]  
      W[length(time)] = ((profits[length(time)] )*exp(-rho *(time[length(time)]-1)))/rho
    }
    
  Rprivate  = R
  Xprivate = X
  Uprivate <- U_max2
  
  R_dis = R*exp(-rho*time) # Discounted value function
  NPV_R <- sum(R_dis)/1e9
  
  ### optimal social cost of palm oil per hectare (how much the government should charge extra to companies 
  ### for a metric ton of palm oil to reduce production to the social optimum amount)
  G = (  Rprivate - Rsocial    )/ (CPO_yield*HA_in_1_percent*Xsocial)
  
  AvgG = mean(G[2:50])
  
  AvgG1 = mean(G[2:11])
  AvgG2 = mean(G[12:21])
  AvgG3 = mean(G[22:31])
  AvgG4 = mean(G[32:41])
  AvgG5 = mean(G[42:50])
  
  G_dis = G*exp(-rho*time) # Discounted value function
  NPV_G <- sum(G_dis)
  
  ### Find the optimal price for social optimal 
  opt_W3 <- function (U, npar = 1) {
    for (i in 1:(length(time)-1)){
      X[i+1] = X[i] + theta*U[i]*(100-X[i]) - r/(1+X[i])  
      #S[i] = (X[i]/100)^2/n1
      #Z[i+1] = (1- omega)*Z[i] + S[i]
      R[i] = ( CPOprice*X[i]*prop_CPO_total + kernelprice*X[i]*prop_kernel_total )  - G[i]*CPO_yield*HA_in_1_percent*X[i]     # Revenue function for developed land
      #R[i] = G*CPO_yield*HA_in_1_percent*X[i]        # Revenue function for developed land
      C[i] = g1*U[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
      profits[i] = R[i] - C[i]
      #disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
      #B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
      #disc_B[i] = B[i]*exp(-rho*(i-1)) 
      #D[i] = d1*Z[i]^d2
      #D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
      #disc_D[i] = D[i]*exp(-rho*(i-1))
      W[i] = profits[i]  
      W[length(time)] = ((profits[length(time)] )*exp(-rho *(time[length(time)]-1)))/rho
    }
    W_dis = W*exp(-rho*time) # Discounted value function
    NPV_W <- sum(W_dis)
    return(NPV_W)
  }
  
  max_W3 <- optimx(par = U, fn = opt_W3, lower = -1, upper = 1,
                   method = ("L-BFGS-B"),
                   control = list(maximize = TRUE, trace=6))
  
  U_max3 <- t(data.frame(coef(max_W3)))
  
  Uprivate_nonoptimal <- U_max3
  
  
  
  
  # Comparison 
  qplot(Usocial,Uprivate_nonoptimal) # They should be equal if G reduces conversion
  qplot(Usocial,Uprivate) # They should be equal if G reduces conversion
  
  plot(G[1:50])
  
  diff_U <- sum(Uprivate_nonoptimal - Usocial )
  plot(diff_U)
  