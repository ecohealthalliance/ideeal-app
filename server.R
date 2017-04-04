library(shiny)
library(optimx)

# Simulation and Shiny Application of Optimization
shinyServer(function(input, output) {
  
  time = seq(0,80) # Set time periods
  theta = 1 # effectiveness of land conversion
  d_coeff = 2 # Health damage exponential function
  b_coeff = 1 # ES benefits exponential function
  pop_density = 0.3 # Number of people per hectare in the study area
  alpha = 1 # Parameter for revenue function
  delta = 0.01 # Growth rate of forest 
    
    
  mydata <- reactive({
    
    total_land    = input$total_land 
    forest_land   = input$forest_land
    palmoil_price = input$palmoil_price
    p_c           = input$p_c            # Marginal cost per ha of forest converted to palm oil 
    p_b           = input$p_b
    c_coeff       = input$c_coeff # Coefficient for C(u) curve
    p_d           = input$p_d            
    palmoil_yield = input$palmoil_yield
    rho           = input$rho # discount rate
    years         = input$years
    
    # Set parameters
    # production 
    
    
    kerneloil_price = 1
    kerneloil_yield = 1  
    py = palmoil_price*palmoil_yield
    prop_forest_land = forest_land # proportion of forest land from total (%)
    prop_developed_land =  1 - prop_forest_land # proportion of developed land from total (%)
    population = pop_density*total_land
    
    # Health Damages
    number_of_malaria_cases = 0
    treatment_cost_per_person = 0
    

    Uo = 0.001 # Initial effort of land conversion at time 0
    Xo = prop_developed_land  # Initial proportion of total landscape developed at time 0
    FXo = delta/(1+Xo)  # Initial forest growth rate
    Ro = palmoil_price*Xo # Initial revenue at time 0
    Bo = p_b*(1-Xo)^b_coeff
    Co = p_c*Uo^c_coeff    # Initial cost at time 0
    Do = p_d*Xo^d_coeff    # Initial cost at time 0
    Vo = Ro - Co
    
    # Vectors for function
    U <- rep(Uo, length(time))                 # vector for effort in land conversion
    X <- rep(Xo,length(time))                  # vector for stock of developed land X(t)
    X_land <- rep(Xo,length(time))  
    FX = rep(FXo,length(time))   
    R <- rep(Ro,length(time))                  # vector for the revenue function R(X(t))
    B <- rep(Bo, length(time))  
    C <- rep(Co, length(time))                 # vector for the cost function C(U(t))
    D <- rep(Do, length(time))      
    V <- rep(Vo, length(time))                  # vector for value function V(U(t))
    V_diso = Vo*exp(-rho * time) 
    V_dis <- rep(V_diso, length(time))              # vector for discounted value function V(t)*exp(-rho*time)
    resulto = sum(V_diso)
    result <- resulto
   
    
# Include Health damages, Ecosystem services, and yield of palm oil 
   
    # Social optimal function
    private_opt_V <- function (U, npar = 1, print = TRUE) {
      for (i in 2:length(time)){
        FX[i] = delta/(1+X[i])
        X[i] = X[i-1] + theta*U[i]*(1 - X[i]) -FX[i] # change in developed land
        R[i] = py*log(1+alpha*X[i])            # Revenue function for developed land price*
        C[i] = (p_c*U[i])^c_coeff              # Cost function using effort to develop land
        V[i] = R[i] - C[i]                     # Value function
        V[length(time)] = ((R[length(time)] - C[length(time)] )*exp(-rho *(time[length(time)]-1)))/rho
      }
      V_dis = V*exp(-rho * time) # Discounted value function
      private_result <- sum(V_dis)
      return(private_result)
    }
      
    # Optimize system by choosing effort
    max_private_opt_V <- optimx(par = U, fn = private_opt_V, lower = 0, upper = 1,
                               method = ("L-BFGS-B"),
                               control = list(maximize = TRUE, trace=6), hessian = FALSE)
    U_private_max <- coef(max_private_opt_V)
    
    # Social optimal function
    social_opt_V <- function (U, npar = 1, print = TRUE) {
      for (i in 2:length(time)){
        FX[i] = delta/(1+X[i])
        X[i] = X[i-1] + theta*U[i]*(1 - X[i]) - FX[i] # change in developed land
        R[i] = py*log(1+alpha*X[i])            # Revenue function for developed land price  alpha = 10
        B[i] = (p_b*(1-X[i]))^b_coeff    
        C[i] = (p_c*U[i])^c_coeff                    # Cost function using effort to develop land
        D[i] = (p_d*X[i])^d_coeff 
        V[i] = R[i] + B[i] - C[i] - D[i]                  # Value function
        V[length(time)] = ((R[length(time)] + B[length(time)] - C[length(time)] - D[length(time)] )*exp(-rho *(time[length(time)]-1)))/rho
      }
      V_dis = V*exp(-rho * time) # Discounted value function
      social_result <- sum(V_dis)
      return(social_result)
    }
    
    X_social= rep(Xo,length(time))
    X_private= rep(Xo,length(time))
    
    withProgress(message = 'Optimizing',{ 
      
    # Optimize system by choosing effort
    max_social_opt_V <- optimx(par = U, fn = social_opt_V, lower = 0, upper = 1,
                    method = ("L-BFGS-B"),
                    control = list(maximize = TRUE, trace=6), hessian = FALSE)
    U_social_max <- coef(max_social_opt_V)
    
  
    
    for (i in 2:length(time)){
      X_social[i] = X_social[i-1] + theta*U_social_max[i]*(1 - X_social[i])  
    }
    
    for (i in 2:length(time)){
      X_private[i] = X_private[i-1] + theta*U_private_max[i]*(1 - X_private[i])  
    }
    
    return(list(X=X, X_social=X_social,X_private=X_private, 
                U_private_max=U_private_max, U_social_max=U_social_max))

    
  })
  
  }) 
    
  output$Plot1 <- renderPlot({
   
     withProgress(message = 'Making plot', value = 0, {
    
    plot(time, mydata()$X_social, main = 'Private and Social optimal allocation of palm oil development (%)', 
         col = 'darkgreen', xlim =c(0,max(time)) , ylim =c(0,1) )
       par(new=TRUE)
       plot(time, mydata()$X_private, col = 'darkblue', xlab = 'Years', ylab = 'Development (%)')   
      })
  })
  
  output$Plot2 <- renderPlot({
    
    plot(time, mydata()$X_land, main = 'Optimal allocation through the years (ha)', 
         col = 'darkblue', xlab = 'Years', ylab = 'Hectares of palm oil plantations', xlim =c(0,80) )
  })
  
  output$Plot3 <- renderPlot({
    
    plot(time, mydata()$U_max, main = 'Optimal effort through the years (ha)', 
         col = 'darkred', xlab = 'Years', ylab = 'Hectares of palm oil plantations', xlim =c(0,80) )
  })
  
})
