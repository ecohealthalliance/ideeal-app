library(shiny)
library(optimx)

# Simulation and Shiny Application of Optimization
shinyServer(function(input, output) {
  
  time = seq(0,80) # Set time periods
  theta = 1 # effectiveness of land conversion
  d_coeff = 2
  b_coeff = 1
  mydata <- reactive({
    
    total_land    = input$total_land 
    forest_land   = input$forest_land
    palmoil_price = input$palmoil_price
    p_c           = input$p_c            # Marginal cost per ha of forest converted to palm oil 
    p_b           = input$p_b
    c_coeff       = input$c_coeff # Coefficient for C(u) curve
    p_d           = input$p_d            
    oil_yield     = input$oil_yield
    rho           = input$rho # discount rate
    years         = input$years
    
    # Set parameters
    py = palmoil_price*oil_yield
    prop_forest_land = forest_land # proportion of forest land from total (%)
    prop_developed_land =  1 - prop_forest_land # proportion of developed land from total (%)

    Uo = 0.001 # Initial effort of land conversion at time 0
    Xo = prop_developed_land  # Initial proportion of total landscape developed at time 0
    X_land0 = prop_developed_land*total_land
    Ro = palmoil_price*Xo # Initial revenue at time 0
    Bo = p_b*(1-Xo)^b_coeff
    Co = p_c*Uo^c_coeff    # Initial cost at time 0
    Do = p_d*Xo^d_coeff    # Initial cost at time 0
    Vo = Ro - Co - Do
    
    # Vectors for function
    U <- rep(Uo, length(time))                 # vector for effort in land conversion
    X <- rep(Xo,length(time))                  # vector for stock of developed land X(t)
    X_land <- rep(X_land0,length(time))  
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
   
      
    # Function to be optimized
    opt_V <- function (U, npar = 1, print = TRUE) {
      for (i in 2:length(time)){
        X[i] = X[i-1] + theta*U[i]*(1 - X[i])  # change in developed land
        R[i] = py*X[i]                                # Revenue function for developed land
        B[i] = (p_b*(1-X[i]))^b_coeff    
        C[i] = (p_c*U[i])^c_coeff                    # Cost function using effort to develop land
        D[i] = (p_d*X[i])^d_coeff 
        V[i] = R[i] + B[i] - C[i] - D[i]                  # Value function
        V[length(time)] = ((R[length(time)] + B[length(time)] - C[length(time)] - D[length(time)] )*exp(-rho *(time[length(time)]-1)))/rho
      }
      V_dis = V*exp(-rho * time) # Discounted value function
      result <- sum(V_dis)
      return(result)
    }
    
    withProgress(message = 'Optimizing',{ 
      
    # Optimize system by choosing effort
    max_V <- optimx(par = U, fn = opt_V, lower = 0, upper = 1,
                    method = ("L-BFGS-B"),
                    control = list(maximize = TRUE, trace=6), hessian = FALSE)
    U_max <- coef(max_V)
    #plot(time, U_max)
    
    for (i in 2:length(time)){
      X[i] = X[i-1] + theta*U_max[i]*(1 - X[i])  # change in developed land
    }
    for (i in 2:length(time)){
      X_land[i] = X[i]*total_land  # change in developed land
    }
    
  
    return(list(X=X))
           list(X_land=X_land)
    list( U_max=U_max)
    
  })
  
  }) 
    
  output$Plot1 <- renderPlot({
   
     withProgress(message = 'Making plot', value = 0, {
    
    plot(time, mydata()$X, main = 'Optimal allocation of development through the years (%)', 
         col = 'darkgreen', xlab = 'Years', ylab = 'Development (%)', xlim =c(0,max(time)) , ylim =c(0,1) )
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
