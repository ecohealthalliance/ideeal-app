### IDEEAL shiny app ###
## Create by: EcoHealth Alliance
# Date: 10 November 2017


### Things to do:
#### include a boton that allows to choose to include or exclude ecosystem services, infectious disease, or/and haze
###

## app.R ##
library(shiny)
library(shinydashboard)
library(optimx)
library(ggplot2)
library(ggthemes)
# library(devtools)
# install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(plotly)
library(shinyWidgets)

my_names = c("private", "social")

ui <- dashboardPage(skin = "green",
                    header <- dashboardHeader(title = "IDEEAL", dropdownMenuOutput("messageMenu")
                                              ),
                    sidebar <- dashboardSidebar(width = 300,
                                                sidebarMenu(
                                                  menuItem("Overview", tabName = "Picture", icon = icon("map-o")),
                                                  menuItem("Project Details", tabName = "proj_bg", icon = icon("book")),
                                                  menuItem("How to use this app?", tabName = "app_use", icon = icon("info")),
                                                  menuItem("Private vs Social Optimal", tabName = "figure3", icon = icon("shower"),  badgeLabel = "results", badgeColor = "green"),
                                                  br(),
                                                  menuItem("Model Input Variables", tabName = "var_tab", icon = icon("globe"), 
                                                           menuSubItem("Detailed Variable Control", tabName = "var_input", icon = icon("industry")),
                                                           menuSubItem("Variable Appendix", tabName = "var_appendix", icon = icon("globe"))
                                                  ),
                                                  
                                                  # br(),
                                                  #menuItem(submitButton("update"), badgeLabel = "click 'update' after input changes", badgeColor = "green"),
                                                  # br(),
                                                  # menuItem("Social Optimal", tabName = "figure1", icon = icon("shower"),  badgeLabel = "results", badgeColor = "green"),
                                                  # menuItem("Private Optimal", tabName = "figure2", icon = icon("shower"),  badgeLabel = "results", badgeColor = "green"),
                                                  menuItem("Option Value", tabName = "option_value", icon = icon("shower"),  badgeLabel = "results", badgeColor = "green"),
                                                  br(),
                                                  
                                                  h5("Created by:"),
                                                  tags$a("EcoHealth Alliance", 
                                                         href="http://www.ecohealthalliance.org"),
                                                  h5("For details on how the model is generated go to:"),
                                                  tags$a("IDEEAL modeling at EcoHealth alliance", 
                                                         href="http://www.ecohealthalliance.org/program/ideeal"),
                                                  h5(textOutput("counter")),
                                                  
                                                  br(),
                                                  
                                                  menuItem("IDEEAL Webpage", icon = icon(""), 
                                                           href = "https://www.ecohealthalliance.org/program/ideeal"),
                                                  menuItem("EcoHealth Alliance", icon = icon(""), 
                                                           href = "https://ecohealthalliance.org")
                                                )
                    ),

# Dashboard Body ----------------------------------------------------------
                    body <- dashboardBody(
                      shinyDashboardThemes(theme = "poor_mans_flatly"),
                      tabItems(
# Landing Page tab --------------------------------------------------------
                        tabItem(tabName = "Picture",
                                h2("Infectious Disease Emergence and Economics of Altered Landscapes (IDEEAL)"),
                                fluidRow(
                                  HTML('<iframe width="640" height="360" src="https://www.youtube.com/embed/asERnZ6byh8?rel=0&amp;controls=0&amp;showinfo=0;autoplay=1&amp;mute=1" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
                                  
                                  box(img(src = "img1.png", height = 400, width = 600), width=12)
                                ),
                                fluidRow(  
                                  box(includeMarkdown('landing_page.MD'), width=12)) 
                        ),
# Background tab -------------------------------------------------------------------
                        tabItem(tabName = "proj_bg", 
                                h2("Infectious Disease Emergence and Economics of Altered Landscapes (IDEEAL)"),
                                # fluidRow(  
                                box(includeMarkdown('background.MD'), width=12)
                                # box("Project Background: Please choose the values in each tab and then click update to estimate the model. 
                                #      If no values are chosen, the model will run with the default values", width=9  )
                                # Add section: why palm oil? (slide 13-16)
                                # )
                        ),
# How to tab -------------------------------------------------------------------
                        tabItem(tabName = "app_use", 
                                box(includeMarkdown('how_to_use.MD'), width=12)
                        ),
# Variable Appendix tab -------------------------------------------------------------------
                        tabItem(tabName = "var_appendix", 
                                box(includeMarkdown('var_appendix.MD'), width=12)
                        ),
# Detailed Variable Control tab -------------------------------------------------------------------
                        # Second tab content
                        tabItem(tabName = "var_input",
                                fluidRow(
                                  tabBox(
                                    width = 12,
# Land Use tabPanel -------------------------------------------------------
                                    tabPanel("Land Use", 
                                    # Input for total amount of land in analysis
                                    numericInput('total_land', 'Total Land (in ha):', 
                                                 min = 1, max = 10000000000, value = 7363000 ),
                                    helpText("Total land is the sum of pristine hectares and development hectares "),
                                    # Input for percentage of forest land
                                    sliderInput("forest_land", "Forest land (%):",
                                                min = 0, max = 1, value = 0.68),
                                    # Input for palm oil price (US$)
                                    sliderInput("kerneloil_price", "Kernel price (US$):",
                                                min = 0, max = 1500, pre = "$",value =1300),
                                    # Yield of Kernel per hectare (metric tons)
                                    sliderInput("kerneloil_yield", "Kernel yield per hectare (in metric tons):", 
                                                min=0, max=5.01, value=1.04), 
                                    helpText("The kernel oil yield is the amount of kernel oil in metric tons produced 
              from one hectare of land (default: FAOSTAT(2014) for Malaysia. It is approximately 5% of FFB)"),
                                    sliderInput("prop_CPO_total", "Proportion CPO from total production:", 
                                                min=0, max=1, value=0.8)
                                    ),
# Land Holder % tabPanel -------------------------------------------------------                                    
                                    tabPanel("Land Holder %",
                                             # "Yield Land holders"
                                             fluidRow(
                                             column(width = 6,
                                             sliderInput('small_landholders', 'Proportion of small landholders (%):', 
                                                         min = 0, max = 1, value=0.41, pre = "", step = .01),
                                             uiOutput('large_landholders'),
                                             # sliderInput('large_landholders', 'Proportion of large landholders (%):', 
                                             #             min = 0, max = 1, value=0.49, pre = ""),
                                             strong(textOutput("gov_landholders")),
                                             # sliderInput('gov_landholders', 'Proportion of goverment plantations (%):', 
                                             #             min = 0, max = 1, value=0.10, pre = ""),
                                             helpText("The sum of the proportions must be equal to 1 (100%). \n The default numbers come from Suharto (2009) for Indonesia")
                                             ),
                                             column(width = 6,
                                             numericInput('yield_small_lanholders', 'Yield small landholders (tonnes/ha):', 
                                                          min = 0, max = 5.0, value=3), 
                                             numericInput('yield_large_lanholders', 'Yield large landholders (tonnes/ha):', 
                                                          min = 0, max = 5.0, value=4.2, step = 0.1),
                                             numericInput('yield_gov_lanholders', 'Yield goverment plantations (tonnes/ha):', 
                                                          min = 0, max = 5.0, value=4),
                                             helpText("The yields come from Suharto (2009) for Indonesia")
                                             )
                                             )
                                             ),

# Ecosystem Service -------------------------------------------------------
                                    tabPanel("Ecosystem Service",
                                             fluidRow(
                                             column(width = 6,
                                             sliderInput('food', 'Provision of FOOD value ($US/ha):', 15,
                                                         min = 0, max = 200, pre = "$"), 
                                             sliderInput('water', 'Provision of clean WATER value ($US/ha):', 28,
                                                         min = 8, max = 46, pre = "$"), 
                                             sliderInput('raw_materials', 'Provision of RAW MATERIALS value ($US/ha):', 31,
                                                         min = 0, max = 84, pre = "$"), 
                                             sliderInput('genetic', 'Provision of GENETIC MATERIALS value ($US/ha):', 13,
                                                         min = 0, max = 13, pre = "$"), 
                                             sliderInput('medical', 'Provision of MEDICINE MATERIALS value ($US/ha):', 0,
                                                         min = 0, max = 1504, pre = "$"), 
                                             sliderInput('air_quality', 'Provision of CLEAN AIR value ($US/ha):', 12,
                                                         min = 0, max = 12, pre = "$"), 
                                             sliderInput('climate', 'Provision of CLIMATE (carbon sequestration) value ($US/ha):', 261,
                                                         min = 4, max = 2044, pre = "$"), 
                                             sliderInput('extreme_events', 'Provision of PROTECTION AGAINST EXTREME EVENTS value ($US/ha):', 14,
                                                         min = 1, max = 66, pre = "$"), 
                                             sliderInput('water_flow', 'Provision of FLOW OF WATER value ($US/ha):', 342,
                                                         min = 2, max = 342, pre = "$")
                                             ),
                                             column(width = 6,
                                             sliderInput('waste', 'Provision of WASTE REMOVAL value ($US/ha):', 8,
                                                         min = 6, max = 10, pre = "$"), 
                                             sliderInput('erosion', 'Provision of EROSION CONTROL value ($US/ha):', 13,
                                                         min = 4, max = 15, pre = "$"),
                                             sliderInput('soil_fertility', 'Provision of SOIL FERTILITY value ($US/ha):', 2,
                                                         min = 2, max = 7, pre = "$"),
                                             sliderInput('pollination', 'Provision of POLLINATION value ($US/ha):', 30,
                                                         min = 6, max = 53, pre = "$"), 
                                             sliderInput('biocontrol', 'Provision of BIOCONTROL value ($US/ha):', 1,
                                                         min = 0, max = 11, pre = "$"), 
                                             sliderInput('nursery', 'Provision of NURSERY value ($US/ha):', 16,
                                                         min = 0, max = 16, pre = "$"), 
                                             sliderInput('genepool', 'Provision of GENEPOOL value ($US/ha):', 12,
                                                         min = 0, max = 23, pre = "$"), 
                                             sliderInput('recreation', 'Provision of recreation activities value ($US/ha):', 45,
                                                         min = 0, max = 867, pre = "$"), 
                                             sliderInput('population', 'Total population in the region:', 3.55e6,
                                                         min = 0, max = 1e7, pre = "")
                                             )
                                             )
                                             ),
# Placeholder tabPanel ----------------------------------------------------
                                    tabPanel("Other variables",
                                             sliderInput("rho", "Discount rate:",
                                                         min=0.0, max=0.1, value=0.05),
                                             helpText("The discount rate is used to discount the values to the present. 
                                                           Every time a piece of land is converted into palm plantation in the future, 
                                                           it generates revenues that need to be discounted. 
                                                           Future ecosystem services and costs are also discounted by this rate.", 
                                                      br(),
                                                      "It is by default at 5%, if you increase the discount rate, 
                                                           future value flows will become smaller"),
                                             br(),
                                             #land conversion
                                             sliderInput('cost_per_HA', 'Land conversion costs ($US/ha):', 
                                                         min = 100, max = 3000, value=1410, pre = "$")
                                    )
                                  )
                            )
                                # 
                                # fluidRow(  )
                        ),

# Option Value tab --------------------------------------------------------
                        tabItem(tabName = "option_value",
                                h2("Option Value"),
                                br(),
                                h2("Given uncertainty in future prices, the option value estimates 
                 whether it is optimal to develop land or conserve"),
                                h2("In construction...")
                        ),
# Result Plot tab ---------------------------------------------------------
                        tabItem(tabName = "figure3",

# Key variable box --------------------------------------------------------
                                fluidRow(
                                  box(title = "Key variable", status = "success", width = 12, collapsible = TRUE,
                                      fluidRow(
                                        column(width = 6,
                                      
                                    sliderInput("CPO_price", "Core Palm Oil (CPO) international price (US$):",
                                                min = 0, max = 1500, pre = "$",value =517),
                                    # Yield of CPO  per hectare (metric tons)
                                    sliderInput("CPO_yield", "CPO yield per hectare (in metric tons):",
                                                min=0, max=5.01, value=4.19),
                                    helpText("The core palm oil yield is the amount of palm oil in metric tons produced
                                             from one hectare of land (default: FAOSTAT(2014) for Malaysia. It is approximately 20% of FFB)")
                                        ),
                                        column(width = 6, 
                                    sliderInput('expenditures', 'Total expenditures on prevention and control in the region (US$):', 9e6,
                                                  min = 0, max = 1e8, pre = "$"),
                                    
                                    sliderInput('infections', 'Total number of infections:', 2000,
                                                  min = 0, max = 1e6, pre = ""),
                                    h2(textOutput("text8")), 
                                    helpText("Increasing the value of ecosystem services will reduce the land conversion 
                  for the social optimal")
                                        )
                                      )
                                  )
                                ),

# Key output plot ---------------------------------------------------------
                                fluidRow(
                                  box(title = "Key result: Private vs Social Optimal", width = 12, status = "primary",
                                      # checkboxGroupInput("checkGroup", label = h4("Results to show"), 
                                      #                    choices = list("Private" = "private", "Social" = "social"), 
                                      #                    selected = c('private', 'social')),

                                      prettyCheckbox(inputId = "private_check",  label = "Private",
                                                     status = "primary",
                                                     value = TRUE, fill = TRUE), #outline = TRUE
                                      
                                      prettyCheckbox(inputId = "social_check",  label = "Social",
                                                     status = "success",
                                                     value = TRUE, fill = TRUE),
                                      
                                      plotlyOutput("plotly4")
                                    # plotOutput("plot4")
                                  )
                                ),
                                
                                br(),
                                fluidRow(
                                  box(title = "Raw data output", status = "primary", width = 12,
                                      h4("Net Persent Value for Social:"),
                                      textOutput("text4"),
                                      h4("Net Persent Value for Private:"),
                                      textOutput("text5")
                                  )
                                )
                        )

                        
                        
                      )
                    )
)

# server ------------------------------------------------------------------
server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  time <- seq(0,50) # Set time periods
  time2 = seq(1,81,1)
  
  ################################ MYDATA0 (Ecosystem Services Values) ######################################  
  mydata0 <- reactive({
    food = input$food
    water = input$water
    raw_materials = input$raw_materials
    genetic = input$genetic
    medical = input$medical
    air_quality = input$air_quality
    climate = input$climate
    extreme_events = input$extreme_events
    water_flow = input$water_flow
    waste = input$waste
    erosion = input$erosion
    soil_fertility = input$soil_fertility
    pollination = input$pollination
    biocontrol = input$biocontrol
    nursery = input$nursery
    genepool = input$genepool
    recreation = input$recreation
    ES_value = food + water + raw_materials + genetic + medical + air_quality + climate + extreme_events + water_flow + waste +  erosion + soil_fertility + pollination + biocontrol + nursery + genepool + recreation
    
    return(list(ES_value=ES_value))
  })
  ################################ END of MYDATA0 ######################################  
  
  
  ################################ MYDATA ######################################  
  mydata <- reactive({
    
    #### INPUTS ####
    total_land      = input$total_land  # total land 
    forest_land     = input$forest_land # total forest land 
    cost_per_HA     = input$cost_per_HA 
    ES_value        = mydata0()$ES_value
    CPO_price       = input$CPO_price
    CPO_yield       = input$CPO_yield
    kerneloil_price = input$kerneloil_price
    kerneloil_yield = input$kerneloil_yield
    rho             = input$rho
    expenditures    = input$expenditures
    infections      = input$infections
    population      = input$population
    prop_CPO_total  = input$prop_CPO_total
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
    r = 0.02 # growth rate of ecosystem 
    
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
    prop_kernel_total = 1 - prop_CPO_total 
    
    #  Calibration parameters 
    Uo = 0.00000001 # Initial effort of land conversion at time 0
    Xo =  prop_developed_land*100# Initial proportion of total landscape developed at time 0
    FXo = r/(1+Xo) # Initial value for function of 
    So = (Xo/100)^2/n1
    Zo = So 
    Ro = CPOprice*Xo + kernelprice*Xo # Initial revenue at time 0
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
    
    X_social = X
    X_social2 = rep(X_social[51], 30)
    X_social3 = c(X_social,X_social2)
    
    
    # Function to be optimized
    opt_W <- function (U, npar = 1) {
      for (i in 1:(length(time)-1)){
        X[i+1] = X[i] + theta*U[i]*(100-X[i]) - r/(1+X[i])  
        S[i] = (X[i]/100)^2/n1
        Z[i+1] = (1- omega)*Z[i] + S[i]
        R[i] = CPOprice*X[i] + kernelprice*X[i]         # Revenue function for developed land
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
      R[i] = CPOprice*X[i] + kernelprice*X[i]        # Revenue function for developed land
      C[i] = g1*U[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
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
    
    return(list(X=X,
                U_max=U_max,  
                NPV_W_social=NPV_W_social,
                NPV_profits_social=NPV_profits_social
    ) 
    )  
  }) 
  ############ End of mydata <- reactive({ ##########################
  
  ################################ MYDATA2 ######################################
  mydata2 <- reactive({
    
    #### INPUTS ####
    total_land      = input$total_land  # total land 
    forest_land     = input$forest_land # total forest land 
    cost_per_HA     = input$cost_per_HA 
    ES_value        = mydata0()$ES_value
    CPO_price       = input$CPO_price
    CPO_yield       = input$CPO_yield
    kerneloil_price = input$kerneloil_price
    kerneloil_yield = input$kerneloil_yield
    rho             = input$rho
    expenditures    = input$expenditures
    infections      = input$infections
    population      = input$population
    prop_CPO_total  = input$prop_CPO_total
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
    Ro = CPOprice*Xo + kernelprice*Xo # Initial revenue at time 0
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
    opt_W_P <- function (U, npar = 1) {
      for (i in 1:(length(time)-1)){
        X[i+1] = X[i] + theta*U[i]*(100-X[i]) - r/(1+X[i])  
        S[i] = (X[i]/100)^2/n1
        Z[i+1] = (1- omega)*Z[i] + S[i]
        R[i] = CPOprice*X[i] + kernelprice*X[i]         # Revenue function for developed land
        C[i] = g1*U[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
        profits[i] = R[i] - C[i]
        disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
        B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
        disc_B[i] = B[i]*exp(-rho*(i-1)) 
        #D[i] = d1*Z[i]^d2
        D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
        disc_D[i] = D[i]*exp(-rho*(i-1))
        W[i] = profits[i] 
        W[length(time)] = ((profits[length(time)]  )*exp(-rho *(time[length(time)]-1)))/rho
      }
      W_dis = W*exp(-rho*time) # Discounted value function
      NPV_W <- sum(W_dis)
      return(NPV_W)
    }
    
    max_W_P <- optimx(par = U, fn = opt_W_P, lower = -1, upper = 1,
                      method = ("L-BFGS-B"),
                      control = list(maximize = TRUE, trace=6))
    
    U_max_P <- t(data.frame(coef(max_W_P)))
    
    ## Estimate optimal stock of land each period (X)
    for (i in 1:(length(time)-1)){
      X[i+1] = X[i] + theta*U_max_P[i]*(100-X[i]) - r/(1+X[i])  
      S[i] = (X[i]/100)^2/n1
      Z[i+1] = (1- omega)*Z[i] + S[i]
      R[i] = CPOprice*X[i] + kernelprice*X[i]        # Revenue function for developed land
      C[i] = g1*U[i]^C_coeff + g2*X[i]         # Cost function using effort to develop land
      profits[i] = R[i] - C[i]
      disc_profits[i] = profits[i]*exp(-rho*(i-1)) 
      B[i] = ESTV*((exp(b) - exp(b*(X[i]/100)))/(exp(b)-1))
      disc_B[i] = B[i]*exp(-rho*(i-1)) 
      disc_B[length(time)] = (B[length(time)]*exp(-rho *(time[length(time)]-1)))/rho 
      #D[i] = d1*Z[i]^d2 # simple damage function
      D[i] = d3*(exp(d4 * ((Z[i]*n1)^(1/2) - d5)) + d6)**(-1)
      disc_D[i] = D[i]*exp(-rho*(i-1))
      disc_D[length(time)] = (D[length(time)]*exp(-rho *(time[length(time)]-1)))/rho
      W[i] = profits[i] 
      W[length(time)] = ((profits[length(time)])*exp(-rho *(time[length(time)]-1)))/rho
    }
    W_dis = W*exp(-rho*time) # Discounted value function
    NPV_W_private <- sum(W_dis)/1e9
    NPV_profits_private <- sum(disc_profits)/1e9
    NPV_ES_private <- sum(disc_B)/1e9
    NPV_D_private <- sum(disc_D)/1e9
    NPV_W_private2 <- NPV_W_private + NPV_ES_private - NPV_D_private
    
    return(list(X=X,
                U_max_P=U_max_P, 
                NPV_W_private=NPV_W_private2, 
                NPV_profits_private=NPV_profits_private
    ) 
    )  
  }) 
  ######################### End of mydata2 <- reactive({ #############################  
  
  #### Plot 1 #######
  
  output$plot1 <- renderPlot({
    
    plot(mydata()$time2, mydata()$X_social3  )
    
  })
  
  #### Plot 2  ######
  
  output$plot2 <- renderPlot({ 
    
    X_social = mydata()$X
    X_social2 = rep(X_social[51], 30)
    X_social3 = c(X_social,X_social2)
    
    withProgress(message = 'Making plot', value = 0, {
      
      ggplot() + 
        geom_line(aes(time2, X_social3, color=X_social3), size=2) +
        xlim(0, 90) +
        ylim(0, 100) +
        labs(x = "Time" , y = "Optimal proportion of land converted to palm oil (%)", title = "Land Conversion Path - Social Optimal" ) +
        scale_fill_tableau() +
        theme_minimal() +
        theme(legend.position="none", plot.title = element_text(face="bold", size = 15))
      
    })
  })
  
  #### Plot 3 #####
  output$plot3 <- renderPlot({
    
    X_private = mydata2()$X
    X_private2 = rep(X_private[51], 30)
    X_private3 = c(X_private,X_private2)
    
    withProgress(message = 'Making plot', value = 0, {
      
      ggplot() + 
        geom_line(aes(time2, X_private3, color=X_private3), size=2) +
        xlim(0, 90) +
        ylim(0, 100) +
        labs( x = "Time" , y = "Optimal proportion of land converted to palm oil (%)", title = "Land Conversion Path - Private Optimal" ) +
        scale_fill_tableau() +
        theme_minimal() +
        theme(legend.position="none", plot.title = element_text(face="bold", size = 15))
    })
  })
  
  
  #### Plot 4  - private vs social #######
  # print(mydata()$X)
  output$plot4 <- renderPlot({
  
    X_social = mydata()$X
    X_social2 = rep(X_social[51], 30)
    X_social3 = c(X_social,X_social2)
    
    X_private = mydata2()$X
    X_private2 = rep(X_private[51], 30)
    X_private3 = c(X_private,X_private2)
    
    withProgress(message = 'Making plot', value = 0, {
      
    p <- ggplot() + 
        xlim(0, 90) +
        ylim(0, 100) +
        labs(x = "Time (Years)" , y = "Optimal proportion of land converted to palm oil (%)") + #, title = "Private vs Social Optimal")
        theme_minimal() +
        theme(plot.title = element_text(face="bold", size = 25), # 
              legend.position="top",
              legend.text = element_text(size = 14)) #+ 
    if(input$private_check == TRUE & input$social_check == FALSE){
      p <- p + 
        geom_line(aes(time2, X_private3), size=2, col = "#377EB8") +
        scale_color_manual(name = NULL, labels = c("Private ")) +
        annotate("text", label = paste0(round(X_private2, 1), "%"), x = 85, y = X_private2)
    } else if(input$private_check == FALSE & input$social_check == TRUE){
      p <- p + 
        geom_line(aes(time2, X_social3), size=2, col = "#4DAF4A") +
        scale_color_manual(name = NULL, labels = c("Social ")) +
        annotate("text", label = paste0(round(X_social2, 1), "%"), x = 85, y = X_social2)
    } else {
      p <- p +
        geom_line(aes(time2, X_private3), size=2, col = "#377EB8") +
        geom_line(aes(time2, X_social3), size=2, col = "#4DAF4A") +
        scale_color_manual(name = NULL, labels = c("Private ", "Social ")) +
        annotate("text", label = paste0(round(X_private2, 1),  "%"), x = 85, y = X_private2) +
        annotate("text", label = paste0(round(X_social2, 1),  "%"), x = 85, y = X_social2)
    }
    
    
    # if(length(input$checkGroup) == 1){ # to ensure first line is always blue
    #   if(input$checkGroup == "private"){
    #     p <- p + 
    #       geom_line(aes(time2, X_private3), size=2, col = "#377EB8") +
    #       scale_color_manual(name = NULL, labels = c("Private ")) +
    #       annotate("text", label = paste0(round(X_private2, 1), "%"), x = 85, y = X_private2)
    #   } else { # social
    #     p <- p + 
    #       geom_line(aes(time2, X_social3), size=2, col = "#377EB8") +
    #       scale_color_manual(name = NULL, labels = c("Social ")) +
    #       annotate("text", label = paste0(round(X_social2, 1), "%"), x = 85, y = X_social2)
    #   } 
    # }
    # if(length(input$checkGroup) == 2){
    #   p <- p +
    #     geom_line(aes(time2, X_private3), size=2, col = "#377EB8") +
    #     geom_line(aes(time2, X_social3), size=2, col = "#4DAF4A") +
    #     scale_color_manual(name = NULL, labels = c("Private ", "Social ")) +
    #     annotate("text", label = paste0(round(X_private2, 1),  "%"), x = 85, y = X_private2) +
    #     annotate("text", label = paste0(round(X_social2, 1),  "%"), x = 85, y = X_social2)
    # }
        
    print(p)
    #+ 
      # theme(plot.background = element_rect(fill = "transparent", color = NA))
      # theme(
      #   panel.background = element_rect(fill = "transparent") # bg of the panel
      #   , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
      #   , panel.grid.major = element_blank() # get rid of major grid
      #   , panel.grid.minor = element_blank() # get rid of minor grid
      #   , legend.background = element_rect(fill = "transparent") # get rid of legend bg
      #   , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      # )
    })
  })
  
  output$plotly4 <- renderPlotly({
    
    X_social = mydata()$X
    X_social2 = rep(X_social[51], 30)
    X_social3 = c(X_social,X_social2)
    
    X_private = mydata2()$X
    X_private2 = rep(X_private[51], 30)
    X_private3 = c(X_private,X_private2)
    
    withProgress(message = 'Making plot', value = 0, {
      
      p <- ggplot() + 
        xlim(0, 90) +
        ylim(0, 100) +
        labs(x = "Time (Years)" , y = "Optimal proportion of land converted to palm oil (%)") + #, title = "Private vs Social Optimal")
        theme_minimal() +
        theme(plot.title = element_text(face="bold", size = 25), # 
              legend.position="top",
              legend.text = element_text(size = 14)) #+ 
      if(isTRUE(input$private_check) & !isTRUE(input$social_check)){
        p <- p + 
          geom_line(aes(time2, X_private3), size=2, col = "#377EB8") +
          scale_color_manual(name = NULL, labels = c("Private ")) +
          annotate("text", label = paste0(round(X_private2, 1), "%"), x = 85, y = X_private2)
      } 
      if(!isTRUE(input$private_check) & isTRUE(input$social_check)){
        p <- p + 
          geom_line(aes(time2, X_social3), size=2, col = "#4DAF4A") +
          scale_color_manual(name = NULL, labels = c("Social ")) +
          annotate("text", label = paste0(round(X_social2, 1), "%"), x = 85, y = X_social2)
      } 
      if (isTRUE(input$private_check) & isTRUE(input$social_check)){
        p <- p +
          geom_line(aes(time2, X_private3), size=2, col = "#377EB8") +
          geom_line(aes(time2, X_social3), size=2, col = "#4DAF4A") +
          scale_color_manual(name = NULL, labels = c("Private ", "Social ")) +
          annotate("text", label = paste0(round(X_private2, 1),  "%"), x = 85, y = X_private2) +
          annotate("text", label = paste0(round(X_social2, 1),  "%"), x = 85, y = X_social2)
      }
      ggplotly(p)

    })
  })
  
  #####   
  output$large_landholders <- renderUI({
  sliderInput('large_landholders', 'Proportion of large landholders (%):', 
                min = 0, max = (1 - input$small_landholders), value = 0.49, pre = "", step = .01)
  })
  output$gov_landholders <- renderText({
    twoinputsum = input$large_landholders + input$small_landholders
    paste("Proportion of goverment plantations (%):", round(1 - twoinputsum, digits = 2))
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
  
  output$text1 <- renderText({ 
    paste(round(mydata()$X, digits=1)) 
  })
  
  output$text2 <- renderText({ 
    paste(round(mydata2()$X, digits=1)) 
  })
  #### Difference in Effort between private and social optimums ####  
  output$text3 <- renderText({ 
    paste(round( mydata2()$U_max_P - mydata()$U_max, digits=1)) 
  })
  
  output$text4 <- renderText({ 
    paste(round( mydata()$NPV_W_social, digits=1)) 
  })
  
  output$text5 <- renderText({ 
    paste(round( mydata2()$NPV_W_private, digits=1)) 
  })
  
  output$text6 <- renderText({ 
    paste(round( mydata2()$NPV_profits_private, digits=1)) 
  })
  
  output$text7 <- renderText({ 
    paste(round( mydata()$NPV_profits_social, digits=1)) 
  })
  
  output$text8 <- renderText({ 
    paste("Sum Total Ecosystem Services Value ($US/ha):", round( mydata0()$ES_value, digits=1)) 
  })
  
}

shinyApp(ui, server)

