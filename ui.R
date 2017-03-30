library(shiny)

# Define UI 
fluidPage(
  
  #  Application title
  titlePanel("Infectious Disease Emergence Economic Analysis (IDEEAL)"),
  
  # Inputs
  sidebarLayout(
    sidebarPanel(
      # Input for total amount of land in analysis
      numericInput('total_land', 'Total Land (in ha):', 100000,
                   min = 1, max = 100000000),
    
      # Input for percentage of forest land
      sliderInput("forest_land", "Forest land:", 
                  min = 0, max = 1, value = 0.99),
      
      # Specification of range within an interval
      sliderInput("palmoil_price", "Palm oil price range (US$):",
                  min = 1, max = 5000, pre = "$",value =1300),
      
      sliderInput('p_c', 'Conversion cost ($US/ha):', 3000,
                   min = 1000, max = 6000, pre = "$"),
      
      sliderInput('p_d', 'Health Damages ($US/ha):', 50,
                  min = 0, max = 2000, pre = "$"),
      
      sliderInput('c_coeff', 'Exponential cost function:', 1.6,
                  min = 1, max = 2),
      
      # Yield of palm oil per hectare (metric tons)
      sliderInput("oil_yield", "Oil yield per hectare (in metric tons):",
                  min=0, max=10, value=4),
      
      sliderInput("rho", "Discount rate:",
                  min=0.01, max=0.09, value=0.03),
      
      # Animation with custom interval (in ms) to control speed,
      # plus looping
      sliderInput("years", "Years:", 1, 100, 1,
                  step = 1, animate=
                    animationOptions(interval=100, loop=TRUE)),
      
      submitButton("Update"),
      
      br(),
      
      h5("Created by:"),
      tags$a("EcoHealth Alliance", 
             href="http://www.ecohealthalliance.org"),
      h5("For details on how model is generated"),
      tags$a("Blog Post", 
             href=""),
      h5(textOutput("counter"))
  
    ),     
      
      mainPanel(
        img(src = "img1.png", height = 150, width = 300),
        plotOutput("Plot1"),
        plotOutput("Plot2"),
        plotOutput("Plot3")
      
    )
  )
)
