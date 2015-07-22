# Central Limit Theorem: The Age of Coins
#
# marco caserta (marco dot caserta at ie dot edu)
# july 2015

# User Interface

library(shiny)

shinyUI(fluidPage(
  

  # Application title
  titlePanel("Exploring the Central Limit Theorem (CLT): The Age of Coins"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(

      withMathJax(),
      
      fluidRow(
        column(4,
               img(src="IE.jpg", height = 40, width = 40))
        )
      ,
      
      fluidRow(
        column(6, 
               radioButtons("popType", label=h3("Population:"),
                            choices=list("Uniform"="unif",
                                         "Normal "="norm",
                                         "Skewed"="beta",
                                         "Exponential"="expo"),
                            selected="unif")),
        column(6,
               img(src="euro.jpg", height = 180, width = 180))
      ),
      
      hr(),
      checkboxInput("checkbox", label = "Add samples one at a time", value = FALSE),
      br(),
      
      fluidRow(
        column(12,
               sliderInput("sliderN", label = h6("Sample size"),
                           min = 5, max = 100, value = 35)) 
      ),
      
      fluidRow(align="bottom",
               column(6,
                      numericInput("nSamples", label=h6("Nr. of Samples"), value = 1)),
               column(6,
                      actionButton("resample", label = "Sample"),
                      tags$style(type='text/css', "button#resample { margin-top: 38px;}")
               )
      ),
      
      fluidRow(align="center",
               column(6,
                      tableOutput("summary"))
               )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel("",
              plotOutput("plotPop")
    )
  )
))
