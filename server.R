# Central Limit Theorem: The Age of Coins
#
# marco caserta (marco dot caserta at ie dot edu)
# july 2015

# This app implements a simulator for the Central Limit Theorem (CLT) based on
# the example of the age of euro coins.

# The Age of Coins example is a classical classroom example to show the main 
# findings behind the CLT. We want to estimate the average age of euro coins and
# we assume that four types of populalations can be hypothesized to model the 
# coins production volume. The four populations are: Uniform, Normal, Right-skewed,
# and Exponentional. Each one of them models a different set of assumptions behing
# the production of euro coins.

# The user can define the sample size, and the number of samples, to obtain the
# sampling distribution for the sample means (xbar). The user should be able to
# verify that, if the assumptions of the CLT are satisfied, the sample mean follows
# a normal distribution with mean equal to the population mean and standard deviation
# inversely proportional to the sample size.

# Some variables have been hard-coded here, which limits the scope of the app 
# (but simply focuses on the coins example.)
# For example, below here we define the population size, and the starting and 
# ending periods of the simulation (xmin and xmax). We also hard-fixed the two 
# parameters of the beta distribution (beta1 and beta2), to obtain a right-skewed
# distribution. 

library(shiny)
library(gridExtra)
library(ggplot2)

fromPopulation <- TRUE # define whether we want to sample from the population
# or from the theoretical distribution behind the population
popSize <- 10000
xmin <- 1999
xmax <- 2015
mu <- xmin + (xmax-xmin)/2
range <- xmax - xmin
stdev <- range/6
beta1 <- 2
beta2 <- 5
expo1 <- 1/2

shinyServer(function(input, output) { 
  actionType <- reactiveValues()
  actionType$lastAction <- "no"
  
  p       <- NULL # POPULATION
  meanPop <- NULL # mean of the population  
  means   <- NULL # vector of means (for the sampling distribution)
  
  # reactive function. We clear up the memory when the checkbox is used
  doReset <- reactive({
    
    input$checkbox
    means <<- NULL
  })
  
  # this function reacts whenever the population type is changed
  population <- reactive({
    rdist <- switch(input$popType,
                    unif = runif,
                    norm = rnorm,
                    beta = rbeta,
                    expo = rexp,
                    runif)
    
    pop <- switch(input$popType,
                  unif = rdist(popSize, xmin, xmax),
                  norm = rnorm(popSize, mu, stdev),
                  beta = xmin + range*rbeta(popSize, beta1, beta2),
                  expo = xmin + rexp(popSize,expo1)
    )
    pop
  })
  
  # this function allows to sample in two ways:
  # 1. from the theoretical distribution of the population
  # 2. from the population itself (we take a sample from the observations composing
  # the population itself ).
  # To activate one of the options, change the variable fromPopulation (TRUE/FALSE)
  sampling <- eventReactive(input$resample, {
    n <- input$sliderN
    nSamples <- input$nSamples
    
    if (fromPopulation)
    {
      if (input$checkbox)
      {
        x <- sample(p, n)  # SAMPLING from population (rather than from distribution)
        pos <- length(means) + 1
        means[pos] <<- mean(x)          
      }
      else
      {
        means <<- 1:nSamples
        for (i in 1:nSamples)
        {
          x <- sample(p, n)  # SAMPLING from population (rather than from distribution)
          means[i] <<- mean(x)
        }
      }
    }
    else # we sample from the theoretical distribution behind the population
    {
      v1 <- switch(input$popType,
                   unif = xmin,
                   norm = mu,
                   beta = beta1,
                   expo = 1
      )
      v2 <- switch(input$popType,
                   unif = xmax,
                   norm = stdev,
                   beta = beta2,
                   expo = FALSE
      )
      rdist <- switch(input$popType,
                      unif = runif,
                      norm = rnorm,
                      beta = rbeta,
                      expo = rexp,
                      runif)
      if (input$checkbox)
      {
        if (input$popType == "expo")
          x <- xmin + rexp(n,expo1)
        else
        {
          x <- rdist(n, v1, v2)
          if (input$popType == "beta")
            x <- xmin + range*x
        } 
        pos <- length(means) + 1
        means[pos] <<- mean(x)
      }
      else
      {
        means <<- 1:nSamples
        for (i in 1:nSamples)
        {
          if (input$popType == "expo")
            x <- xmin + rexp(n,expo1)
          else
          {
            x <- rdist(n, v1, v2)
            if (input$popType == "beta")
              x <- xmin + range*x
          }
          means[i] <<- mean(x)
        }
      }
    }
    x
  })
  
  # redraw a population when the "resample" button has been pressed
  # but only if the type of population has been changed
  population_def <- eventReactive(input$resample,{
    v1 <- switch(input$popType,
                 unif = xmin,
                 norm = mu,
                 beta = beta1
    )
    v2 <- switch(input$popType,
                 unif = xmax,
                 norm = stdev,
                 beta = beta2
    )
    
    ddist <- switch(input$popType,
                    unif = dunif,
                    norm = dnorm,
                    beta = dbeta) 
    type <- switch(input$popType,
                   unif = "Uniform",
                   norm = "Normal",
                   beta = "Skewed")  
    p <<- population() # get population (a new one is created only if needed)
    meanPop <<- mean(p) # population mean
   
    # create chart for the POPULATION
    msg <- paste("Population Distribution: ", type)
    q0 <- qplot(p , geom="blank", xlim=c(xmin, xmax), main=msg, xlab="", ylab="")
    
    if (input$popType == "unif" || input$popType == "norm")
      q0 <- q0 + stat_function(fun=ddist, arg=list(v1, v2), geom="ribbon", 
                               mapping=aes(colour="Normal", ymin=0, ymax=..y..),
                               fill=I("red"), alpha=0.3)
    
    q0 <- q0 +  geom_histogram(aes(y=..density..), alpha=0.5, colour=I("white"), 
                               fill=I("blue"))
    q0 <- q0 + theme(legend.position="none") + 
      scale_x_continuous(breaks = round(seq(xmin, xmax, by = 1),1))
    q0
  })
  
  # redefine the sample chart
  sample_def <- eventReactive(input$resample, {
    x <- sampling() 
       
    # create chart for the SAMPLE
    qplot(x, geom="dotplot", xlim=c(xmin,xmax), main="One Sample from the Population",
          method="histodot", dotsize=0.4, colour=I("white"), fill=I("blue"), ylab="") +
      scale_x_continuous(breaks = round(seq(xmin, xmax, by = 1),1))
    
  })
  
  # redefine the sampling distribution
  samplingDistr_def <- eventReactive(input$resample, {
    
    # generate sample
    mm <- mean(means)
    dd <- sd(means)
    bwidth <- min((xmax-xmin), (max(means)-min(means))/20)
    # generate sampling distribution
    title <- "Distribution of the Sample Means"
    msg <- paste("Mean of means = ", signif(mm,6),"\nSD of means = ", signif(dd,6))
    qdistr <- qplot(means, geom="blank", ylab="", xlab=msg, main=title) +
      scale_x_continuous(limits=c(xmin,xmax), breaks = round(seq(xmin, xmax, by = 1),1)) +
      geom_segment(aes(x=meanPop, y= 0, xend=meanPop, yend=1), colour=I("blue"), size=1.5) 
    if (length(means) > 1)
      qdistr <- qdistr +
      geom_segment(aes(x=mm, y= 0, xend=mm, yend=0.8), colour=I("red"), size=1.5) 
    if (length(means) <= 50)
      qdistr <- qdistr +  geom_dotplot(method="histodot", dotsize=0.4,colour=I("white")) 
    else
      qdistr <- qdistr + geom_histogram(aes(y=..density..), alpha=0.5, 
                                        colour=I("white"), fill=I("black"), 
                                        binwidth=bwidth)
    print(mean(means))
    
    qdistr
  })
  
  # compute summary statistics
  compute_summary <- eventReactive(actionType$lastAction,{
    actionType$lastAction <- "no"
    
    grandMean <- mean(means)
    cat("grand mean is : ", grandMean)
    sd <- sd(means)
    nSamples <- length(means)
    summary <- c(meanPop, grandMean, sd, nSamples)
    summary
  })
  
  # output summary statistics
  output$summary <- renderTable(digits=4, {
    summary <-  compute_summary()
    
    input$resend
    isolate({
      data <- data.frame(
        params = c("Population Mean",
                   "Mean of Samples",
                   "SD of Samples",
                   "Nr. Samples"),
        values = c(summary[1], summary[2], summary[3], summary[4]))
      colnames(data) <- c("Observed Parameter","Current Value")
      return(data)
    })
    
  })
  
  # create main plot (divided in three subplots)
  output$plotPop <- renderPlot({
    doReset()
    
    # generate population under a given distribution    
    q0 <- population_def() # activated only when the "resample" button is pressed
    qsample <- sample_def() # resample
    qdistr <- samplingDistr_def() 
    
    # plot the three charts
    grid.arrange(q0, qsample, qdistr, nrow=3, ncol=1)

    actionType$lastAction <- "yes"
  }, height=700)
  
})
