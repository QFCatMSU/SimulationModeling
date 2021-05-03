# This code demonstrates two ways to interpret parameter uncertainty from a model fitting process

library(ggplot2)
library(mvtnorm)  # we need the mvtnorm package to generate multivariate normal random numbers

# This function calculates the standard error of the residuals from a lm object
s_e_resid <- function(lin_model) {   
  sumsq_res <- sum(lin_model$residuals^2)
  df <- lin_model$df.residual
  return <- sqrt(sumsq_res/df)
}

# Get the data:
br_data <- read.csv(file="../Data/BirthData.csv", header=T)

# Fit a linear model to log-transformed data:
br_data$lnbr <- log(br_data$Obs_br)
linearfit <- lm(lnbr~Numbers, data=br_data)
summary(linearfit)


# Stochastic population model that uses birth process parameter estimates
# Define variables for processes
a <- exp(linearfit$coef[1])+s_e_resid(linearfit)^2/2  # includes bias correction
b <- linearfit$coef[2]
c <- 1
d <- 50
hm <- 3  #harvest or culling rate (1000s)

opt <- 2  # which uncertainty option to use

# Set up a data frame for time series of numbers and rates for each replicate simulation
birds_over_time <- numeric(10000) # 100 years x 100 replicates
harvest <- numeric(10000)
years <- numeric(10000)
reps <- numeric(10000)
outputs <- data.frame(reps,years,birds_over_time,harvest)
df_counter <- 1 # initial row for data frame

# Start a loop over replicate simulations

if (opt == 1) { # birth rate parameters are known, all uncertainty is process variation
for (sims in 1:100) {
  
  # Re-establish starting values for each replicate simulation
  birds <- 20
  
  # Start the main simulation loop over time
  for (i in 1:100) {   # we will run the model for 100 time steps
    
    # Calculate the birth and death rates according to our dynamic processes
    # Option 1: all uncertainty is process variation
    br_dev <- rnorm(1,0,s_e_resid(linearfit))  # residual sd from model fit
    births_per_female <- a * exp(b * birds + br_dev) # add lognormal error
    deaths_per_capita <- c * birds / (d + birds)
    
    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds
    harvest <- hm # no variation in harvest
    
    # Apply the tautology
    birds <- birds + births - deaths - harvest
    if (birds < 0) birds <- 0
    
    # Add a row to the output data frame
    outputs[df_counter,] <- c(sims,i,birds,harvest)
    df_counter <- df_counter + 1
  } #close the time loop
  
} #close the replicate simulation loop
  
  
} else { # Option 2: birth rate pars uncertain, plus process variation
    for (sims in 1:100) {
    
    # select birth rate parameters
    br_par <- rmvnorm(1,mean=linearfit$coefficients, sigma=vcov(linearfit))
    a <- exp(br_par[1])+s_e_resid(linearfit)^2/2
    b <- br_par[2]
    # Re-establish starting values for each replicate simulation
    birds <- 20
    
    # Start the main simulation loop over time
    for (i in 1:100) {   # we will run the model for 100 time steps
      
      # Calculate the birth and death rates according to our dynamic processes
      # Option 1: all uncertainty is process variation
      br_dev <- rnorm(1,0,s_e_resid(linearfit))  # residual sd from model fit
      births_per_female <- a * exp(b * birds + br_dev) # add lognormal error
      deaths_per_capita <- c * birds / (d + birds)
      
      # Convert rates to actual numbers
      births <- births_per_female * birds / 2  # divide by 2 because per female
      deaths <- deaths_per_capita * birds
      harvest <- hm # no variation in harvest
      
      # Apply the tautology
      birds <- birds + births - deaths - harvest
      if (birds < 0) birds <- 0
      
      # Add a row to the output data frame
      outputs[df_counter,] <- c(sims,i,birds,harvest)
      df_counter <- df_counter + 1
    } #close the time loop
    
  } #close the replicate simulation loop  
}
# calculate the mean across simulations
avg_birds <- as.numeric(by(outputs$birds, outputs$years, FUN=mean))
avg_harvest <- as.numeric(by(outputs$harvest, outputs$years, FUN=mean))

# Plot the results - ggplot version
x <- seq(from=1,to=100)
Number_plot <- ggplot() +
  geom_line(mapping=aes(x=outputs$years,y=outputs$birds_over_time, group=outputs$reps),
            color = 'lightgrey') +
  geom_line(mapping=aes(x=x, y=avg_birds),
            color='black', size=1.3) +
  geom_line(mapping=aes(x=x, y=avg_harvest),
            color='red', size=1.3) +
  labs(title = "Abundance",
       x = "Year",
       y = "Cormorant numbers") +
  coord_cartesian(ylim=c(0,50)) +
  theme_classic()
plot(Number_plot)
lastyear <- subset(outputs,outputs$years==100)
Dist_plot <- ggplot() +
  geom_histogram(mapping = aes(x=lastyear$birds_over_time),
                 binwidth = 3) +
  labs(title="Distribution of forecasted bird abundance",
       x="Numbers",
       y="Count") +
  theme_bw()
plot(Dist_plot)                 
