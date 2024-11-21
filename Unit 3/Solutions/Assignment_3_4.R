# Use the cormorant model and add:

# 1. stochastic winter severity index using a gamma distribution 
#    simulate death rate as linearly related to winter severity
# 2. lognormally distributed stochastic variation in birth rate, same as earlier
# 3. normal distribution of cull levels, same as assignment 3.2

# Set an objective for controlled abundance level and determine the largest 
# target cull amount that meets the objective 90% of the time.

# Use 20 year simulations
# Record the final abundance
# Set a target based on the lower 20th percentile of final abundance
# Calculate the variance in this value among replicate simulations
# Estimate a target number of simulations to get an estimate of the 20th percentile that is +/- 10%

# Results comments at end of code

library(ggplot2)

# Simulation controls
nsims <- 100
nyears <- 20
simyears <- nsims * nyears

# Define variables for processes
# Birth rate
a <- 1
b <- .0046
br_sd <- .2

# Death rate
# Gamma with mean ~ 6, variance ~ 18, 90% between 1 and 14
g_shape <- 2  # gamma shape
g_scale <- 3  # gamma scale
ws_slope <- .0154  # winter severity slope
ws_int <- .25    # winter severity intercept

# Harvest
hm <- 1.8  # average harvest or culling rate (1000s)
hsd <- .4 * hm # standard deviation of harvest: cv = .4

# Assignment 3.4 - Part 3
# Calculate variance of the objective proportion
nsamp <- 100
metric <- numeric(nsamp)
for (ivar in 1:nsamp) {
  
# Set up a data frame for time series of numbers and rates for each replicate simulation
birds_over_time <- numeric(simyears) # 100 years x 100 replicates
harvest <- numeric(simyears)
years <- numeric(simyears)
reps <- numeric(simyears)
br <- numeric(simyears)
dr <- numeric(simyears)
outputs <- data.frame(reps,years,birds_over_time,harvest,br,dr)
df_counter <- 1 # initial row for data frame


# Start a loop over replicate simulations
for (sims in 1:nsims) {
  
  # Re-establish starting values for each replicate simulation
  birds <- 20
  
  # Start the main simulation loop over time
  for (i in 1:nyears) {   # we will run the model for 100 time steps
    
    # Calculate the birth and death rates according to our dynamic processes
    # births per female is gamma dist around Ricker (density dependent) average
    br_dev <- rnorm(1,0,br_sd)  # .2 = sqrt(.04)
    births_per_female <- a * exp(-b * birds + br_dev) # add lognormal error
    
    # deaths per capita depend winter severity
    wint_sev <- rgamma(1,shape=g_shape,scale=g_scale)
    deaths_per_capita <- ws_int + ws_slope * wint_sev
    
    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds
    harvest <- rnorm(1,hm,hsd)  # normally distributed harvest variation
    
    # Apply the tautology
    birds <- birds + births - deaths - harvest
    if (birds < 0) birds <- 0
    
    # Add a row to the output data frame
    outputs[df_counter,] <- c(sims,i,birds,harvest,births_per_female,
                              deaths_per_capita)
    df_counter <- df_counter + 1
    #close the time loop
  }
  #close the replicate simulation loop
}

# Management objective: 20th quantile is greater than xx
lastyear <- subset(outputs,outputs$years==nyears)
metric[ivar] <- quantile(lastyear$birds_over_time,probs=c(.2))

# Next sample
}
# Get var and CV
v_metric <- var(metric)
m_metric <- mean(metric)
cv_metric <- sqrt(v_metric)/m_metric
# Confidence interval
upper_ci <- m_metric+1.96*sqrt(v_metric/nsamp)
lower_ci <- m_metric-1.96*sqrt(v_metric/nsamp)
(upper_ci - m_metric)/m_metric
# sample requirement: # of samples of nsims x nyears simulations
n_req <- ((1.96/.05)*cv_metric)^2




breaks <- seq(0,150,5)  # define bins for histogram using "breaks"
Dist_plot <- ggplot() +
  geom_histogram(mapping = aes(x=lastyear$birds_over_time),
                 breaks=breaks) +
  labs(title="Distribution of forecasted bird abundance",
       x="Numbers",
       y="Count") +
  theme_bw()
plot(Dist_plot)


# calculate the mean across simulations
avg_birds <- as.numeric(by(outputs$birds, outputs$years, FUN=mean))
avg_harvest <- as.numeric(by(outputs$harvest, outputs$years, FUN=mean))
avg_br <- as.numeric(by(outputs$br, outputs$years, FUN=mean))
avg_dr <- as.numeric(by(outputs$dr, outputs$years, FUN=mean))


# Plot the results - ggplot version
x <- seq(from=1,to=nyears)
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
  coord_cartesian(ylim=c(0,100)) +
  theme_classic()
plot(Number_plot)

Rate_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=avg_br),
            color='blue') +
  geom_line(mapping=aes(x=x,y=avg_dr),
            color="red") +
  labs(title = "Birth and death rates",
       x="Year",
       y="Per capita rate") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw()
plot(Rate_plot)

Birth_var <- ggplot() +
  geom_histogram(mapping=aes(x=outputs$br)) +
  theme_bw()
Birth_var

# For 100 simulation replicates
# A cull amount of 1.8 (1,800 birds) gives a "prop_low" value that is less than .1
# at least, most of the time

# Sample size requirement ranges from ~ 24-33 based on 20 trials 
