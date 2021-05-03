# Use the cormorant model and add:

# 1. stochastic winter severity (compound binomial and beta distributions) to 
#    simulate natural mortality, replace the original death function with
#    binomial probability of bad winter (p(bad) = .2), that selects the 
#    average mortality rate and then use this as the mean for a beta distribution
#    of the actual death rate
# 2. gamma distributed stochastic variation in birth rates with a skew
# 3. normal distribution of cull levels, same as assignment 3.2

# Set an objective for controlled abundance level and determine target cull amount that meets the objective 90% of the time.

# Determine the number of simulations needed to be 95% certain your harvest rate is sufficient to meet the objective

library(ggplot2)

# Define variables for processes
# Birth rate
a <- 1
b <- .0046
g_scale <- 1.5  # gamma scale
# gamma shape defined by avg_br/scale

# Death rate
binom_p <- .2     # prob of high mortality
avg_dr_high <- .6 # high mortality average
avg_dr_low  <- .3 # low mortality average
beta_a <- 2  # beta shape 1
# beta shape 2 defined by beta_a/avg_dr - beta_a

hm <- 0  # average harvest or culling rate (1000s)
hsd <- .4 * hm # standard deviation of harvest: cv = .4

# Set up a data frame for time series of numbers and rates for each replicate simulation
birds_over_time <- numeric(10000) # 100 years x 100 replicates
harvest <- numeric(10000)
years <- numeric(10000)
reps <- numeric(10000)
outputs <- data.frame(reps,years,birds_over_time,harvest)
df_counter <- 1 # initial row for data frame

# Start a loop over replicate simulations
for (sims in 1:100) {
  
  # Re-establish starting values for each replicate simulation
  birds <- 20
  
  # Start the main simulation loop over time
  for (i in 1:100) {   # we will run the model for 100 time steps
    
    # Calculate the birth and death rates according to our dynamic processes
    # births per female is gamma dist around Ricker (density dependent) average
    avg_br <- a * exp(-b * birds)
    g_shape <- avg_br/g_scale
    births_per_female <- rgamma(1,shape=g_shape,scale=g_scale)

    # deaths per capita depend on high vs low Bernoulli draw and beta
    if (rbinom(n=1,size=1,prob=binom_p)==1) {  # single Bernoulli trial, high mort if result is 1
      beta_b <- beta_a/avg_dr_high - beta_a
    } else {
      beta_b <- beta_a/avg_dr_low - beta_a
    }
    deaths_per_capita <- rbeta(1,shape1=beta_a,shape2=beta_b)

    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds
    harvest <- rnorm(1,hm,hsd)  # normally distributed harvest variation
    
    # Apply the tautology
    birds <- birds + births - deaths - harvest
    if (birds < 0) birds <- 0
    
    # Add a row to the output data frame
    outputs[df_counter,] <- c(sims,i,birds,harvest)
    df_counter <- df_counter + 1
    #close the time loop
  }
  #close the replicate simulation loop
}

# Assignment 3.1.2 - frequency for #s < 10,000
too_low <- subset(outputs, outputs$birds_over_time<10)
prop_low <- length(too_low$birds_over_time)/length(outputs$birds_over_time)

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
breaks <- seq(0,40,5)  # define bins for histogram using "breaks"
Dist_plot <- ggplot() +
  geom_histogram(mapping = aes(x=lastyear$birds_over_time),
                 breaks=breaks) +
  labs(title="Distribution of forecasted bird abundance",
       x="Numbers",
       y="Count") +
  theme_bw()
plot(Dist_plot)                 
