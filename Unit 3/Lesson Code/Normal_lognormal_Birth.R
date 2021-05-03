#Unit 3: Normal and lognormal distributions 
# We use the same model here, from Unit 2, a simple closed population

# This time we will simulate first, stochastic variation in harvest (normal) and
# and variation in the max birth (uniform) - same as previous script - 
# and second, variation in harvest and variation in the actual birth rate (lognormal)

# In this version of the model the harvest is an amount, not a rate

library(ggplot2)

# Part 1 - Normally distributed variation in the actual harvest, plus uniform distribution  
# of maximum birth rate

# Define variables for processes
b <- .0046
c <- 1
d <- 50
# 100 uniform random values for "a"
a_var <- runif(100,.75,1.25)

hm <- 3  # average harvest or culling rate (1000s)
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
 for (i in 1:100) { 
  
  # Calculate the birth and death rates according to our dynamic processes
  #'a_var' varies among sims
  births_per_female <- a_var[sims] * exp(-b * birds) 
  deaths_per_capita <- c * birds / (d + birds)
  
  # Convert rates to actual numbers
  births <- births_per_female * birds / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds
  # normally distributed harvest variation
  harvest <- rnorm(1,hm,hsd)
  
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

#Part 2 - Add lognormal variation in birth rates, instead of uniform variation in a

# Define variables for processes
a_fix <- 1  # max birth rate no longer variable
b <- .0046
c <- 1
d <- 50
hm <- 3  # average harvest or culling rate (1000s)
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
  for (i in 1:100) {  
    
    # Calculate the birth and death rates according to our dynamic processes
    br_dev <- rnorm(1,0,.2)  # .2 = sqrt(.04)
    births_per_female <- a_fix * exp(-b * birds + br_dev) # add lognormal error
    deaths_per_capita <- c * birds / (d + birds)
    
    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds
    # normally distributed harvest variation
    harvest <- rnorm(1,hm,hsd)  
    
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


