#Unit 6, Simple decision, part 2: one objective, continuous options
# We will use the cormorant model here, with stochastic birth rate,
# and normally distributed variation (outcome uncertainty) in the harvest (cull)
# birth rate cv is 20%, harvest cv is 20%

# We will make the objective to maintain abundance as close to 10,000 birds as possible
# and only consider the last 50 years to avoid the effect of initial values

library(ggplot2)

# Define variables for processes
a <- 1
b <- .0046
c <- 1
d <- 50

# Set up a loop over harvest rates
hm <- seq(3,5,.2)  # 3000-5000 birds per year
hsd <- .2 * hm # standard deviation of harvest: cv = .2

# storage vectors for intermediate calculations
avg_birds <- numeric(11)  # 11 harvest levels
birds_l50 <- numeric(100)  # 100 sim replicates
birds_over_time <- numeric(100) # 100 years
for (ih in 1:11) {
  
  # Start a loop over replicate simulations
  for (sims in 1:100) {
  
  # Re-establish starting values for each replicate simulation
  birds <- 20
  
  # Start the main simulation loop over time
  for (i in 1:100) {   # we will run the model for 100 time steps
    
    # Calculate the birth and death rates according to our dynamic processes
    br_dev <- rnorm(1,0,.2)  # .2 = sqrt(.04)
    births_per_female <- a * exp(-b * birds + br_dev) # add lognormal error
    deaths_per_capita <- c * birds / (d + birds)
    
    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds
    harvest <- rnorm(1,hm[ih],hsd[ih])  # normally distributed harvest variation
    
    # Apply the tautology
    birds <- birds + births - deaths - harvest
    if (birds < 0) birds <- 0
    
    # Add a row to the output data frame
    birds_over_time[i] <- birds
    #close the time loop

  }
  #compute average numbers for last 50 years
  birds_l50[sims] <- mean(birds_over_time[51:100])
  #close the replicate simulation loop
  }
  avg_birds[ih] <- mean(birds_l50)
}


# Plot the results - ggplot version
Harvest_range <- ggplot() +
  geom_line(mapping=aes(x=hm*1000,y=avg_birds*1000),
            color = 'black') +
  geom_hline(yintercept = 10000, color='red') +
  labs(title = "Culling rate performance",
       x = "Culling rate",
       y = "Cormorant numbers") +
  coord_cartesian(ylim=c(0,30000)) +
  theme_classic()
plot(Harvest_range)

