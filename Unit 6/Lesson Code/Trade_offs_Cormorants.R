#Unit 6, looking at trade-offs among competing objectives
#  First objective is to maintain cormorants close to an acceptable level
#  Second objective is to avoid low prey fish abundances

#We use the model from Assignment 2.4b and add annual stochasticity to birth rates for birds and
#  among simulation stochasticity for pop growth rates for fish
# We add a cull and look for the culling rate that balances cormorant and fish numbers
#  Objective for cormorants:  average deviation from 25,000 birds is minimized
#  Objective for prey fish #1: minimize frequency of of prey biomass < 5 kg/ha

##################################
# Specify feeding area
area <- 25  # ha
# Add feeding season length in days to get total seasonal consumption
daysperseason <- 80

# Define variables for cormorant processes
a <- 1  
b <- .0046
c <- 1
d <- 50
cr <- 0.2 # culling rate

# Define variables for predation
sr <- rep(.05,3)  # same as Tsehaye paper, units are ha/day
ht <- rep(4,3)    # units are days/kg
# functional response calculates consumption in kg per day

#Define population paramters for prey species (logistic model)
prey_r_mean <- c(.5,1.5,2)
prey_r <- numeric(3)
prey_K <- c(10,30,50)

# Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
eaten <- array(0,dim=c(3,100))  #prey consumption
prey_over_time <- array(0,dim=c(3,100))  # biomass of prey kg/ha
mortality <- array(0,dim=c(3,100))  # predation mortality

# Start an outer loop over simulations
kreps <- 1000  # of simulations
target_birds <- 25 # management target for cormorants
target_prey <- 5  # management target for fish
final_birds <- numeric(kreps)
target_dev <- numeric(kreps)
final_prey <- array(0,dim=c(3,kreps))
for (k in 1:kreps) {
  # Define starting values for state variables
  birds <- 20  # total numbers
  prey <- c(5, 15, 25)  # three fish prey types, kg per ha
  # Start the main simulation loop over time
  for (i in 1:100) {   # we will run the model for 100 time steps

    for (j in 1:3) {
      eaten[j,i] <- sr[j]*prey[j]/(1+sum(sr*ht*prey)) * birds/area * daysperseason
      # cap consumption at the total biomass available
      eaten[j,i] <- min(prey[j],eaten[j,i])
      # Calculate and save mortality
      mortality[j,i] <- eaten[j,i]/prey[j]
      # If it's the first year, get random prey_r values for this simulation
      if (i == 1) prey_r[j] <- rnorm(1,prey_r_mean[j], prey_r_mean[j]*.2)
    }  
    # Calculate the birth and death rates according to our dynamic processes
    # Add stochastic birth rate
    births_per_female <- a * exp(-b * birds + rnorm(1,0,.2))  # Add lognormal birth error
    deaths_per_capita <- c * birds / (d + birds)

    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds + cr * birds # add culling
    
    # Apply the tautologies
    # birds
    birds <- birds + births - deaths
    if (birds < 0) birds <- 0  #trap negative numbers
    # prey fish
    # add stochastic variation in prey r
    prey <- prey + prey_r*prey*(1-prey/prey_K) - mortality[,i]*prey
    
    # Update the vectors of numbers and rates at the end of the time step
    birds_over_time[i] <- birds/area
    prey_over_time[,i] <- prey  # calculate consumption for each prey
   
    #close the time loop
  }
  #save final numbers
  final_birds[k] <- mean(birds_over_time[80:100]) * area
  target_dev[k] <- abs(final_birds[k]-target_birds)  # cormorant performance metric
  for (j in 1:3) {
    final_prey[j,k] <- mean(prey_over_time[j,80:100])
  }
}

count_low <- length(which(final_prey[1,]<target_prey))  # fish performance metric

# Display final values and performance metrics
mean(final_birds)
mean(final_prey[1,])
cr
mean(target_dev)
count_low/kreps

# Plot the histograms of results
library(ggplot2)
b_breaks <- seq(0,30,2)  # define bins for histogram using "breaks"
Dist_birds <- ggplot() +
  geom_histogram(mapping = aes(x=final_birds),
                 breaks=b_breaks) +
  geom_vline(xintercept = 25) +
  labs(title="Distribution of forecasted bird abundance",
       x="Numbers (thousands)",
       y="Count") +
  annotate(geom="text", x=0, y=30, hjust=0,
           label=paste('Mean birds =',signif(mean(final_birds),3), sep=" ")) +
  annotate(geom="text", x=0, y=20, hjust=0,
           label=paste('Mean deviation=',signif(mean(target_dev),3), sep=" ")) +
  annotate(geom="text", x=0,y=75, hjust=0,
           label="Target abundance: 25K") +
  theme_bw()
plot(Dist_birds)

p_breaks <- seq(0,15,2)  # define bins for histogram using "breaks"
Dist_fish <- ggplot() +
  geom_histogram(mapping = aes(x=final_prey[1,]),
                 breaks=p_breaks) +
  geom_vline(xintercept = 5) + 
  labs(title="Distribution of forecasted prey fish abundance",
       x="Numbers",
       y="Count") +
  annotate(geom="text", x=0, y=70, hjust=0,
           label=paste('mean =',signif(mean(final_prey[1,]),3), sep=" ")) +
  annotate(geom="text", x=0, y=80, hjust=0,
           label=paste('Frequency low=',count_low/kreps, sep=" ")) +
  annotate(geom="text", x=2.5,y=120, hjust=0,
           label=" Target\n biomass:\n 5 kg/ha") +
  theme_bw()
plot(Dist_fish)   

