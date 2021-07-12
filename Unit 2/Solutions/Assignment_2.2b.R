# Assignment 2.2b - harvesting

# Second model - closed population with age structure

# Create a vector of initial numbers at age
# Note that R normally has "1" as the first index of a vector, so 
#  for us Birds[1] is actually age 0 birds...
birds <- c(15,10,5,3,10)
cumharvest <- 0  # New quantity for Assignment 2.2b

# Define variables for processes (same as First_Model!)
a <- 3  #Assignment 2.2b - adjust this parameter upwards
b <- .0046
c <- 1
d <- 50
#Assignment 2.2b - create a vector of harvest rate by age
#Assignment 2.2b - Solution: harvest rate of 0.34 (34%) is max that keeps pop > 10K
hr <- c(0,0,.34,.34,.34)
#Assignment 2.2b - Solution 2: harvest rate of 0.3 gives max cumulative harvest

# Set up containers for time series of numbers and rates
birds_over_time <- array(0,dim=c(5,100))
all_ages_over_time <- numeric(100)
birth_rate <- numeric(100)
death_rate <- numeric(100)

# Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  
  # Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a * exp(-b * sum(birds[4:5]))  # only mature (age 3,4+) birds affect birth rate
  deaths_per_capita <- c * sum(birds) / (d + sum(birds))
  
  # Convert rates to actual numbers
  births <- births_per_female * sum(birds[4:5]) / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds   # note: deaths will be a vector by age, just like birds
  #Assignment 2.2b - don't allow harvest until year 51
  if (i < 50) {
    harvest <- rep(0,5) } # no harvest
  else {
    harvest <- hr * birds
  }
  # Now the tautology - more complicated than the last time
    # First the plus group: subtract deaths for the oldest two age groups
    birds[5] <- birds[5] + birds[4] - deaths[5] - deaths[4] - harvest[5] - harvest[4]
    # Next the remaining ages except babies, working backwards
    for (j in 4:2) { #loop backwards over ages
      birds[j] <- birds[j-1] - deaths[j-1] - harvest[j-1]
    }
    # Finally, the babies
    birds[1] <- births
  
  # Update the vectors of numbers and rates
    birds_over_time[,i] <- birds
    all_ages_over_time[i] <- sum(birds_over_time[,i])
    birth_rate[i] <- births_per_female
    death_rate[i] <- deaths_per_capita
    # Assignment 2.2b: accumulate the total harvest
    cumharvest <- cumharvest + sum(harvest)

  # close the time loop
}
#Assignment 2.2b - report results
all_ages_over_time[100]
cumharvest

# Plot the results - base version
x <- seq(from=1,to=100)
plot(all_ages_over_time~x, xlab = "Year", ylab = "Total Numbers", type='l',
     ylim=c(0,100))
plot(birth_rate~x, xlab="Year", ylab="Rate", type = "l",
     ylim=c(0,1), col='blue')
lines(death_rate~x, col='red')


# Plot the results - ggplot version
library(ggplot2)
x <- seq(from=1,to=100)
Number_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=all_ages_over_time)) +
  labs(title = "Abundance",
       x = "Year",
       y = "Cormorant numbers") +
  coord_cartesian(ylim=c(0,50)) +
  theme_bw()
plot(Number_plot)
Rate_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=birth_rate),
            color='blue') +
  geom_line(mapping=aes(x=x,y=death_rate),
            color="red") +
  labs(title = "Birth and death rates",
       x="Year",
       y="Per capita rate") +
  coord_cartesian(ylim=c(0,3)) +
  theme_bw()
plot(Rate_plot)

