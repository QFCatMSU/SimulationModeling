# Assignment 2.2a --- recalibrating pop dy for age-structured model

# Create a vector of initial numbers at age
# Note that R normally has "1" as the first index of a vector, so 
#  for us Birds[1] is actually age 0 birds...
birds <- c(15,10,5,3,10)

# Define variables for processes (same as First_Model!)
#Assignment 2.2a - Solution: Only older birds are reproducing, so:
#   a) # offspring per female * # females is a smaller number
#   b) only density of mature birds affects birth rates so compensation is less at the same pop size
a <- 1  # Assignment 2.2a: you can adjust this parameter to compensate for fewer mature birds...
b <- .0046
c <- 1
d <- 50

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
  
  # Now the tautology - more complicated than the last time
    # First the plus group: subtract deaths for the oldest two age groups
    birds[5] <- birds[5] + birds[4] - deaths[5] - deaths[4]
    # Next the remaining ages except babies, working backwards
    for (j in 4:2) { #loop backwards over ages
      birds[j] <- birds[j-1] - deaths[j-1]
    }
    # Finally, the babies
    birds[1] <- births
  
  # Update the vectors of numbers and rates
    birds_over_time[,i] <- birds
    all_ages_over_time[i] <- sum(birds_over_time[,i])
    birth_rate[i] <- births_per_female
    death_rate[i] <- deaths_per_capita

  # close the time loop
}

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
  geom_line(mapping=aes(x=x,y=birds_over_time)) +
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
  coord_cartesian(ylim=c(0,1)) +
  theme_bw()
plot(Rate_plot)


  