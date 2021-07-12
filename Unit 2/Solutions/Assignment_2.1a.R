# Assignment 2.1a  --- Adding harvest

#First model - simple closed population

# Define starting values for state variables (only one in this case)
birds <- 20

# Define variables for processes
a <- 1  
b <- .0046
c <- 1
d <- 50

#Assignment 2.1a - add a harvest rate parameter here  
hr <- .31   #Solution: max hr that keeps birds above 10K is ~ .31


# Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
birth_rate <- numeric(100)
death_rate <- numeric(100)

# Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  
  # Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a * exp(-b * birds)
  deaths_per_capita <- c * birds / (d + birds)
  
  # Convert rates to actual numbers
  births <- births_per_female * birds / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds

  #Assignment 2.1a - calculate harvest losses
  harvest <- hr * birds
  
  #Assignment 2.1a - modeify the tautology to include harvest losses
  birds <- birds + births - deaths - harvest
  
  #Assignment 2.1a - protect against negative numbers of birds
  if (birds < 0) birds <- 0
  
  # Update the vectors of numbers and rates
  birds_over_time[i] <- birds
  birth_rate[i] <- births_per_female
  death_rate[i] <- deaths_per_capita
  
  #close the time loop
}

# Plot the results - base version
x <- seq(from=1,to=100)
plot(birds_over_time~x, xlab = "Year", ylab = "Numbers", type='l',
     ylim=c(0,50))
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


