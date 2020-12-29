#Assignment 2.2.1a - using Model4, looking at parameter effects

#################################
#Answers to questions:
#Part 1 - increase prey[3] abundance
#prey <- c(5, 15,50)
#Consumption of prey[3] increases, consumption of other prey drops
#Part 2 - increase prey[1] search rate
#sr[1] <- .2 - 4x increase
#consumption of prey[1] increases, consumption of other prey drops
#Demonstrates interconnection among prey types - changes in one prey type affects predation 
# on other prey
##################################

# Define starting values for state variables
birds <- 20  # total numbers
prey <- c(5, 15, 25)  # three fish prey types, kg per ha


# Define variables for cormorant processes
a <- 1  
b <- .0046
c <- 1
d <- 50
# Define variables for predation
sr <- rep(.05,3)  # same as Tseheye paper, units are ha/day
ht <- rep(4,3)    # units are days/kg
# functional response calculates consumption in kg per day
sr[1] <- .2

# Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
eaten <- array(0,dim=c(3,100))  #prey consumption

# Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  # calculate consumption for each prey
  for (j in 1:3) {
    eaten[j,i] <- sr[j]*prey[j]/(1+sum(sr*ht*prey)) * birds
  } 
  # Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a * exp(-b * birds)
  deaths_per_capita <- c * birds / (d + birds)
  
  # Convert rates to actual numbers
  births <- births_per_female * birds / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds
  
  # Apply the tautology
  birds <- birds + births - deaths
  
  # Update the vectors of numbers and rates
  birds_over_time[i] <- birds
  birth_rate[i] <- births_per_female
  death_rate[i] <- deaths_per_capita
  
  #close the time loop
}

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
Consumption_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=eaten[1,]),
            color='blue') +
  geom_line(mapping=aes(x=x,y=eaten[2,]),
            color="red") +
  geom_line(mapping=aes(x=x,y=eaten[3,]),
            color="green") +
  labs(title = "Predation",
       x="Year",
       y="Consumption per day (kg)") +
  coord_cartesian(ylim=c(0,10)) +
  theme_bw()
plot(Consumption_plot)


