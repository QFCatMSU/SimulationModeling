#Assignment 2.2.1c - using Model4, looking at predator effects on prey dynamics
# Type 2 vs Type 3 functional response
# To make functional response curves look similar the parameters for the type 2
# and type 3 responses need to be different

#################################
#Answers to questions:
# Prey collapse for Type 2 and 15, but find lower eql for Type 3 and 15
# Prey reach similar levels for Type 2 and 3 and 50
# Look at mortality versus density for two fr types
##################################

# Units matter more now, so we need to specify the size of the area where predation is 
# ocurring to convert bird numbers to bird density, and the length of the feeding period
# Specify feeding area
area <- 25  # ha
# Add feeding season length in days to get total seasonal consumption
daysperseason <- 80

# Define starting values for state variables
birds <- 36  # total numbers
prey <- 15  # single prey type for this question

# Define variables for cormorant processes
a <- 1  
b <- .0046
c <- 1
d <- 50
# Define variables for predation
sr <- 0.015  # same as Tseheye paper, units are ha/day
ht <- 4    # units are days/kg
type <- 2  # functional response type
# functional response calculates consumption in kg per day

#Define population paramters for prey species (logistic model)
prey_r <- .9
prey_K <- 100

# Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
eaten <- numeric(100)  #prey consumption
prey_over_time <- numeric(100)  # biomass of prey kg/ha
mortality <- numeric(100) # predation mortality

# Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  # calculate consumption for each prey
  for (j in 1:3) {
    eaten[i] <- sr*prey^(type-1)/(1+sr*ht*prey^(type-1)) * birds/area * daysperseason
    # cap consumption at the total biomass available
    eaten[i] <- min(prey,eaten[i])
    # Calculate and save mortality
    mortality[i] <- eaten[i]/prey
      } 
  # Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a * exp(-b * birds)
  deaths_per_capita <- c * birds / (d + birds)
  
  # Convert rates to actual numbers
  births <- births_per_female * birds / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds
  
  # Apply the tautologies
  # birds
  birds <- birds + births - deaths
  # fish
  prey <- prey + prey_r*prey*(1-prey/prey_K) - mortality[i]*prey
  
  
  # Update the vectors of numbers and rates
  birds_over_time[i] <- birds/area
  prey_over_time[i] <- prey
  
  
  #close the time loop
}

# Plot the results - ggplot version
library(ggplot2)
x <- seq(from=1,to=100)
Number_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=birds_over_time*area)) +
  labs(title = "Abundance",
       x = "Year",
       y = "Cormorant numbers") +
  coord_cartesian(ylim=c(0,50)) +
  theme_bw()
plot(Number_plot)
Consumption_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=eaten*area/daysperseason),
            color='blue') +
  labs(title = "Predation",
       x="Year",
       y="Consumption per day (kg)") +
  coord_cartesian(ylim=c(0,10)) +
  theme_bw()
plot(Consumption_plot)
Prey_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=prey_over_time),
            color='blue') +
  labs(title = "Prey fish biomass",
       x= 'Year',
       y='Biomass (kg/ha') +
  coord_cartesian(ylim=c(0,100)) +
  theme_bw()
plot(Prey_plot)
Mort_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=mortality),
            color='blue') +
  labs(title = "Prey fish mortality",
       x= 'Year',
       y='Mortality rate') +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw()
plot(Mort_plot)
