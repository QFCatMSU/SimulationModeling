#Assignment 2.2.1b - using Model4, looking at predator effects on prey dynamics

#################################
#Answers to questions:
# Part 1 - mortality is same for all three prey species; equilibrium abundances
# below K for each species, accounting for predation losses at equilibrium;
# consumption reflects differences in biomass - this is why mortality is not
# different; prey #1 might be heading to extinction

# Part 2 - reducing r for species 1 to 0.3 (prey_r <- c(0.3,1.5,2))
# means that it goes extinct by year 100. Increasing it to 1 raises its eqm
# biomass (prey_r <- c(1.0,1.5,2)). The biomasses of the other species change
# slightly when r for species #1 is increased to 1. This is presumably because
# consumption of species #2,3 can drop slightly because more predator
# demand is met by species #1
# Increasing K for species #2,3 can prevent species #1 from going extinct. Higher
# K means more biomass and thus more consumption for specices #2,3, and therefore
# less consumption for species #1. If the latter is low enough predation 
# mortality does not exceed population growth due to r.

##################################

# Units matter more now, so we need to specify the size of the area where predation is 
# ocurring to convert bird numbers to bird density, and the length of the feeding period
# Specify feeding area
area <- 25  # ha
# Add feeding season length in days to get total seasonal consumption
daysperseason <- 80

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

#Define population paramters for prey species (logistic model)
prey_r <- c(.5,1.5,2)
prey_K <- c(10,30,50)

# Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
eaten <- array(0,dim=c(3,100))  #prey consumption
prey_over_time <- array(0,dim=c(3,100))  # biomass of prey kg/ha
mortality <- array(0,dim=c(3,100))  # predation mortality

# Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps

  for (j in 1:3) {
    eaten[j,i] <- sr[j]*prey[j]/(1+sum(sr*ht*prey)) * birds/area * daysperseason
    # cap consumption at the total biomass available
    eaten[j,i] <- min(prey[j],eaten[j,i])
    # Calculate and save mortality
    mortality[j,i] <- eaten[j,i]/prey[j]
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
  # prey fish
  prey <- prey + prey_r*prey*(1-prey/prey_K) - mortality[,i]*prey
  
  # Update the vectors of numbers and rates at the end of the time step
  birds_over_time[i] <- birds/area
  prey_over_time[,i] <- prey  # calculate consumption for each prey
 
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
  geom_line(mapping=aes(x=x,y=eaten[1,]*area/daysperseason),
            color='blue') +
  geom_line(mapping=aes(x=x,y=eaten[2,]*area/daysperseason),
            color="red") +
  geom_line(mapping=aes(x=x,y=eaten[3,]*area/daysperseason),
            color="green") +
  labs(title = "Predation",
       x="Year",
       y="Consumption per day (kg)") +
  coord_cartesian(ylim=c(0,10)) +
  theme_bw()
plot(Consumption_plot)
Prey_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=prey_over_time[1,]),
            color='blue') +
  geom_line(mapping=aes(x=x,y=prey_over_time[2,]),
            color='red') +
  geom_line(mapping=aes(x=x,y=prey_over_time[3,]),
            color='green') +
  labs(title = "Prey fish biomass",
       x= 'Year',
       y='Biomass (kg/ha') +
  coord_cartesian(ylim=c(0,50)) +
  theme_bw()
plot(Prey_plot)
Mort_plot <- ggplot() +
  geom_line(mapping=aes(x=x,y=mortality[1,]),
            color='blue') +
  geom_line(mapping=aes(x=x,y=mortality[2,]),
            color='red') +
  geom_line(mapping=aes(x=x,y=mortality[3,]),
            color='green') +
  labs(title = "Prey fish mortality",
       x= 'Year',
       y='Mortality rate') +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw()
plot(Mort_plot)
