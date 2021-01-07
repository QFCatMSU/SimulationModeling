#Fourth model - simple closed population, with predation on three fish prey types

# Step 1: Define starting values for state variables
birds <- 20  # total numbers
prey <- c(5, 15, 25)  # three fish prey types, kg per ha


# Step 2: Define variables for cormorant processes
a <- 1  
b <- .0046
c <- 1
d <- 50
# Step 3: Define variables for predation
sr <- rep(.05,3)  # same as Tseheye paper, units are ha/day
ht <- rep(4,3)    # units are days/kg
# functional response calculates consumption in kg per day

# Step 4: Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
eaten <- array(0,dim=c(3,100))  #prey consumption

# Step 5: Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  # Step 6: calculate consumption for each prey
  for (j in 1:3) {
    eaten[j,i] <- sr[j]*prey[j]/(1+sum(sr*ht*prey)) * birds
  } 
  # Step 7: Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a * exp(-b * birds)
  deaths_per_capita <- c * birds / (d + birds)
  
  # Convert rates to actual numbers
  births <- births_per_female * birds / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds
  
  # Step 8: Apply the tautology
  birds <- birds + births - deaths
  
  # Step 9: Update the vectors of numbers and rates
  birds_over_time[i] <- birds

  #Step 10: close the time loop
}

# Step 11: Plot the results - ggplot version
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


