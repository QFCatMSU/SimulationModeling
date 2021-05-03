## Assignment 2.1b --- logistic pop dy

#First model - simple closed population

# Define starting values for state variables (only one in this case)
birds <- 20

# Define variables for processes
# 1-1b use r and K instead of a-d
r <- 0.38
K <- 36.6
# hr <- .31
# 1-1b - r = .4 and K = 36.6 give pretty similar dynamics.

# Set up vectors for time series of numbers and rates
birds_over_time <- numeric(100)
# 1-1b only one rate in this case
growth_rate <- numeric(100)


# Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  
  # 1-1b calculate the population growth rate according to our (logistic) dynamic process
  gr <- r * birds * ( 1 - birds/K)
#  harvest <- hr * birds

  # 1-1b Apply the tautology
  birds <- birds + gr # - harvest
  
  # Update the vectors of numbers and rates
  birds_over_time[i] <- birds
  growth_rate[i] <- gr
  
  #close the time loop
}

# Plot the results - base version
x <- seq(from=1,to=100)
plot(birds_over_time~x, xlab = "Year", ylab = "Numbers", type='l',
     ylim=c(0,50))
plot(growth_rate~x, xlab="Year", ylab="Rate", type = "l",
     ylim=c(0,5), col='blue')

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
  geom_line(mapping=aes(x=x,y=growth_rate),
            color='blue') +

  labs(title = "Population growth rate",
       x="Year",
       y="Per capita rate") +
  coord_cartesian(ylim=c(0,2)) +
  theme_bw()
plot(Rate_plot)

