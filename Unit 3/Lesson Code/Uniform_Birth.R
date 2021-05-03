#Unit 3: Uniform distribution
# We use the first model from Unit 2, a simple closed population
# and add a stochastic birth rate process

# We simulate a case where the maximum birth rate (parameter "a" below) varies
# randomly from one simulation to the next, but remains the same for all simulated
# years in each 100-year simulation

# This version of the model also includes a harvest, or cull,implemented as a rate

library(ggplot2)

# Define variables for processes
b <- .0046
c <- 1
d <- 50
hr <- .2  # harvest or culling rate

# 100 uniform random values for "a"
a <- runif(100,.75,1.25)

# Set up a data frame for time series of numbers and rates for each replicate simulation
birds_over_time <- numeric(10000) # 100 years x 100 replicates
birth_rate <- numeric(10000)
death_rate <- numeric(10000)
years <- numeric(10000)
reps <- numeric(10000)
outputs <- data.frame(reps,years,birds_over_time,birth_rate,death_rate)
df_counter <- 1 # initial row for data frame

# Start a loop over replicate simulations
for (sims in 1:100) {

 # Re-establish starting values for each replicate simulation
 birds <- 20

# Start the main simulation loop over time
 for (i in 1:100) {   
  
  # Calculate the birth and death rates according to our dynamic processes
  # Note that 'a' varies from one simulation to the next
  births_per_female <- a[sims] * exp(-b * birds)
  deaths_per_capita <- c * birds / (d + birds)
  
  # Convert rates to actual numbers
  births <- births_per_female * birds / 2  # divide by 2 because per female
  deaths <- deaths_per_capita * birds
  harvest <- hr * birds
  
  # Apply the tautology
  birds <- birds + births - deaths - harvest
  if (birds < 0) birds <- 0
  
  # Add a row to the output data frame
  outputs[df_counter,] <- c(sims,i,birds,births_per_female,deaths_per_capita)
  df_counter <- df_counter + 1
  #close the time loop
}
#close the replicate simulation loop
}
# calculate the mean across simulations
# these are all vectors of length 100
avg_birds <- by(outputs$birds_over_time, outputs$years, FUN=mean) 
avg_br <- by(outputs$birth_rate, outputs$years, FUN=mean)
avg_dr <- by(outputs$death_rate, outputs$years, FUN=mean)


# Plot the results - ggplot version
x <- seq(from=1,to=100)
Number_plot <- ggplot() +
  geom_line(mapping=aes(x=outputs$years,y=outputs$birds_over_time, group=outputs$reps),
              color = 'lightgrey') +
  geom_line(mapping=aes(x=x, y=avg_birds),
            color='black', size=1.3) +
  labs(title = "Abundance",
       x = "Year",
       y = "Cormorant numbers") +
  coord_cartesian(ylim=c(0,50)) +
  theme_classic()
plot(Number_plot)
Rate_plot <- ggplot() +
  geom_line(mapping=aes(x=outputs$years,y=outputs$birth_rate, group=outputs$reps),
            color='lightgrey') +
  geom_line(mapping=aes(x=x,y=avg_br),
            color="red", size=1.3) +
  geom_line(mapping=aes(x=outputs$years,y=outputs$death_rate, group=outputs$reps),
            color='lightgrey') +
  geom_line(mapping=aes(x=x,y=avg_dr),
            color="blue", size=1.3) +
  labs(title = "Birth and death rates",
       x="Year",
       y="Per capita rate") +
  coord_cartesian(ylim=c(0,1)) +
  theme_bw()
plot(Rate_plot)

lastyear <- subset(outputs,outputs$years==100)
Dist_plot <- ggplot() +
  geom_histogram(mapping = aes(x=lastyear$birds_over_time),
                 binwidth = 3) +
  labs(title="Distribution of forecasted bird abundance",
       x="Numbers",
       y="Count") +
  theme_bw()
plot(Dist_plot)

                 


