#Assignment 3.1 - find max "safe" harvest rate: safe defined as <5% incidence of N < 10,000
#Using Unit 2 first model with uniform variability

# Compare cases where "a" varies among years and where it does not
# Answer: varies among years: 0.27; doesn't vary among years: .2
# Allowing "a" to vary among years causes a "regression to the mean" effect - lower birth
# rates don't persist so the pop doesn't tend to drop as far...

# Define variables for processes
b <- .0046
c <- 1
d <- 50
hr <- .2  # harvest or culling rate
# 100 uniform random values for "a"
a <- runif(100,.75,1.25)  # "a" varies among simulations but not years

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
 for (i in 1:100) {   # we will run the model for 100 time steps
  #a <- runif(1,.75,1.25)  # a varies among years and sims
  # Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a[sims] * exp(-b * birds) #'a' varies among sims
  #births_per_female <- a * exp(-b * birds) #'a' varies among sims and years
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

# Assignment 3.1 - frequency for #s < 10,000
too_low <- subset(outputs, outputs$birds_over_time<10)
prop_low <- length(too_low$birds_over_time)/length(outputs$birds_over_time)

# calculate the mean across simulations
avg_birds <- by(outputs$birds_over_time, outputs$years, FUN=mean) 
avg_br <- by(outputs$birth_rate, outputs$years, FUN=mean)
avg_dr <- by(outputs$death_rate, outputs$years, FUN=mean)


# Plot the results - ggplot version
library(ggplot2)
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
breaks <- seq(0,40,5)  # define bins for histogram using "breaks"
Dist_plot <- ggplot() +
  geom_histogram(mapping = aes(x=lastyear$birds_over_time),
                 breaks=breaks) +
  labs(title="Distribution of forecasted bird abundance",
       x="Numbers",
       y="Count") +
  theme_bw()
plot(Dist_plot)

                 


