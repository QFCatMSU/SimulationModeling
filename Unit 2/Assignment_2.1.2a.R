# Assignment 2.1.2.a --- recalibrating pop dy for age-structured model

# Create a vector of initial numbers at age
# Note that R normally has "1" as the first index of a vector, so 
#  for us Birds[1] is actually age 0 birds...
birds <- c(15,10,5,3,10)

# Define variables for processes (same as First_Model!)
a <- 1  # adjust this parameter to compensate for lower mature birds...
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

#This code shows how I created a data frame with year, age, and numbers as columns
# which enables me to display the results as a stacked-bar plot of the numbers-at-age
# over time. There is almost certainly a more elegant way to do this, including by
# storing the age-based numbers in the appropriate format during the simulation
# However, this works...

#Create five data frames, one for each age group, with columns for year, age and numbers
tmp_1 <- data.frame(seq(1:100),rep(1,100), birds_over_time[1,], row.names = NULL)
dimnames(tmp_1)[[2]] <- c('Year','Age','Numbers')
tmp_2 <- data.frame(seq(1:100),rep(2,100), birds_over_time[2,], row.names = NULL)
dimnames(tmp_2)[[2]] <- c('Year','Age','Numbers')
tmp_3 <- data.frame(seq(1:100),rep(3,100), birds_over_time[3,], row.names = NULL)
dimnames(tmp_3)[[2]] <- c('Year','Age','Numbers')
tmp_4 <- data.frame(seq(1:100),rep(4,100), birds_over_time[4,], row.names = NULL)
dimnames(tmp_4)[[2]] <- c('Year','Age','Numbers')
tmp_5 <- data.frame(seq(1:100),rep(5,100), birds_over_time[5,], row.names = NULL)
dimnames(tmp_5)[[2]] <- c('Year','Age','Numbers')

#Combine all five data frames into one
AgeComp_data <- rbind(tmp_1, tmp_2, tmp_3, tmp_4, tmp_5)

#Use geom_bar to create the graph
AgeComp_Plot <- ggplot(data=AgeComp_data) +
  geom_bar(mapping = aes(fill=Age,x=Year,y=Numbers),
           position = 'stack', stat = 'identity') +
  labs(title="Age Composition",
       x="Year",
       y="Numbers") +
  theme_bw()
plot(AgeComp_Plot)
  