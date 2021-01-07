# Third model - meta-population with age structure
# Three sub-populations, identical dynamics

# Step 1: Create an array of initial numbers at age, by sub-population
# Note that R normally has "1" as the first index of a vector, so 
#  for us Birds[1] is actually age 0 birds...

birds <- array(c(15,15,15,
                 10,10,10,
                 5,5,5,
                 3,3,3,
                 10,10,10),
               dim=(c(3,5)))

# Step 2: Define variables for birth and death processes (same as Model1!)
a <- 2  
b <- .0046
c <- 1
d <- 50

# Step 3: Create a movement matrix - initially set movement to zero
move <- t(array(c(1,0,0,
                  0,1,0,
                  0,0,1), dim = c(3,3))) # NB - transposed to make code look like result

# Step 4: Set up containers for time series of numbers
birds_over_time <- array(0,dim=c(3,5,100))
sub_pop_over_time <- array(0,dim=c(3,100))

# Step 5: Start the main simulation loop over time
for (i in 1:100) {   # we will run the model for 100 time steps
  # Step 6: For each sub-population...

  # Step 7: Move birds among sub-populations (tautology part 1)
    for (k in 1:5) {  # ages
      birds[,k] <- colSums(birds[,k]*move) # Sums birds coming to each subpop from other subpops, including those that stay
    }

  for (j in 1:3) { # loop over sub-populations 
  # Step 8: Calculate the birth and death rates according to our dynamic processes
  births_per_female <- a * exp(-b * sum(birds[j,4:5]))  # only mature (age 3,4+) birds affect birth rate
  deaths_per_capita <- c * sum(birds[j,]) / (d + sum(birds[j,]))  # all birds affect death rate
  
  # Convert rates to actual numbers
  births <- births_per_female * sum(birds[j,4:5]) / 2  # divide by 2 because per female, only age 3 and older are mature
  deaths <- deaths_per_capita * birds[j,]   # note: deaths will be a vector by age, just like birds

  # Step 9: Now tautology part 2
  # First the plus group: subtract deaths for the oldest two age groups
  birds[j,5] <- birds[j,5] + birds[j,4] - deaths[5] - deaths[4]
  # Next the remaining ages except babies, working backwards
  for (k in 4:2) { #loop backwards over ages
    birds[j,k] <- birds[j,k-1] - deaths[k-1]
  }
  # Step 10: Finally, the babies
  birds[j,1] <- births  
   
# Step 11: Update the vectors of numbers and rates
birds_over_time[j,,i] <- birds[j,]
sub_pop_over_time[j,i] <- sum(birds_over_time[j,,i])


# close the time loop
  }}

# Step 12: Plot the results - ggplot version only from now on
library(ggplot2)
x <- seq(from=1, to=100)
plot_subs <- ggplot() +
  geom_line(mapping=aes(x=x,y=sub_pop_over_time[1,]),
            color='blue') +
  geom_line(mapping = aes(x=x,y=sub_pop_over_time[2,]),
            color='red') +
  geom_line(mapping = aes(x=x,y=sub_pop_over_time[3,]),
            color='green') +
  labs(title = "Abundance",
       x = "Year",
       y = "Cormorant numbers") +
  coord_cartesian(ylim=c(0,50)) +
  theme_bw()
plot(plot_subs)  

