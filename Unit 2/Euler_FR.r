#This script shows how we can use the Euler method to determine a suitable time step
#for a simple model of predation

#We use fixed predator numbers and
# a single prey species

# Define starting values for predator and prey
birds <- 50  
prey <- 15  

# Define functional response parameters
sr <- .1  # search rate (area per unit time)
ht <- 3    # time per prey

#Start with consumption calculated once per year
# based on starting prey numbers
#This is equivalent to setting the Euler step size to 1 and the 
# number of steps to 1 as well
eaten_onestep <- sr*prey/(1+sr*ht*prey) * birds

#Now calculate for different step sizes (portions of the year)
# updating the prey numbers after each step
Euler_eaten <- numeric(7)
Euler_eaten[0] <- eaten_onestep
step_sz <- c(.5,.2,.1,.05,.02,.01,.005)
steps <- c(2,5,10,20,50,100,200)  # note step_sz[i]*steps[i] must equal 1
# Call function Euler_fr to get "integrated" consumption
for (i in 1:7) {
  Euler_eaten[i] <- Euler_fr(prey,birds,sr,ht,step_sz[i],steps[i])
}
#plot the estimate versus number of steps
plot(Euler_eaten~steps, type='l', ylim=c(0,15))

# For this model and parameters, the estimate levels off at < 50 steps
# This is for a case where annual consumption is a high fraction of 
# prey supply
# Try another case with many fewer predators (e.g.,10)

#Euler function for functional response
Euler_fr <- function(prey_init,birds,sr,ht,step_sz,steps) {
  for (i in 1:steps) {
    consump <- sr*prey/(1+sr*ht*prey)*birds*step_sz
    prey <- prey - consump  #adjust prey numbers based on consumption this step
    eaten <- eaten + consump  # accumulate total consumption across steps
  }
  eaten
}
