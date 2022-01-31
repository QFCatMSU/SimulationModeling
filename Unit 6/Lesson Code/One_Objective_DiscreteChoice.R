#Unit 6: Simple Decision - one objective, discrete choices
#
# This is the Bull trout model with catch per trip modeled stochastically (Unit 3: Poisson_NB_catch.R) 

# We use this model to examine the effect of a bag limit
# Objective is to maximize average catch in the final 50 years

tot_catch <- numeric(100)  # total catch, by year

# Define population variables
ages <- seq(1:15)
btrout <- array(0,dim=c(15,100))
adults <- numeric(100)
bt_len <- numeric(15)
bt_wt <- numeric(15)
mat_age <- 6
fec <- c(-61.288,1.478) 
vB <- c(80,.32,2.29)  
lw <- c(.01,3)       
natmort <- .2 
sr <-  c(.002,.00000125) #values from VB model, not Table 1 in Post et al

# Define fishery parameters
vf <- c(.3,1200)     
q <- 0.07  # catchability from Post et al paper (assume this is ha per angler-hr)

# Modifications needed for discrete catch per trip distribution
triplength <- 3.5  # hours per trip
lake_area <- 525   # ha of lake
q_corr <- q * triplength / lake_area  # q in units of 1/trips (i.e., adjust for trip length and area)
trips <- 500 #replace E (effort) with trips
release_fish <- numeric(trips)
bag_limit <- 1

hm <- 0.1  # hooking mortality
ncm <-0.1

# Calculate lengths and weights
bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth
bt_len[1] <- 7.3 #linear growth age 1 and 2 
bt_len[2] <- 11.8
bt_wt <- lw[1]*bt_len^lw[2]

# and calculate age-specific vulnerability
v <- (1 - exp(-vf[1]*bt_len))^vf[2]


# initialize numbers
age0 <- 2000
btrout[1,1] <- 1000
for (j in 2:15) {
  btrout[j,1] <- btrout[j-1,1]*(1-natmort)
}
adults[1] <- sum(btrout[6:15,1])

# Start a loop over time
for (i in 1:99)  {
  # Calculate average catch per trip
  catch_age <- q_corr * btrout[,i]*v  # by age
  avg_catch <- sum(catch_age)         # all ages
  # Generate a distribution of catches
  catch_by_trip_P <- rpois(trips,avg_catch)  # Poisson
  catch_by_trip <- rnbinom(trips, size=.5, mu=avg_catch) # Negative binomial
  # Check each trip against the bag limit
  for (k in 1:trips) { # sets fish to release if catch > bag
    release_fish[k] <- max(0,catch_by_trip[k]-bag_limit)
  } 
  kept_by_trip <- catch_by_trip - release_fish  # for each trip
  # Adjust catches to reflect trips where bag limit exceeded
  kept_catch <- sum(kept_by_trip)  #sum over trips
  catch_reduce <- kept_catch/avg_catch  #proportion kept
  catch_age_kept <- catch_age * catch_reduce  # numbers kept by age
  tot_catch[i] <- sum(catch_age_kept)  # total catch, all ages
  # Calculate fish deaths due to hooking mortality
  release_death <- catch_age * (1-catch_reduce) * hm

  # Calculate total deaths, use non-instantaneous rates, as per VisBasic model
  deaths <- natmort * btrout[,i] + catch_age_kept + release_death
  
  # Apply tautology
  for (j in 15:2) {
    btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1] 
    btrout[1,i+1] <- age0 * (1 - natmort)  #first year losses, no fishing
  } 
  adults[i+1] <- sum(btrout[6:15,i+1])
  # Calculate population egg production
  eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
  
  # Calculate recruitment using Bev-Holt model
  age0 <- sr[1]*eggs/(1+sr[2]*eggs)
  
# End time loop
}
# Compute objective value
objective <- mean(tot_catch[50:99])
