#Bull trout model, based on Post et al. 2003. NAJFM 23:22-34
# This code is much the same as for Assignment 2.5b, but with modifications to 
# allow angler catch to be modeled as distributed among anglers using a discrete 
# distribution: Poisson or negative binomial, and then examine the effect
# of a bag limit
library(ggplot2)
#Steps 1 and 2: Define population variables
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

#Step 3: Define fishery parameters
vf <- c(.3,1200)     
q <- 0.07  # catchability from Post et al paper (I think this is ha per angler-hr)

# Modications needed for discrete catch per trip distribution
triplength <- 3.5  # hours per trip
lake_area <- 525   # ha of lake
q_corr <- q * triplength / lake_area  # q in units of 1/trips (i.e., adjust for trip length and area)

hm <- 0.1  # hooking mortality
ncm <-0.1
trips <- 150 #replace E (effort) with trips
release_fish <- numeric(trips)
bag_limit <- 2
  
#Step 4: Calculate lengths and weights
bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth
bt_len[1] <- 7.3 #linear growth age 1 and 2 
bt_len[2] <- 11.8
bt_wt <- lw[1]*bt_len^lw[2]

# and calculate age-specific vulnerability
v <- (1 - exp(-vf[1]*bt_len))^vf[2]


#Step 5: initialize numbers
age0 <- 2000
btrout[1,1] <- 1000
for (j in 2:15) {
  btrout[j,1] <- btrout[j-1,1]*(1-natmort)
}
adults[1] <- sum(btrout[6:15,1])
#Step 6: Start a loop over time
for (i in 1:99)  {
  #Steps 7a-e and 8 are modified to model catch per trip as a discrete distibution among trips
  #Step 7a: Calculate average catch per trip
  catch_age <- q_corr * btrout[,i]*v  # by age
  avg_catch <- sum(catch_age)         # all ages
  #Step 7b: Generate a distribution of catches
  catch_by_trip <- rpois(trips,avg_catch)  # Poisson
  catch_by_trip_NB <- rnbinom(trips, size=.5, mu=avg_catch) # Negative binomial
  #Step 7c: Check each trip against the bag limit
  for (k in 1:trips) { # sets fish to release if catch > bag
    release_fish[k] <- max(0,catch_by_trip[k]-bag_limit)
  } 
  kept_by_trip <- catch_by_trip - release_fish  # for each trip
  #Step 7d: Adjust catches to reflect trips where bag limit exceeded
  kept_catch <- sum(kept_by_trip)  #sum over trips
  catch_reduce <- kept_catch/avg_catch  #proportion kept
  catch_age_kept <- catch_age * catch_reduce  # numbers kept by age
  #Step 7e: Calculate fish deaths due to hooking mortality
  release_death <- catch_age * (1-catch_reduce) * hm

  #Step 8: Calculate total deaths, use non-instantaneous rates, as per VisBasic model
  deaths <- natmort * btrout[,i] + catch_age_kept + release_death
  
  #Step 9: Apply tautology
  for (j in 15:2) {
    btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1] 
    btrout[1,i+1] <- age0 * (1 - natmort)  #first year losses, no fishing
  } 
  adults[i+1] <- sum(btrout[6:15,i+1])
  #Step 10: Calculate population egg production
  eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
  
  #Step 11: Calculate recruitment using Bev-Holt model
  age0 <- sr[1]*eggs/(1+sr[2]*eggs)
  
#Step 12: End time loop
}

#Step 13: Plot the results
#Combine Poisson and NB distributions into a single data frame
dtype <- rep('P', trips) # category lables for each distribution
dtypeNB <- rep('NB', trips)
catch_dists <- data.frame(dtype,catch_by_trip)
catch_dists <- rbind(catch_dists, data.frame(dtype=dtypeNB,catch_by_trip=catch_by_trip_NB))

rowlab <- c(NB="Neg Binomial",P="Poisson") # labels for facets
Poisson <- ggplot(data=catch_dists) +
  geom_histogram(mapping = aes(x=catch_by_trip),
                 binwidth = 1, fill='grey') +
  facet_grid(dtype ~ .,
             labeller = labeller(dtype=rowlab)) +
  labs(title="Catch by Trip - Poisson vs Neg Binomial",
       x="Catch",
       y="Trips") +
  theme_bw()
plot(Poisson)

