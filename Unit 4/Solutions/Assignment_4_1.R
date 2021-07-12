#Unit 4: Assignment 4.1 Using evidence to inform
#        the bull trout model

# This code is based on Poisson_NB_Catch.R

library(ggplot2)
library(mvtnorm)

# Define population variables
ages <- seq(1:15)
btrout <- array(0,dim=c(15,100))
adults <- numeric(100)
bt_len <- numeric(15)
bt_wt <- numeric(15)
mat_age <- 6
fec <- c(-61.288,1.478) 
# vB <- c(80,.32,2.29)  # we estimate this instead of assuming the value
lw <- c(.01,3)       
natmort <- .2 
# sr <-  c(.002,.00000125) #these will also be estimated

# Define fishery parameters
vf <- c(.3,1200)     
q <- 0.07  # catchability from Post et al paper (assume this is ha per angler-hr)
hm <- 0.1  # hooking mortality

# Modifications needed for discrete catch per trip distribution
triplength <- 3.5  # hours per trip
lake_area <- 525   # ha of lake
q_corr <- q * triplength / lake_area  # q in units of 1/trips (i.e., adjust for trip length and area)
trips <- 250 #replace E (effort) with trips
release_fish <- numeric(trips)
bag_limit <- 2

# Non compliance - here we assume the distribution is uniform
p_keep_low <- .2   # lower and upper bounds on probability of keeping fish 
p_keep_high <- .5  #   above the bag limit

# Read in the s-r and growth data
sr_data <- read.csv(file='../Data/S-R_data.csv', header=T)
gr_data <- read.csv(file='../Data/growth_data.csv', header=T)

# Estimate parameters from the data
# Stock-recruitment
sr_data$y <- log(sr_data$stoch_age0/sr_data$eggs)
sr_model <- nls(y~log(a/(1+b*eggs)),start=list(a=.005,b=.00003),
                data=sr_data)
# Growth
gr_model <- nls(stoch_len~a*(1-exp(-b*(pick_age-c))),
                start=list(a=65,b=.5,c=2), data=gr_data)

# Set up a replicate simulation loop, and bins for model outputs
nsims <- 100
nyears <- 100
final_abundance <- numeric(100)
for (isim in 1:nsims) {

  # Calculate lengths and weights (don't vary over time)
  # First select a set of von-B parameters
  vB <- rmvnorm(1,mean=coef(gr_model),
                      sigma=vcov(gr_model))
  # Get lengths, then weights for this simulation
  bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth equation
  bt_len[1] <- 7.3   #fix linear growth for age 1 and 2 
  bt_len[2] <- 11.8
  bt_wt <- lw[1]*bt_len^lw[2]
  
  # and calculate age-specific vulnerability
  v <- (1 - exp(-vf[1]*bt_len))^vf[2]


  # Initialize numbers
  age0 <- 2000
  btrout[1,1] <- 1000
  for (j in 2:15) {
    btrout[j,1] <- btrout[j-1,1]*(1-natmort)
  }
  adults[1] <- sum(btrout[6:15,1])
  
  # Choose a probability of non-compliance for this simulation
  p_keep <- runif(1,min=p_keep_low,max=p_keep_high)

  # Start a loop over time
  for (i in 1:(nyears-1))  {
    # Calculate average catch per trip
    catch_age <- q_corr * btrout[,i]*v  # by age
    avg_catch <- sum(catch_age)         # all ages
    # Generate a distribution of catches
    catch_by_trip <- rnbinom(trips, size=.5, mu=avg_catch) # Negative binomial
    # Check each trip against the bag limit
    for (k in 1:trips) { # sets fish to release if catch > bag
      release_fish[k] <- max(0,catch_by_trip[k]-bag_limit)
      # set release_fish to zero for this trip if non-compliance
      # use a Bernoulli trial to decide
      if (rbinom(1,1,p_keep)==1) release_fish[k] <- 0
    } 
    kept_by_trip <- catch_by_trip - release_fish  # for each trip
    # Adjust catches to reflect trips where bag limit exceeded
    kept_catch <- sum(kept_by_trip)  #sum over trips
    catch_reduce <- kept_catch/avg_catch  #proportion kept
    catch_age_kept <- catch_age * catch_reduce  # numbers kept by age
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
    lnRS <- log(coef(sr_model)[1]/(1+coef(sr_model)[2]*eggs))
    # add normal deviate
    lnRS <- lnRS + rnorm(1,0,sigma(sr_model))
    RS <- exp(lnRS)*exp(sigma(sr_model)^2/2)
    age0 <- unname(RS * eggs)
  
  # End time loop
  }
  final_abundance[isim] <- adults[i+1]
  # End replicate simulation loop
}

# Plot the distribution of final abundance values
breaks <- seq(0,4000,250)  # define bins for histogram using "breaks"
PlotNumbers <- ggplot() +
   geom_histogram(mapping = aes(x=final_abundance),
                 breaks=breaks) +
  labs(title="Distribution of forecasted bull trout abundance",
       x="Numbers",
       y="Count") +
  theme_bw()
PlotNumbers
