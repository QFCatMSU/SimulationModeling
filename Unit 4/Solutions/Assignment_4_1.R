#Unit 4: Assignment 4.1 - Using evidence to inform
#        the bull trout model and evaluate bag limits

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
# vB <- c(80,.32,2.29)  # we now estimate this instead of assuming the value
lw <- c(.01,3)       
natmort <- .2 
# sr <-  c(.002,.00000125) # these will also be estimated

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

# ********************Assignment 4.1***********************************************

#  Elicited values for non compliance probability
# We will assume the distribution of non-compliance uncertainty is uniform
p_keep_low <- .2   # lower and upper bounds on probability of keeping fish 
p_keep_high <- .5  # above the bag limit

# Read in the stock-recruitment and growth data
sr_data <- read.csv(file='../Data/S-R_data.csv', header=T)
gr_data <- read.csv(file='../Data/growth_data.csv', header=T)

# Next we estimate parameters from the data
# 
# First, recruitment
# We're fitting the S-R data to the Ricker model:
#  Step 1: Create the y value for the linearized model (ln(R/S))
sr_data$y <- log(sr_data$Age0/sr_data$Eggs)
#  Step 2: Fit the model using lm  
sr_model <- lm(y~Eggs, data=sr_data)

# Second, growth
# We're fitting the growth data to the von-Bertalanffy function:
#  Step 1: Convert lengths from mm to cm, to be consistent with earlier versions of the model
gr_data$Length <- gr_data$Length_mm/10
#  Step 2: Fit the model using nls and the starting values provided in the assignment
gr_model <- nls(Length~a*(1-exp(-b*(Age-c))),
                start=list(a=65,b=.5,c=2), data=gr_data)

# ********************Assignment 4.1***********************************************

# Set up a replicate simulation loop, and bins for model outputs
nsims <- 500  # 100 simulations
nyears <- 100  # 100 years per simulation
final_abundance <- numeric(500) #save the final year abundance of adults
for (isim in 1:nsims) {
  
  # ********************Assignment 4.1***********************************************
    #  First select a set of von-B parameters from gr_model
    #  Using a multivariate normal distribution
    #  coef gives parameter point estimates, vcov is covariance matrix
  vB <- rmvnorm(1,mean=coef(gr_model),
                      sigma=vcov(gr_model))
  
  # Choose a probability of non-compliance for this simulation,
  #  based on a uniform distribution with elicited bounds
  p_keep <- runif(1,min=p_keep_low,max=p_keep_high)
  
  # ********************Assignment 4.1***********************************************
  
  # Use selected parameters to get lengths, then weights for this simulation
  bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth equation, using parameters from rmvnorm
  bt_len[1] <- 7.3   #fix lengths at age for age 1 and 2 
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
    if (avg_catch > 0) {  # trap to protect simulations with avg_catch = 0
      catch_reduce <- kept_catch/avg_catch  # proportion kept
    }
    else {
      catch_reduce <- 0
    }
    catch_age_kept <- catch_age * catch_reduce  # numbers kept by age
    # Calculate fish deaths due to hooking mortality
    release_death <- catch_age * (1-catch_reduce) * hm

    # Calculate total deaths, use non-instantaneous rates, as per VisBasic model
    deaths <- natmort * btrout[,i] + catch_age_kept + release_death
    
    # Apply tautology
    for (j in 15:2) {
      btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1]
      if (btrout[j,i+1]<0) btrout[j,i+1] = 0
      btrout[1,i+1] <- age0 * (1 - natmort)  #first year losses, no fishing
    } 
    adults[i+1] <- sum(btrout[6:15,i+1])
    # Calculate population egg production
    eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
    
    # ********************Assignment 4.1*********************************************** 
    # Calculate recruitment using estimated Ricker model parameters
    lnRS <- coef(sr_model)[1]+coef(sr_model)[2]*eggs
    # add normal deviate
    lnRS <- lnRS + rnorm(1,0,sigma(sr_model))
    age0 <- exp(lnRS)*eggs
    # ********************Assignment 4.1***********************************************

      # End time loop
  }
  # Save the final abundance of adults for each simulation
  final_abundance[isim] <- adults[i+1]
  # End replicate simulation loop
}

target_abundance <- 1000  # Assignment 4.1 - set target
sum(final_abundance<target_abundance)  # Count number of sims with N < target

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

#Results, bad outcomes out of 500 simulations:
# Bag limit = 2; Count = 100-110
# Bag limit = 3; Count = 135-145
# Bag limit = 1; Count = 45-60
