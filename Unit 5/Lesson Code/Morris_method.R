#Unit 5 - Implementation of the Morris method for sensitivity analysis
#  Using the bull trout model
library(ggplot2)

# Define population variables
ages <- seq(1:15)
btrout <- array(0,dim=c(15,100))
adults <- numeric(100)
bt_len <- numeric(15)
bt_wt <- numeric(15)
mat_age <- 6

#Create a single vector with all parameters
xi <- numeric(16)
xi[1:2] <- c(.01,3) #lw
xi[3] <- .2 #natmort
xi[4:5] <- c(.3,1200) #vf
xi[6] <- 0.07 #q
xi[7] <- 3.5 #triplength
xi[8] <- .1 #hm
xi[9] <- .1 #ncm
xi[10:11] <- c(-61.288,1.478) #fec_pars
xi[12:14] <- c(80,.32,2.29) #vB_pars
xi[15:16] <- c(.002,.00000125) #sr_pars

# create a grid of parameter values
xi_grid <- array(0,dim=c(16,5))
xi_levels <- c(.9,.95,1,1.05,1.1)
xi_grid <- outer(xi,xi_levels)

# loop over replicate "tranjectories"
ee <- array(0,dim=c(40,16))
for (irep in 1:40) {
  
#randomly choose a starting cell in the 16x5 grid
ref_col <- sample(1:5,16, replace=TRUE)
  for (i in 1:16) {
  xi_ref[i] <- xi_grid[i,ref_col[i]]
}

#run the model and save the output
# create a function that runs the model, with a call to
# the current parameter set (see bt_model())

# Loop over all the parameters, in a random order
pick_par <- sample(1:16,16,replace=F)
for (ipar in 1:16) {
  #index for parameters to change:
  jpar <- pick_par[ipar]
  # get model result for the initial set of parameters
  out_1 <- bt_model(xi_ref)
  #change one parameter by one level
  #increase by one level unless ref_col is 5, then decrease
  if (ref_col[jpar] < 5) {
    xi_ref[jpar] <- xi_grid[jpar,ref_col[jpar]+1]
  }
  else {
    xi_ref[jpar] <- xi_grid[jpar,ref_col[jpar]-1]
  }
  # run the model again, calculate the elementary effect, and save 
  #  the elementary effect for that parameter
  out_2 <- bt_model(xi_ref)
  ee[irep,jpar] <- (out_2 - out_1)/0.05  # delta is 5%
# Finish the loop over paramters
}
# Repeat r times
}
# Create a data frame for the summary statistics
ee_summ <- setNames(data.frame(matrix(ncol=4,nrow=16)),
                    c("Parameter","EE_mean", "EE_mean_star", "EE_variance"))
# Calculate mean, mean(abs) and std dev of elementary effects
ee_summ$Parameter <- seq(1:16)
ee_summ$EE_mean <- colMeans(ee)
ee_summ$EE_mean_star <- colMeans(abs(ee))
ee_summ$EE_variance <- apply(ee,2, FUN='var')
ee_summ$EE_cv <- sqrt(ee_summ$EE_variance)/ee_summ$EE_mean_star

ee_plot <- ggplot(data=ee_summ, aes(x=EE_mean_star,y=EE_variance)) +
  geom_point() +
  geom_text(aes(label=Parameter),size=4, vjust=1, hjust=0) +
  theme_bw()
ee_plot
# Model function:
bt_model <- function(xi) {
  # Modifications needed for discrete catch per trip distribution
  lake_area <- 525   # ha of lake
  q_corr <- xi[6]*xi[7] / lake_area  # q in units of 1/trips (i.e., adjust for trip length and area)
  trips <- 150 #replace E (effort) with trips
  release_fish <- numeric(trips)
  bag_limit <- 1
  dist_catch <- 'P'  # P means Poisson, NB means Negative Binomial
  # Calculate lengths and weights
  bt_len <- xi[12] * (1-exp(-xi[13]*(ages-xi[14])))  #VonB growth
  bt_len[1] <- 7.3 #linear growth age 1 and 2 
  bt_len[2] <- 11.8
  bt_wt <- xi[1]*bt_len^xi[2]

  # and calculate age-specific vulnerability
  v <- (1 - exp(-xi[4]*bt_len))^xi[5]

  #Initialize numbers
  age0 <- 2000
  btrout[1,1] <- 1000
  for (j in 2:15) {
    btrout[j,1] <- btrout[j-1,1]*(1-xi[3])
  }
  adults[1] <- sum(btrout[6:15,1])

  # Start a loop over time
  for (i in 1:99)  {
  #Calculate average catch per trip
    catch_age <- q_corr * btrout[,i]*v  # by age
    avg_catch <- sum(catch_age)         # all ages
    #Generate a distribution of catches
    if (dist_catch=='P') catch_by_trip <- rpois(trips,avg_catch)  # Poisson
    if (dist_catch=='NB') catch_by_trip <- rnbinom(trips, size=.5, mu=avg_catch) # Negative binomial
    #Check each trip against the bag limit
    for (k in 1:trips) { # sets fish to release if catch > bag
      release_fish[k] <- max(0,catch_by_trip[k]-bag_limit)
    } 
    kept_by_trip <- catch_by_trip - release_fish  # for each trip
    #Adjust catches to reflect trips where bag limit exceeded
    kept_catch <- sum(kept_by_trip)  #sum over trips
    catch_reduce <- kept_catch/avg_catch  #proportion kept
    catch_age_kept <- catch_age * catch_reduce  # numbers kept by age
    #Calculate fish deaths due to hooking mortality
    release_death <- catch_age * (1-catch_reduce) * xi[8]
  
    #Calculate total deaths, use non-instantaneous rates, as per VisBasic model
    deaths <- xi[3] * btrout[,i] + catch_age_kept + release_death
  
    #Apply tautology
    for (j in 15:2) {
      btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1] 
      btrout[1,i+1] <- age0 * (1 - xi[3])  #first year losses, no fishing
    } 
    adults[i+1] <- sum(btrout[6:15,i+1])
    #Calculate population egg production
    eggs <- sum(btrout[mat_age:15,i]*(xi[10]+xi[11]*bt_wt[mat_age:15])*0.5)
  
    #Calculate recruitment using Bev-Holt model
    age0 <- xi[15]*eggs/(1+xi[16]*eggs)
  
    #End time loop
  }
return(adults[100])
  #End function
}

