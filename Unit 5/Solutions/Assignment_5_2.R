#Unit 5: Assignment 5.2
#
# This assignment involves modifying the means, variances and
#  covariances for two uncertain relationships used in
#  "Uncertainty_Analysis_BullTrout.R" and then repeating the
#  uncertainty analysis used there

library(mvtnorm)
library(ggplot2)
library(GGally)
library(QuantPsyc)


# Define population variables
ages <- seq(1:15)
btrout <- array(0,dim=c(15,100))
adults <- numeric(100)
bt_len <- numeric(15)
bt_wt <- numeric(15)
mat_age <- 6
lw <- c(.01,3)       
natmort <- .2 

# Define fishery parameters
vf <- c(.3,1200)     
q <- 0.07  # catchability from Post et al paper (assume this is ha per angler-hr)

# Modifications needed for discrete catch per trip distribution
triplength <- 3.5  # hours per trip
lake_area <- 525   # ha of lake
q_corr <- q * triplength / lake_area  # q in units of 1/trips (i.e., adjust for trip length and area)
trips <- 150 #replace E (effort) with trips
release_fish <- numeric(trips)
bag_limit <- 1
dist_catch <- 'P'  # P means Poisson, NB means Negative Binomial

hm <- 0.1  # hooking mortality
ncm <-0.1

# Uncertain model parameters
fec_means <- c(-61.288,1.478)
fec_vcov <- array(c(36,0,0,.02),dim=c(2,2))
#These  estimates have been replaced with estimates from actual data
# see the table in the Unit 5 notes for this assignment
vB_means <- c(101.3,.057,-1.35)
vB_vcov <- array(c(276.8,0,0,0,.00023,0,0,0,.229),dim=c(3,3))
sr_means <-  c(.002,1.25e-6) 
sr_vcov <- array(c(4e-6,0,0,1.56e-12),dim=c(2,2))

# Create a data frame to save the results
nsims <- 1000
outputs <- setNames(data.frame(matrix(ncol=8,nrow=nsims)),
                     c('adults','vB_1','vB_2','vB_3','fec_1','fec_2',
                       'sr_1','sr_2'))
# Loop over simulations
for (isims in 1:nsims) {
  # Get the parameter values for this simulation
  vB <- rmvnorm(1, mean=vB_means, sigma=vB_vcov)
  fec <- rmvnorm(1, mean=fec_means, sigma=fec_vcov)
  sr <- rmvnorm(1, mean=sr_means,sigma=sr_vcov)

  
  # Calculate lengths and weights
  bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth
  bt_len[1] <- 7.3 #linear growth age 1 and 2 
  bt_len[2] <- 11.8
  bt_wt <- lw[1]*bt_len^lw[2]

  # and calculate age-specific vulnerability
  v <- (1 - exp(-vf[1]*bt_len))^vf[2]


  #Initialize numbers
  age0 <- 2000
  btrout[1,1] <- 1000
  for (j in 2:15) {
    btrout[j,1] <- btrout[j-1,1]*(1-natmort)
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
    if (avg_catch > 0) {  # trap to protect simulations with avg_catch = 0
      catch_reduce <- kept_catch/avg_catch  # proportion kept
    }
    else {
      catch_reduce <- 0
    }
    catch_age_kept <- catch_age * catch_reduce  # numbers kept by age
    #Calculate fish deaths due to hooking mortality
    release_death <- catch_age * (1-catch_reduce) * hm
  
    #Calculate total deaths, use non-instantaneous rates, as per VisBasic model
    deaths <- natmort * btrout[,i] + catch_age_kept + release_death
    
    #Apply tautology
    for (j in 15:2) {
      btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1]
      if (btrout[j,i+1]<0) btrout[j,i+1] = 0
      btrout[1,i+1] <- age0 * (1 - natmort)  #first year losses, no fishing
    } 
    adults[i+1] <- sum(btrout[6:15,i+1])
    #Calculate population egg production
    eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
    
    #Calculate recruitment using Bev-Holt model
    age0 <- sr[1]*eggs/(1+sr[2]*eggs)
    if (age0<0) age0 <- 0  # trap to protect against negative sr[1] values
  #End time loop
  }
# Store results here for post-processing
outputs[isims,] <- c(adults[100], vB,fec,sr)

# Close loop over simulations
  

}

# Post processing - correlations of outputs with inputs
cor(outputs[1:nsims,])[,1]

# Plot sensitivity in descending order
sens <- cor(outputs[1:nsims,])[2:8,1]
sens_ord <- as.numeric(sens[order(abs(sens),decreasing = TRUE)])
sens_plot <- as.data.frame(cbind(sens_ord, rank(abs(sens_ord))))
sens_labels <- names(sens[order(abs(sens))])
Plot_sens <- ggplot(data=sens_plot) +
  geom_col(mapping=aes(y=sens_ord,x=V2)) +
  labs(title="Tornado plot",
       x="Uncertain parameter",
       y="Correlation with output variability") +
  scale_x_continuous(breaks=1:7,labels=c(sens_labels)) +
  coord_flip() +
  theme_bw()
Plot_sens

# Scatter plot matrix of results
ggpairs(outputs[1:nsims,], title="Scatter plot matrix of uncertain parameters")

# Regression analysis to look for interactions
model1 <- lm(adults ~ vB_1+vB_2+vB_3+fec_1+fec_2+sr_1+sr_2, data=outputs)
std_model1 <- lm.beta(model1)

cor2cov_1 <- function(R,S){
  diag(S) %*% R %*% diag(S)
}


#The results are much more variable among simulations, mainly due to the higher
# variance for the s-r relationship. Because of this, the correlations between
# the parameter values and the model output are much smaller. As well
# the relative importance of the different parameters is not the same:
# sr_2 is now the most important parameter, and vb_1 is much less important.