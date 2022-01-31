#Unit 6 - Demonstration of a search for an optimal harvest policy for
#  bull trout model, using size limits and a harvest rate
library(ggplot2)

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
sr <-  c(.002,.00000125) 
vf <- c(.3,1200)    

#Data frame to save results for plotting
results <- setNames(data.frame(matrix(ncol=3,nrow=90)),
                    c("Min_length","Exp_rate","Harvest"))

# x1 is min length as fraction of l-infinity
# x2 is exploitation rate
x1 <- seq(0.1,0.9,.1)
x2 <- seq(.05,.5,.05)

# Run the model across a grid of policy values
irow <- 1
for (s in 1:9) {
  for (h in 1:10) {
    results$Min_length[irow] <- x1[s]
    results$Exp_rate[irow] <- x2[h]
    x <- c(x1[s],x2[h])
    results$Harvest[irow] <- -bt_model(x)
    irow <- irow + 1
}
}

#Contour plot of results
opt_harvest <- ggplot(data=results) +
  geom_contour_filled(mapping=aes(x=Min_length,y=Exp_rate,z=Harvest),
                      bins=10) +
  labs(title="Average long term harvest",
       x="Length limit as a proportion of max length",
       y="Exploitation rate",
       fill="Harvest/year") +
  theme_bw()
opt_harvest

optim(c(0.2,0.4),bt_model)  # first argument is starting values, second is model function


# Model function for optimization:
bt_model <- function(x) {
  
  bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth
  bt_len[1] <- 7.3 #linear growth age 1 and 2 
  bt_len[2] <- 11.8
  bt_wt <- lw[1]*bt_len^lw[2]
  
  # and calculate age-specific vulnerability
  v <- (1 - exp(-vf[1]*bt_len))^vf[2]

  # adjust for size limit
  # size limit is defined is fraction of L-infinity
  sl <- x[1] * vB[1]
  # adjust vulnerability for age 4 and older fish according to the proportion
  #  of that age class that would be below the size limit
  #  Assumption: variation in size at age has a cv of 20%
  for (il in 4:15) {
    v[il]<- 1-pnorm(sl,bt_len[il],.2*bt_len[il])
  }

  #Initialize numbers
  age0 <- 6000
  btrout[1,1] <- 4000
  for (j in 2:15) {
    btrout[j,1] <- btrout[j-1,1]*(1-natmort)
  }
  adults[1] <- sum(btrout[6:15,1])

  # Start a loop over time
  for (i in 1:99)  {
    # Calculate catch by age and total catch, exploitation rate is x[2]
    U = x[2] 
    catch <- U * btrout[,i]*v
    catch_tot <- sum(catch)
    
    # Calculate total deaths, use non-instantaneous rates, as per VisBasic model
    deaths <- natmort * btrout[,i] + catch
    
    # Apply tautology
    for (j in 15:2) {
      btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1]
      if (btrout[j,i+1]<0) btrout[j,i+1] <- 0
    }
      btrout[1,i+1] <- age0 * (1 - natmort)  #first year losses, no fishing
    
    # Calculate population egg production
    eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
    
    # Calculate recruitment using Bev-Holt model
    age0 <- sr[1]*eggs/(1+sr[2]*eggs)
    
    # End time loop
  }
return(-catch_tot)  #return negative value to get minimum
  #End function
}

