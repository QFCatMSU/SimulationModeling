#Bull trout model, based on Post et al. 2003. NAJFM 23:22-34
# The version of the model below accurately reproduces the results from the Excel/VB model
# shared with me by John Post, where I varied mortality from .05 to .35 and recorded
# final adult population size. The figure is slightly different from Figure 1 in the Post
# et al paper, but qualitatively the same.

# Looking at the code for the Excel/VB model, natural mortality appears
# to be modeled using annual rates, not instantaneous rates as described
# in the publication 
# Also, the stock-recruitment parameters they used in their code were
# not the same as reported in the ms - this makes only a small difference

# This is a version of the model for Assignment 2.5
# Angler effort is fixed, not dynamic

#Steps 1 and 2: Define population variables
ages <- seq(1:15)
btrout <- array(0,dim=c(15,100))
bt_len <- numeric(15)
bt_wt <- numeric(15)
mat_age <- 6
fec <- c(-61.288,1.478) # should second parameter be per g or per kg
vB <- c(80,.32,2.29)  # checked
lw <- c(.01,3)       # checked
natmort <- .2 # c(0.05,0.1,0.15,0.2,0.25,0.3,0.35) #range of mortality values (not instantaneous rates)
sr <-  c(.002,.00000125) # c(.00332, 2.075e-6) #values from VB model

#Step 3: Define fishery parameters
vf <- c(.3,1200)     # checked
q <- 0.07
hm <- 0.1
ncm <-0.1
E <- c(0,1,2,3,4,5,6)   #initial effort at zero, later make this a vector
  
#Step 4: Calculate lengths and weights
bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))  #VonB growth
bt_len[1] <- 7.3 #linear growth age 1 and 2 
bt_len[2] <- 11.8
bt_wt <- lw[1]*bt_len^lw[2]

# and calculate age-specific vulnerability
v <- (1 - exp(-vf[1]*bt_len))^vf[2]

#For assignment 2.5: Add a loop over effort as per assignment task
final_pop <- numeric(7)
for (m in 1:7) {

#Step 5: initialize numbers
age0 <- 6000
btrout[1,1] <- 4000
for (j in 2:15) {
  btrout[j,1] <- btrout[j-1,1]*(1-natmort)
}

#Step 6: Start a loop over time
for (i in 1:99)  {
  
  #Step 7: Calculate catch by age and total catch
  catch <- q * E[m] * btrout[,i]*v
  catch_tot <- sum(catch)
  
  #Step 8: Calculate total deaths, use non-instantaneous rates, as per VisBasic model
  deaths <- natmort * btrout[,i] + catch
  
  #Step 9: Apply tautology
  for (j in 15:2) {
    btrout[j,i+1] <- btrout[j-1,i] - deaths[j-1] 
    btrout[1,i+1] <- age0 * (1 - natmort)  #first year losses, no fishing
  } 
  
  #Step 10: Calculate population egg production
  eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
  
  #Step 11: Calculate recruitment using Bev-Holt model
  age0 <- sr[1]*eggs/(1+sr[2]*eggs)
  
#Step 12: End time loop
}

# Final year adult numbers
final_pop[m] <- sum(btrout[6:15,100])
}
#Step 13: Plot the results
final_pop
plot(final_pop~E,  type='l',)
