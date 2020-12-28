#Bull trout model, based on Post et al. 2003. NAJFM 23:22-34

# This is a version of the model that completes Assignment 2.5
# Angler effort is fixed, not dynamic

#Steps 1 and 2: Define population variables
ages <- seq(1:15)
btrout <- array(0,dim=c(15,100))
bt_len <- numeric(15)
bt_wt <- numeric(15)
mat_age <- 6
fec <- c(-61.3,1.48) # should second parameter be per g or per kg
vB <- c(80,.32,2.2)  # checked
lw <- c(.01,3)       # checked
M <- c(0.05,0.1,0.15,0.2,0.25,0.3,0.35)
sr <- c(.00332,2.075e-6) #checked

#Step 3: Define fishery parameters
vf <- c(.3,1200)     # checked
q <- 0.07
hm <- 0.1
ncm <-0.1
E <- 0   #initial effort at zero, later make this a vector

#Step 4: Calculate lengths and weights
bt_len <- vB[1] * (1-exp(-vB[2]*(ages-vB[3])))
# this function gives negative lengths for ages 1 and 2, which causes problems
# I arbitrarily adjusted them to 3, 10
bt_len[1] <- 3
bt_len[2] <- 10
bt_wt <- lw[1]*bt_len^lw[2]

#Step 4.6: Add a loop over M values as per assignment task
final_pop <- numeric(6)
for (m in 1:7) {

#Step 4.5: initialize numbers
btrout[1,1] <- 1000
for (j in 2:15) {
  btrout[j,1] <- btrout[j-1,1]*exp(-.2)
}


#Step 5: Start a loop over time
for (i in 1:99)  {
  #Step 6: Calculate population egg production
  eggs <- sum(btrout[mat_age:15,i]*(fec[1]+fec[2]*bt_wt[mat_age:15])*0.5)
  #Step 7: Calculate recruitment
  age0 <- sr[1]*eggs/(1+sr[2]*eggs)
  #Step 8: Calculate age-specific vulnerability
  v <- (1 - exp(-vf[1]*bt_len))^vf[2]
  #Step 9: Calculate catch by age and total catch
  catch <- q * E * btrout[,i]*v
  catch_tot <- sum(catch)
  #Step 10: Convert catches to instantaneous rates and add natural mort
  F_M <- -log(1-catch/btrout[,i])
  Z_Mort <- F_M + M[m]
  #Step 11: Apply tautology
  for (j in 15:2) {
    btrout[j,i+1] <- btrout[j-1,i] * (exp(-Z_Mort[j-1]))
    btrout[1,i+1] <- age0
  }
}
final_pop[m] <- sum(btrout[6:15,100])
}
#Step 12: Plot the results
