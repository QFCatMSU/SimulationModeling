#R Script developed by Jim Bence and adapted by MLJ 
#that explores the exponential model 
#with Euler numeric approximations to differential equation
#and compares to the analytical solution
#The model will be a simple exponential growth model: N(t+1)=N(t)+r*N(t)

#We use a function to run the Euler method for a variable number of steps and 
#step sizes for the exponential model
 Euler <-function(Ninit,rate,step_sz,steps){
 Nvals<-vector("numeric",length=steps+1)  # A vector with the value of N at each step
 Times <- vector("numeric", length=steps+1) # A vector with the point in time for each step
 Nvals[1]<-Ninit
 Times[1]<- 0
 for (i in 1:steps) {
 Nvals[i+1]<-Nvals[i]+Nvals[i]*rate*step_sz  #This is the Euler equation for the exp model
 Times[i+1]<- Times[i]+step_sz #Keep track of time steps
 }
 #output results as data frame
 data.frame(cbind(Nvals,Times))
 }
 
#Here's a call to the multi-step Euler function
 sz1 <- .01       #specify the step size and number of steps
 steps1 <- 100    #for one year sz * steps = 1 (assuming we're thinking about annual rates)
 N_over_time <- Euler(1,-1,sz1,steps1)
 plot(N_over_time$Nvals~N_over_time$Times, col='blue', ylim=c(0.2,1), type='l')
 N_over_time$Nvals[steps1+1]
#Now change step_sz and steps so their product is still 1 but step_sz is larger
 sz2 <- .2
 steps2 <- 5
 N_fewer_steps <- Euler(1,-1,sz2,steps2)
 points(N_fewer_steps$Nvals~N_fewer_steps$Times, col='red')
 N_fewer_steps$Nvals[steps2+1]
#Compare to analytical solution N(t+1) = N(t) * exp(-r*t)
 timex <- seq(.1,1,.1)  # analytical solution at ten steps through a single year
 correct <- 1 * exp(-1*timex)
 points(correct~timex, pch=17)  # filled triangles
 
 (N_over_time$Nvals[steps1+1]-correct[10])/correct[10]     #proportional difference from true value
 (N_fewer_steps$Nvals[steps2+1]-correct[10])/correct[10]
 
# Note that the proportional difference is very small for 100 steps but over 10% for 5 steps
# Goal is to find smallest number of steps that give an OK proportional difference
 
