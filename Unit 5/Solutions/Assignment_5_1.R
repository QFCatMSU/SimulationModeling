#Assignment 5.1 - modified sensitivity analysis:

# We change the output metric to be the years to 95% of the equilibrium (final) value for birds,
# instead of the actual final value


library(ggplot2)

popmodel <- function(a,b,c,d) {
  # Start the main simulation loop over time
  # Define starting values for state variables
  ###  Need a vector of birds over time now, so we can determine when N get to 
  ###    95% of N-final
  birds <- numeric(100)
  birds[1] <- 20
  for (i in 1:99) {   # we will run the model for 100 time steps
    
    # Calculate the birth and death rates according to our dynamic processes
    births_per_female <- a * exp(-b * birds[i])
    deaths_per_capita <- c * birds[i] / (d + birds[i])
    
    # Convert rates to actual numbers
    births <- births_per_female * birds[i] / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds[i]
    
    # Apply the tautology
    birds[i+1] <- birds[i] + births - deaths
    
    #close the time loop
    
    #Find the earliest year when numbers are at least 95% of the max
    year_95 <- min(which(birds >= 0.95*birds[100]))
  }  
  return(year_95)
}

#Nominal parameter values
a_n <- 1
b_n <- .0046
c_n <- 1
d_n <- 50

#Generate result for nominal values
base <- popmodel(a_n,b_n,c_n,d_n)
sens <- numeric(4)

#Loop over four parameters
# Sensitivity = abs((output difference/nominal output)/(input difference/nominal input))
for (ipar in 1:4) {
  if (ipar==1) {
    a <- a_n + 0.05 * a_n
    sens[ipar] <- abs(((popmodel(a,b_n,c_n,d_n)-base)/base)/((a_n-a)/a_n)) }
  else if (ipar==2) {
    b <- b_n + 0.05 * b_n
    sens[ipar] <- abs(((popmodel(a_n,b,c_n,d_n)-base)/base)/((b_n-b)/b_n)) }
  else if (ipar==3) {
    c <- c_n + 0.05 * c_n
    sens[ipar] <- abs(((popmodel(a_n,b_n,c,d_n)-base)/base)/((c_n-c)/c_n)) }
  else {
    d <- d_n + 0.05 * d_n
    sens[ipar] <- abs(((popmodel(a_n,b_n,c_n,d)-base)/base)/((d_n-d)/d_n)) }
  
}

#With the new output criterion, the only sensitive parameter is c;
# the others show no change in output from a 5% in parameter value
sens

plot_sens <- ggplot() +
  geom_bar(mapping = aes(x=c("a","b","c","d"),y=sens),
                         stat='identity') +
  labs(x='Parameter', y="Sensitivity")
plot_sens

