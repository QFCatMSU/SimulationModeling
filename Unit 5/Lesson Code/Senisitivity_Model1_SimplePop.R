# In this script we conduct a sensitivity analysis of the four parameters for
# the first model we looked at: cormorant population dynamics with no culling

# We will consider the final abundance as the output of interest
# for the sensitivity analysis

# Sensitivity = abs((output difference/nominal output)/(input difference/nominal input))

# We will vary each input parameter by + or - 5% and store the final 
# abundance for each case

# We will turn the model into a function and call the function with a different
# set of parameter values each time, returning the final abundance
library(ggplot2)

popmodel <- function(a,b,c,d) {
  # Start the main simulation loop over time
  # Define starting values for state variables
  birds <- 20
  for (i in 1:100) {   # we will run the model for 100 time steps
    
    # Calculate the birth and death rates according to our dynamic processes
    births_per_female <- a * exp(-b * birds)
    deaths_per_capita <- c * birds / (d + birds)
    
    # Convert rates to actual numbers
    births <- births_per_female * birds / 2  # divide by 2 because per female
    deaths <- deaths_per_capita * birds
    
    # Apply the tautology
    birds <- birds + births - deaths
    
    #close the time loop
  }  
  return(birds)
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

#The results show that the model is most sensitive to a 5% change
# in the a and c parameters, and least sensitive to a 5% change in the b
sens

plot_sens <- ggplot() +
  geom_bar(mapping = aes(x=c("a","b","c","d"),y=sens),
                         stat='identity') +
  labs(x='Parameter', y="Sensitivity")
plot_sens

