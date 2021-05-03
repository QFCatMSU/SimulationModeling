# Fit two different models to the same data

library(ggplot2)

br_data <- read.csv(file="../Data/BirthData.csv", header=T)
#convert birth rate to births
br_data$Births <- br_data$Obs_br*br_data$Numbers/2  # births per female

# Ricker model
nlfit1 <- nls(Births ~ a1 * Numbers * exp(-b1*Numbers), data=br_data, start=list(a1=1,b1=.005))
summary(nlfit1)

#Bev-Holt model
nlfit2 <- nls(Births ~ a2 * Numbers / (b2 + Numbers), data=br_data, start=list(a2=50,b2=100))
summary(nlfit2)

#Calculate model weights
BestAIC <- min(AIC(nlfit1),AIC(nlfit2))
DelAIC <- AIC(nlfit1,nlfit2)$AIC-BestAIC
Rel_Lik <- exp(-0.5*DelAIC)
ModelWeights <- Rel_Lik/sum(Rel_Lik)


# plot model fits to data
br_data$nonlin1 <- predict(nlfit1)
br_data$nonlin2 <- predict(nlfit2)
plotfits <- ggplot(data=br_data) +
  geom_point(mapping=aes(x=Numbers, y=Births)) +
  geom_line(mapping=aes(x=Numbers, y=nonlin1), color='blue') +
  annotate(geom="text", x=250, y=50, hjust=0, 
           label=paste('Ricker weight = ',signif(ModelWeights[1],3), sep=" "),
           color='blue') +
  geom_line(mapping=aes(x=Numbers, y=nonlin2), color='red') +
  annotate(geom="text", x=-20, y=40, hjust=0,
           label=paste('Bev-Holt \n weight =', signif(ModelWeights[2],3), sep=" "),
                                                       color='red') +
  labs(title="Cormorant Birth Data",
       x="Cormorant abundance", y="Births (total fledglings)") +
  theme_bw()
plotfits
