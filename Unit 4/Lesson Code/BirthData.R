# Get data for cormorant birth rates and plot what it looks like

library(ggplot2)

br_data <- read.csv(file="../Data/BirthData.csv", header=T)

plotdata <- ggplot(data=br_data) +
  geom_point(mapping=aes(x=Numbers, y=Obs_br)) +
  geom_line(mapping=aes(x=Numbers, y=Deterministic)) +
  labs(title="Cormorant Birth Rate Data",
       x="Cormorant abundance", y="Birth Rate (fledglings per nest)") +
  theme_bw()
plotdata

# Fit a linear model to the data:

br_data$lnbr <- log(br_data$Obs_br)
linearfit <- lm(lnbr~Numbers, data=br_data)
summary(linearfit)

# Fit a non-linear model to the data:

nlfit <- nls(Obs_br ~ a * exp(-b*Numbers), data=br_data, start=list(a=.5,b=.05))
summary(nlfit)

# plot model fits to data
br_data$linear <- exp(predict(linearfit))*exp(.4359^2/2)  #need to bias-correct estimates
br_data$nonlin <- predict(nlfit)
plotfits <- ggplot(data=br_data) +
  geom_point(mapping=aes(x=Numbers, y=Obs_br)) +
  geom_line(mapping=aes(x=Numbers, y=linear), color='blue') +
  annotate(geom="text", x=58, y=.9, hjust=0, label='linear', color='blue') +
  geom_line(mapping=aes(x=Numbers, y=nonlin), color='red') +
  annotate(geom="text", x=400, y=.27, hjust=0, label='non-linear', color='red') +
  labs(title="Cormorant Birth Rate Data",
       x="Cormorant abundance", y="Birth Rate (fledglings per nest)") +
  theme_bw()
plotfits
