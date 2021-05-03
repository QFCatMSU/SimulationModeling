#Unit 3 - Generate and plot probability distributions for Figures

library(ggplot2)

#1 - Uniform
min <- 0.75
max <- 1.25
x <- runif(10000,min=min,max=max)
obs_mean_U <- mean(x)
obs_stdev_U <- sqrt(var(x))
breaks <- seq(0.75,1.25,.01)
uniform <- ggplot() +
  geom_histogram(mapping=aes(x=x),
                 breaks=breaks, fill='grey') +
  labs(title='U(.75,1.25) distribution sample of 10,000',
       x="Value", y="Count") +
  annotate(geom="text", x=1, y=150, hjust=0,
           label=paste('mean =',signif(obs_mean_U,3), sep=" ")) +
  annotate(geom="text", x=1, y=125, hjust=0,
           label=paste('std dev = ',signif(obs_stdev_U, 3), sep=" ")) +
  theme_bw()
plot(uniform)

#2 - Normal
mean <- 3000
stdev <- 1200
x <- rnorm(10000,mean=mean,sd=stdev)
obs_mean_N <- mean(x)
obs_stdev_N <- sqrt(var(x))
normal <- ggplot() +
  geom_histogram(mapping=aes(x=x),
                 fill='grey') +
  labs(title='N(3000,1200) distribution sample of 10,000',
       x='Value',y='Count') +
  annotate(geom="text", x=5000, y=700, hjust=0,
           label=paste('mean =',signif(obs_mean_N,3), sep=" ")) +
  annotate(geom="text", x=5000, y=600, hjust=0,
           label=paste('std dev = ',signif(obs_stdev_N, 3), sep=" ")) +
  theme_bw()
plot(normal)

#3 - Lognormal
a <- 1
b <- 0.0046
mean <- 0
stdev <- .2
br_dev <- rnorm(10000,mean=mean,sd=stdev) 
br <- a * exp(-b*30 + br_dev)  #br_dev is in exponent so log-normal error, use 30,000 birds
lognorm <- ggplot() +
  geom_histogram(mapping=aes(x=br),
                 fill='grey', binwidth = .05) +
  labs(title="Log-normal distribution sample of 10,000 birth rates",
       x='Value', y='Count') +
  coord_cartesian(xlim=c(0,2)) +
  annotate(geom="text", x=1.2, y=700, hjust=0,
           label=paste('mean birth rate =',signif(mean(br),3), sep=" ")) +
  annotate(geom="text", x=1.2, y=600, hjust=0,
           label=paste('std deviation = ',signif(sqrt(var(br)), 3), sep=" ")) +
  theme_bw()
plot(lognorm)

#4 - Binomial
# Number of "successes" in a certain number of tries, with a given probability
tries <- 10
prob_success <- .5
x <- rbinom(10000,size=tries,prob=prob_success)
mean_successes <- mean(x)
sd_successes <- sqrt(var(x))
binom <- ggplot() +
  geom_histogram(mapping=aes(x=x),
                 fill='grey') +
  labs(title="Binom(10,.5) distribution of successes on 10 trials, n=10,000",
       x='Successes', y='Count') +
  coord_cartesian(xlim=c(0,10)) +
  annotate(geom="text", x=7, y=2000, hjust=0,
           label=paste('mean =',signif(mean_successes,3), sep=" ")) +
  annotate(geom="text", x=7, y=1800, hjust=0,
           label=paste('sd = ',signif(sd_successes, 3), sep=" ")) +
  theme_bw()
plot(binom)

#5 - Poisson
mean <- 5
x <- rpois(10000,lambda = mean)
obs_mean_P <- mean(x)
obs_var_P <- var(x)
Poisson <- ggplot() +
  geom_bar(mapping=aes(x=x),
                 fill='grey') +
  labs(title="Poisson distribution with mean=5, n=10,000",
        y='Count',x='Value') +
  annotate(geom="text", x=7, y=2000, hjust=0,
           label=paste('mean =',signif(obs_mean_P,3), sep=" ")) +
  annotate(geom="text", x=7, y=1800, hjust=0,
           label=paste('var = ',signif(obs_var_P, 3), sep=" ")) +
  theme_bw()
plot(Poisson)

#6 - Negative binomial
mean <- 5
disp <- 2
x <- rnbinom(10000,mu = mean, size = disp)
obs_mean_NB <- mean(x)
obs_var_NB <- var(x)
NBinom <- ggplot() +
  geom_bar(mapping=aes(x=x),
           fill='grey') +
  labs(title="Neg binomial distribution with mean=5, disp=2, n=10,000",
       y='Count',x='Value') +
  annotate(geom="text", x=7, y=1300, hjust=0,
           label=paste('mean =',signif(obs_mean_NB,3), sep=" ")) +
  annotate(geom="text", x=7, y=1200, hjust=0,
           label=paste('var = ',signif(obs_var_NB, 3), sep=" ")) +
  theme_bw()
plot(NBinom)

#7 - gamma
sh <- 2   # gamma shape
sc <- 3   # gamma scale
gamma_mean <- sh * sc
gamma_var <- sh * sc^2
x <- rgamma(10000, shape=sh, scale=sc)
obs_mean_G <- mean(x)
obs_var_G <- var(x)
gamma <- ggplot() +
  geom_histogram(mapping=aes(x=x),
                 fill='grey') +
  labs(title='gamma distribution: shape 2, scale 3, sample of 10,000',
       x='Value',y='Count') +
  annotate(geom="text", x=20, y=700, hjust=0,
           label=paste('mean =',signif(obs_mean_G,3), sep=" ")) +
  annotate(geom="text", x=20, y=600, hjust=0,
           label=paste('var = ',signif(obs_var_G, 3), sep=" ")) +
  theme_bw()
plot(gamma)

#8 - beta
sh1 <- 2
sh2 <- 3
x <- rbeta(10000,shape1=sh1,shape2 = sh2)
obs_mean_B <- mean(x)
obs_var_B <- var(x)
beta <- ggplot() +
  geom_histogram(mapping=aes(x=x),
                 fill='grey',
                 binwidth = .05) +
  labs(title='beta distribution: shape1=2, shape2=3, sample of 10,000',
       x='Value',y='Count') +
  annotate(geom="text", x=.7, y=700, hjust=0,
           label=paste('mean =',signif(obs_mean_B,3), sep=" ")) +
  annotate(geom="text", x=.7, y=600, hjust=0,
           label=paste('var = ',signif(obs_var_B, 3), sep=" ")) +
  theme_bw()
plot(beta)


