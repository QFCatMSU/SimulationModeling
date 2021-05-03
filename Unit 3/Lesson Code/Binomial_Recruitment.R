#Unit 3: Binomial distribution
# We will model fish recruitment modeled as a binomial process

# We will use a delay-difference (D-D) model (see Deriso 1980 - CJFAS 37:268-282) 
# for fish population dynamics based on Vasconelles' (2003, Fisheries Research 59:363-378)
# analysis of Brazilian sardine

library(ggplot2)

# D-D model parameters include survival, growth and recruitment
# we will follow Vasconelles' formulation and parameterization

M <- .95  # instantaneous natural mortality
h <- .75  # exploitation rate (fixed rather than modeled on effort)
alpha <- .025  # Walford growth alpha
rho <- .896    # Walford growth rho (equals exp(-K))
wr <- .025     # weight at recruitment
N <- numeric(100)  # numbers
B <- numeric(100)  # biomass
R <- numeric(100)  # recruitment

# initial numbers and biomass
N[1] <- 10   # billions of fish
B[1] <- .2   # SSB in millions of tons

for (i in 1:99) {   #time loop
  R[i] <- 135*B[i]/(1+B[i]/.0888) * exp(rnorm(1,0,.4)) # average recruitment model
  if (rbinom(n=1,size=1,prob=.2)==1) {  # single Bernoulli trial 
    R[i] <- R[i] * 4  # boom year, increase average R by 4x
  } else {
    R[i] <- R[i] * .25  # bust year, reduce R by 75%
  }
  s <- exp(-M) * (1-h)  # survival combines natural mortality and harvest rate, sequential
  
  N[i+1] <- N[i] * s + R[i]  # next year's pop
  
  B[i+1] <- (alpha*N[i]+rho*B[i]) * s + wr*R[i]  # next year's biomass
  }

Year <- seq(1,100)
B_Plot <- ggplot() +
  geom_line(mapping=aes(x=Year,y=B)) +
  labs(title='Boom-bust recruitment: Brazilian sardine',
       y='Spawning stock biomass (million tons)') +
  theme_bw()
plot(B_Plot)

  
