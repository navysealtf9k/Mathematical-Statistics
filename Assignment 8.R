## Assignment 8 Problem 2b)
#install.packages('smoothmest')
library(smoothmest)

N <- 1000 # number of samples to generate
sample_draw <- 0 # number of samples generated
accepted_samples <- rep(0,N) # accepted samples
iterations <- 0 #Number of samples iterated over till a 1000 samples are generated
target <- function(x){
  (exp(abs(x)-x^2/2-1/2))
}
while(sample_draw < N){
  iterations <- iterations + 1
  x <- rdoublex(1,mu=0,lambda=1)
  u <- runif(1, min = 0, max = 1)
  if (u < target(x)){
    sample_draw <- sample_draw + 1
    accepted_samples[sample_draw] <- x
  }
}
qqnorm(accepted_samples)

#Question 4a
#Initial values
x <- c(1.1777518, -0.5867896, 0.2283789, -0.1735369, -0.2328192,
       1.0955114, 1.2053680, -0.7216797, -0.3387580, 0.1620835,
       1.4173256, 0.0240219, -0.6647623, 0.6214567, 0.7466441,
       1.9525066, -1.2017093, 1.9736293, -0.1168171, 0.4511754)
obs <- length(x)


#values
sum_x <- sum(x)
sum_x_2 <- sum(x^2)
sum_x # found to be 7.018981
sum_x_2 # found to be 17.94659


#Question 4b

#Gibbs sampling.
#install.packages('invgamma')
library(invgamma)


#sampling from the posterior
u <- rep(NA, 1001)
variance <- rep(NA, 1001)
burn <- 1
variance[1] <- 2
 for(i in 2:10001){
   u[i] <- rnorm(1, 4 * obs * mean(x) / (4*obs + variance[i - 1] ), sd =
                     + sqrt(4 * variance[i-1] / (4*obs + variance[i-1])))
   variance[i] <- rinvgamma(1, obs/2 + 5, 0.5*sum((x - u[i])^2) + 1)
 }

#Removing burin
 u <- u[-burn]
variance <- variance[-burn]

#compute means
mean(u) # computed to be 0.3510487
mean(variance) # computed to be 0.6493934


