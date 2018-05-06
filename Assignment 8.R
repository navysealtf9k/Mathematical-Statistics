## Assignment 8 Problem 2b)
#Suppose we want to generate random numbers from standard normal distribution whose density is given by φ(x) = (2π)−1/2 exp(−x2/2). We decide to use rejective sampling plan via double exponential distri- bution whose density function is given by: f0(x) = 1 exp(−|x|). 2

#Write a code in R to implement the rejective sampling method to generate n = 1000 observations from N(0, 1).

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

