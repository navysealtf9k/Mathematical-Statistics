# Question 4: Gibbs Sampling
# Load Package
# install.packages('plot3D')
# install.packages('MCMCpack')
library(MASS)      # Q4 kde2d
library(plot3D)    # Q4 scatter3D
library(MCMCpack)  # Q4 rinvgamma

## Data and summary statistics
dat <- c(1.1777518, -0.5867896, 0.2283789, -0.1735369, -0.2328192,
         1.0955114, 1.2053680, -0.7216797, -0.3387580, 0.1620835,
         1.4173256, 0.0240219, -0.6647623, 0.6214567, 0.7466441,
         1.9525066, -1.2017093, 1.9736293, -0.1168171, 0.4511754)

n <- length(dat)
x_tot <- sum(dat)
x_sq_tot <- sum(dat^2)

## b) Generate Data by Gibbs
N <- 1000
mu <- rep(NA, N)
sigma_sq <- rep(NA, N)
mu[1] <- 0 # initial mu
for (i in 1:N){
  set.seed(i)
  sigma_sq[i] <- rinvgamma(1, shape = n/2+5, scale = (x_sq_tot-2*x_tot*mu[i]+n*(mu[i])^2+2)/2)
  mu[i+1] <- rnorm(1, mean = 4*x_tot/(4*n+sigma_sq[i]), sd = sqrt(4*sigma_sq[i]/(4*n+sigma_sq[i])))
}
mu <- mu[-1001]
length(mu)
(mu.bar <- mean(mu))
(sigma.bar <- mean(sigma_sq))

## c) Plot density function and density estimator

### Density function: Grid and df
# Define pdf of the joint posterior distribution
fpost <- function(x, y){
  f_post <- (y^(-16))*exp(-(x_sq_tot-2*x_tot*x+20*x^2+2)/(2*y)-x^2/8)
  return(f_post)
}

# Create a grid
mu.tmp <- rep(seq(-1, 1, 0.01), each=201)
sigma.tmp <- rep(seq(0.01, 2.01, 0.01), 201)

# Calculate the corresponding value of pdf
z.tmp <- rep(NA, length(mu.tmp))

for (i in 1:length(mu.tmp)){
  z.tmp[i] <- fpost(mu.tmp[i], sigma.tmp[i])
}

### Density Estimator: Grid and df
df_est <- kde2d(mu, sigma_sq, n = N, lims = c(-1, 1, 0, 2))

mu.grid <- rep(df_est$x, length(df_est$x))
sigma.grid <- rep(df_est$y, each = length(df_est$y))
z.vec <- as.vector(df_est$z)

### Plot
# Plot of the density function
scatter3D(mu.tmp, sigma.tmp, z.tmp, xlim=c(-1, 1), ylim=c(0, 2), xlab = expression(mu), ylab = expression(sigma^2), main="Density function", colkey = list(width=0.5))

# Plot of the density estimator
scatter3D(mu.grid, sigma.grid, z.vec, xlim=c(-1, 1), ylim=c(0, 2), xlab = expression(mu), ylab = expression(sigma^2), main="Density estimator", colkey = list(width=0.45))