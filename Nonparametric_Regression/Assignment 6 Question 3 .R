#Using the same data cps71 as in Question #1, obtain the bandwidth recommended by the leave-one-out cross-validation (CV) and general- ized cross-validation (GCV), as follow.
#(a) For local constant kernel estimator of βx, find the bandwidth within the range of h ∈ (0.02, 5).
#(b) For local linear kernel estimator of βx, find the bandwidth within the range of h ∈ (0.02, 5).

library(np)
library(ggplot2)

##The data
data("cps71")
attach(cps71)

##Define the parameters
h= seq(0.6, 5, 0.05)
nh= length(h)
y = logwage
nobs = length(age)
cv = rep(0,nobs)
cv_f<-rep(0,nh)
gcv<-rep(0,nh)
gh<-rep(0,nh)
L = matrix(NA, nrow=nobs, ncol=nobs)



kernel_est_l<- function(x,h,age) {
  z<-cbind(rep(1, nobs)) #Create the z-matrix
  Wx<-diag(1/h*dnorm((x-age)/h)) #Create the W-matrix
  dim(Wx)
  l<- solve(t(z)%*%Wx%*%z,t(z)%*%Wx) #Solution
  return(l)
}

cont=1

for (j in 1:nh) {
  for (i in 1:nobs ) {
    L[i,]<-kernel_est_l(age[i],h[j],age)
    cv[i] = ((y[i]-(L%*%y)[i])/(1-L[i, i]))^2
  }
  cv_f[j]<-sum(cv)
  tr_L_d= sum(diag(L))/nobs
  gcv[j] = sum(((y - L%*%y)/(1 - tr_L_d))^2)
}

cvf<-h[which.min(cv_f)]
gcvf<-h[which.min(gcv)]
cvf;gcvf

ggplot()+geom_point(aes(x=seq(1:89), y=cv_f), color="Blue")+
  geom_point(aes(x=seq(1:89), y=gcv), color="Red")


#My solution
z_x_lc <- matrix(nrow = n, ncol = 1)
z_x_lc[,1] <- rep(1, n)
z_x_ll <- matrix(nrow = n, ncol = 2)
z_x_ll[,1] <- rep(1, n)

L_lc <- matrix(nrow = n, ncol = n)
L_ll <- matrix(nrow = n, ncol = n)
cv_lc <- c()
cv_ll <- c()
e1 <- c(1,0)
h_1 <- seq(0.6,5, by = 0.05)
cv_cons <- c()
cv_line <- c()
gcv_cons <- c()
gcv_line <- c()

for (j in 1 : length(h_1)) {
  
  for (i in 1 : n) {
    z_x_ll[,2] <- c(age - age[i])
    deviation_1 <- c(age - age[i])
    weights_1 <- (1/h_1[j])*dnorm(x = (deviation_1/h_1[j]), mean = 0, sd = 1)
    w_x_1 <- vec2diag(x = weights_1)
    L_lc[i, ] <- as.numeric((solve((t(z_x_lc) %*% w_x_1 %*% z_x_lc)) %*% t(z_x_lc) %*% w_x_1))
    L_ll[i, ] <- as.numeric(e1 %*% (solve((t(z_x_ll) %*% w_x_1 %*% z_x_ll)) %*% t(z_x_ll) %*% w_x_1))
    cv_lc[i] <- c(((wage[i] - (L_lc %*% wage)[i])/(1 - L_lc[i,i]))^2)
    cv_ll[i] <- c(((wage[i] - (L_ll %*% wage)[i])/(1 - L_ll[i,i]))^2)
  }
  cv_cons[j] <- sum(cv_lc)
  cv_line[j] <- sum(cv_ll)
  v_lc <- sum(diag(L_lc))
  v_ll <- sum(diag(L_ll))
  gcv_cons[j] <- c(sum(((wage - (L_lc%*%wage))/(1-(v_lc/n)))^2))
  gcv_line[j] <- c(sum(((wage - (L_ll%*%wage))/(1-(v_ll/n)))^2))
  
}

plot(cv_cons)
plot(cv_line)
plot(gcv_cons)
plot(gcv_line)
hh <- cbind(h_1[which.min(cv_cons)],h_1[which.min(cv_line)], h_1[which.min(gcv_cons)], h_1[which.min(gcv_line)])
colnames(hh) <- c('Min Local Constant by CV','Min Local Linear by CV', 'Min Local Constant by GCV', 'Min Local Linear by GCV')
hh
