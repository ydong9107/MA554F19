# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 1 Problem 4

library(MASS)
library(mvtnorm)
library(graphics)

set.seed(100) # Fix a random seed

mu = c(3,5)
var1 = 4
var2 = 9
corr12 = 0.9
Sigma = matrix(c(var1, sqrt(var1)*corr12*sqrt(var2),sqrt(var1)*corr12*sqrt(var2), var2), nrow=2 ,ncol=2)

# Generate the contour plot
D = det(Sigma)
x = seq(-3,9,length.out=100) # mu+/-3*sigma
y = seq(-4,14,length.out=100)
grids <- expand.grid(x,y)
f = dmvnorm(grids, mu, Sigma, log=FALSE)
z = matrix(f,100,100)
contour(x,y,z, main ='Contour plot',xlab='X_1', ylab='X_2')

# Generate 100 numbers of multivariate normal distribution
num = mvrnorm(n = 100, mu, Sigma)
plot(num)

# Sample mean
xbar = colMeans(num)
xbar

# Sample covariance matrix
S =var(num)
S

# Sketch the equidistance set given by the sample Mahalanobis distance
c = c(1,2,3,4,5);
level = exp(-c^2/2)/(2*pi*sqrt(det(S)));
x = seq(-3,9,length.out=100) # mu+/-3*sigma
y = seq(-4,14,length.out=100)
grids <- expand.grid(x,y)
f_new = dmvnorm(grids, xbar, S, log=FALSE)
z_new = matrix(f_new,100,100)
contour(x,y,z_new,xlim=c(-3,9), ylim=c(-4,14), main ='E_c for c=1',xlab='X_1', ylab='X_2', levels= level)

# Method 2: Use mahalanobis function to compute the Mahalanobis distance
# z = matrix(mahalanobis(grids, xbar, S, inverted = FALSE),100,100) 



