# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 3 Problem 2

library(MASS)

mu = matrix(c(0.564, 0.603), nrow = 2, byrow = T);
n = 42;
p = 2;
Sigma = matrix(
  c(0.0144, 0.0117, 0.0117, 0.0146), 
  nrow = 2, byrow = T)


# Part a: MLE for Sigma matrix
Sigma_MLE = (n-1)/n*Sigma;


Sigma_inv = ginv(Sigma_MLE);
mu_test = matrix(c(0.6, 0.58), nrow = 2, byrow = T);

# Part b: Elliptic Region  
f = (t(mu-mu_test))%*%Sigma_inv%*%(mu-mu_test)

f<p/(n-p)*qf(.95, df1=2, df2=40) 

h = (n-1)*p/(n-p)*qf(.95, df1=2, df2=40)

# Part c: Simultaneous confidence intervals
a1 = matrix(c(1,0), nrow = 2, byrow = T)
a2 = matrix(c(0,1), nrow = 2, byrow = T)

t(a1)%*%mu+sqrt(h*t(a1)%*%Sigma%*%a1/n)
t(a1)%*%mu-sqrt(h*t(a1)%*%Sigma%*%a1/n)


t(a2)%*%mu+sqrt(h*t(a2)%*%Sigma%*%a2/n)
t(a2)%*%mu-sqrt(h*t(a2)%*%Sigma%*%a2/n)

# Part d: Hypothesis Testing
v1 = n*t(mu-mu_test)%*%Sigma_inv%*%(mu-mu_test)

p_value = 1-pf(v1, df1=2, df2=40)

