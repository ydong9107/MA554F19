# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 3 Problem 5

library(MASS)
library(mvtnorm)

set.seed(150) # Fix a random seed

# p = 5
# rho = 0.1

HW3P5<- function(p,rho){
  N = 20000
  mu = matrix(0,p,1)
  
  id = diag(p)
  J = matrix(1,p,p)
  
  Sigma = (1-rho)*id+rho*J
  
  # Part(a-1)
  # Generate the random sample 
  r = 20;
  alpha = 0.9  # Confidence level
  
  Dat = t(mvrnorm(n = N, mu, Sigma))
  
  # vector
  
  a = matrix(1,p,1)
  CI_low = matrix(0,p,N/r)
  CI_upper = matrix(0,p,N/r)
  A = matrix(0,p,N/r)
  b = matrix(0,1,N/r)
  
  for (i in 1:(N/r)){
    
    Atemp = Dat[,((i-1)*r+1):(i*r)]
    
    mutemp = t(rowMeans(Atemp)) # Mean of smaller data matrix
    S = cov(t(Atemp))
    
    # Fvalue = qf(alpha, df1=p, df2=r-p) # Fvalue
    
    tvalue = qt(alpha+(1-alpha)/2, r-1)
    
    S_diag = diag(S) # s_{kk} k=1,2,...,p
    
    CI_low[,i] = t(mutemp - S_diag/sqrt(r)*tvalue)
    CI_upper[,i] = t(mutemp + S_diag/sqrt(r)*tvalue)
    
    # CI_low[,i] = t(mutemp - sqrt((r-1)*p/(r-p)*Fvalue*t(S_diag)/r))
    # CI_upper[,i] = t(mutemp + sqrt((r-1)*p/(r-p)*Fvalue*t(S_diag)/r))
    
    A[,i] = (CI_low[,i]<=mu) & (CI_upper[,i]>=mu)
    
    if (sum(A[,i])<p){
      b[i] = 0;}
    else 
    {b[i]=1;}
  }
  bpercent = sum(b)/(N/r)
  Amean = rowSums(A)/(N/r)
  
  return(list(Amean,bpercent))
}

Result1 = HW3P5(5,0.1)
Result2 = HW3P5(30,0.1)
Result3 = HW3P5(3,0.5)
Result4 = HW3P5(5,0.8)
