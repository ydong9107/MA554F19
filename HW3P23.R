# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 3 Problem 2

library(MASS)
library(mvnmle)
library(car)

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

sci1l = t(a1)%*%mu+sqrt(h*t(a1)%*%Sigma%*%a1/n)
sci1u = t(a1)%*%mu-sqrt(h*t(a1)%*%Sigma%*%a1/n)


sci2l = t(a2)%*%mu+sqrt(h*t(a2)%*%Sigma%*%a2/n)
sci2u = t(a2)%*%mu-sqrt(h*t(a2)%*%Sigma%*%a2/n)

# Part d: Hypothesis Testing
v1 = n*t(mu-mu_test)%*%solve(Sigma)%*%(mu-mu_test)

p_value = 1-pf(v1, df1=2, df2=40)

# Homework 3 Problem 3 Bonferroni

bsci1l = t(a1)%*%mu+qt(0.05/p/2,n-1)*sqrt(t(a1)%*%Sigma%*%a1/n)
bsci1u = t(a1)%*%mu-qt(0.05/p/2,n-1)*sqrt(t(a1)%*%Sigma%*%a1/n)

bsci2l = t(a2)%*%mu+qt(0.05/p/2,n-1)*sqrt(t(a2)%*%Sigma%*%a2/n)
bsci2u = t(a2)%*%mu-qt(0.05/p/2,n-1)*sqrt(t(a2)%*%Sigma%*%a2/n)

# Plot the region
# Draw some random data from the given distribution
data = mvrnorm(n = 100, mu, Sigma)
plot(data,xlim=c(0.45,0.7), ylim=c(0.4,0.8), xlab='X_1', ylab='X_2') 
# Elliptical region R1, rectangular region R2, Bonferroni region R3
radius = sqrt(p / (n-p) * qf(0.95, p, n - p) )
ellipse(c(0.564, 0.603), Sigma_MLE, radius, log="", center.pch=19, center.cex=1.5,  draw=TRUE, col= "blue")

rect(sci1l, sci2l, sci1u, sci2u, border="red")
rect(bsci1l, bsci2l, bsci1u, bsci2u, border="green")
legend(x= 0.45, y=0.8, c('Elliptical Region', 'Simulaneous CIs', 'Bonferroni CIs'), text.col = c("blue","red","green"))

