# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 4 Problem 2

library(MASS)
library(mvtnorm)
library(maps)
library(CCA)

# Part a: Read the data from text file
setwd("C:/Users/YDONG/Dropbox/MA 554/HW/Homework 4")
data <- read.delim('fit.txt', sep="")

X <- data[,1:3]
Y <- data[,4:6]
cc=cancor(X,Y)
rho<-cc$cor
g<-cc$xcoef 
h<-cc$ycoef

g1 <- g[,1]
h1 <- h[,1]
print(g1)
print(h1)

par(mfrow = c(1,2))
barplot(g[,1],main="PC1 loadings")
barplot(h[,2],main="PC2 loadings")



# Part b: Output the sample canonical correlation matrix
xi = as.matrix(X)%*%as.matrix(g)
w = as.matrix(Y)%*%as.matrix(h)

new_matrix = cbind2(xi,w)
corr_new = cor(new_matrix)
print(corr_new)


# Part c: Draw scattor plot
par(mfrow = c(2,2))
plot(w[,1],xi[,1], main="First canonical convariate",xlab="w_1",ylab="xi_1")
plot(w[,2],xi[,2], main="Second canonical convariate",xlab="w_2",ylab="xi_2")
plot(w[,3],xi[,3], main="Third canonical convariate",xlab="w_3",ylab="xi_3")

