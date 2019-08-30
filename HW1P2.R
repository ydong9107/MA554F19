# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 1 Problem 2

A = matrix(
  c(4, 8, 8,
    3, 6, 9), 
  nrow = 2, byrow = T)

y1 = eigen(A%*%t(A)) 
y1$values # eigenvalues
y1$vectors # eigenvector matrix

y2 = eigen(t(A)%*%A) 
y2$values # eigenvalues
y2$vectors # eigenvector matrix

y3 = svd(A)
y3$u # left singular vectors
y3$d # signular values
y3$v # right singular vectors

