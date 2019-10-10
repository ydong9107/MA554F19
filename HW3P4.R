# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 3 Problem 4

# install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
library(stats)
library(graphics)
library(ggplot2)
library(plotly)
library(tseries)
library(MASS)

# Part 1: Read the data from text file
setwd("C:/Users/YDONG/Dropbox/MA 554/HW/Homework 3")
data <- read.csv('homeless_smalldata.csv', header=TRUE, sep=",")
data =  data[,-1]

# Part 2: Conduct the test

p = 3 # degree of freedom
n2 = sum(data[,4]) # Number of homeless people
n1 = nrow(data) - n2  # Number of non-homeless people

D_nh = data[which(data$homeless == 0),] # Data of non-homeless people
D_h = data[which(data$homeless == 1),] # Data of homeless people

S1 = var(D_nh[,1:3]) # Sample covariance matrix of non-homeless people
S2 = var(D_h[,1:3]) # Sample covariance matrix of homeless people
Sp = ((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)

mu1 = colMeans(D_nh)
mu2 = colMeans(D_h)

test_stat = (n1+n2-p-1)/((n1+n2-2)*p)*(n1*n2)/(n1+n2)*t(matrix(mu1[1:3]-mu2[1:3]))%*%ginv(Sp)%*%matrix(mu1[1:3]-mu2[1:3])

p_value = df(test_stat, df1=p, df2=n1+n2-p-1)

# Part 3: Plot the data in 3D plot
data$homeless[which(data$homeless == 0)] <- 'Not Homeless'
data$homeless[which(data$homeless == 1)] <- 'Homeless'
data$homeless <- as.factor(data$homeless)

plot1 <- plot_ly(data, x = ~pcs, y = ~mcs, z = ~cesd, color = ~homeless, marker = list(size = 3), colors = c('#BF382A', '#0C4B8E')) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'PCS'),
                     yaxis = list(title = 'MCS'),
                     zaxis = list(title = 'CESD')))

# Use scatterplot3d function 
#colors <- c("#E69F00","#56B4E9")
#colors <- colors[data$homeless+1]
#scatterplot3d(data[,1:3], pch = 16,
#              main="3D Scatter Plot",
#              xlab = "PCS",
#              ylab = "MCS",
#              zlab = "CESD", color = colors)     



# Part 4: 

# Part 4(1): Joint Normality test

# Method 1: Use the sample Mahalanobis distances. 

# mu = colMeans(data[,1:3])
# S = cov(data[,1:3])
# d = (data[,1:3]-mu)%*%solve(S)%*%t(data[,1:3]-mu)

# # Method 2: Jarque Bera test
# jarque.bera.test(data[,1])
# jarque.bera.test(data[,2])
# jarque.bera.test(data[,3])
# 
# # Method 3: Shaprio-Wilk's test
# shapiro.test(data[,1])
# shapiro.test(data[,2])
# shapiro.test(data[,3])

# Method 3: Draw histogram and find the kernel density function

plot(density(D_nh[,1],kernel = "gaussian"), main='Histogram of PCS (Non-homeless Data)')
plot(density(D_nh[,2],kernel = "gaussian"), main='Histogram of MCS (Non-homeless Data)')
plot(density(D_nh[,3],kernel = "gaussian"), main='Histogram of CESD (Non-homeless Data)')

plot(density(D_h[,1],kernel = "gaussian"), main='Histogram of PCS (Homeless Data)')
plot(density(D_h[,2],kernel = "gaussian"), main='Histogram of MCS (Homeless Data)')
plot(density(D_h[,3],kernel = "gaussian"), main='Histogram of CESD (Homeless Data)')

# Part 4(2): 
# Sample covariance matrix S_1 and Sample covariance matrix S_2 are similar by observation. 

