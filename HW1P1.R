# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 1 Problem 1

# Part 1: Read the data from text file
setwd("C:/Users/YDONG/Dropbox/MA 554/HW/Homework 1")
data <- read.delim('T1-2.txt', header=FALSE, sep="")
# Rename a column in R
names(data)<-c('Density', 'Strength(Machine)', 'Strength(Cross)')

# Part 2: Report summary statistics
# Find means of three variables
colMeans(data)
# Find the covariance matrix of three variables
var(data)
# ScatterPlot
pairs(data)

# Part 3: Identify an outlier
# See report:
