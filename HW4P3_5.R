# MA 554 Applied Multivariate Analysis Fall 2019
# Author: Yuchen Dong
# Homework 4 Problem 3-5

library(MASS)
library(MVN)
library(ggplot2)
library(GGally)
library(lattice)
library(caret)
library(class)

######################################

#  Problem 3

######################################

# Part 1(a): Read the data from text file
setwd("C:/Users/YDONG/Dropbox/MA 554/HW/Homework 4")
data_3 <- read.delim('pendigit3.txt', sep="," , header = FALSE)
data_8 <- read.delim('pendigit8.txt', sep="," , header = FALSE)

# Part 1(b): Rearrange the 16 columns from the first observations
odd_index = seq(1,16, by=2)
even_index = seq(2,16,by=2)
x1 = as.numeric(data_3[1,odd_index])
y1 = as.numeric(data_3[1,even_index])

# visualize the first observation by plotting y against x from the data_3
plot(x1,y1)
lines(x1,y1)
title('First observation of Handwritten 3')

# Part 2: Perform PCA for this dataset
# Apply the PCA procedure
pr.out = prcomp(data_3[,1:16], scale=FALSE)
(pr.var = pr.out$sdev^2)
U = pr.out$rotation

# Plot the scatter plot of the principle components
Z = pr.out$x
pairs(Z)

# Plot the scree plot
plot(pr.var, xlab="Principal Component",ylab="Eigenvalue",type='b')
title('Scree Plot')


# Plot the cumulative Scree plot
pve = pr.var/sum(pr.var)
plot(cumsum(pve), xlab="Principal Component",
     ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')
abline(h = .9, lty = 2)
title('Cumulative Scree Plot')

# Part 3: Assessing the joint normality
result = mvn(data = data_3[,seq(1,16)], multivariatePlot = "qq")


# data_3_num = data_3[,seq(1,16)]
# data_3_mean = colMeans(data_3_num)
# data_3_cov = cov(data_3_num)
# data_3_cov_inv = ginv(data_3_cov)
# # Compute the Mahalanobis distances
# d <- numeric(length = length(dim(data_3_num)[1]))
# for (j in 1:dim(data_3_num)[1]){
#   d[j] = t(as.numeric(data_3_num[j,]-data_3_mean))%*%data_3_cov_inv%*%as.numeric(data_3_num[j,]- data_3_mean)
# }
# # rearrange d in an ascending order
# d_sorted = sort(d)
# n = dim(data_3_num)[1]
# p = dim(data_3_num)[2]
# conflevel = (seq(1:n)-1/2)/n
# qpoint = qchisq(conflevel, df=p)  
# plot(qpoint[1:500],d_sorted[1:500],col = 'red')
# points(qpoint[1:500],qpoint[1:500],col = 'blue')

# Part 4: Determine the number of components
# From the cumulative scree plot, we set a 90% as a cutoff. We could see that the first four components are enough. 

# Part 5: Consider the first four components
np = 4
# +/- 2 sigma on each principle components
v = matrix(colMeans(as.matrix(data_3[,1:16])))


PCAplot <- function(k){ 
  p1 = v-2*sqrt(pr.var[k])*matrix(U[1:16,k])
  p2 = v-sqrt(pr.var[k])*matrix(U[1:16,k])
  p3 = v
  p4 = v+sqrt(pr.var[k])*matrix(U[1:16,k])
  p5 = v+2*sqrt(pr.var[k])*matrix(U[1:16,k])

  par(mfrow = c(1,5))

  plot(p1[odd_index,1], p1[even_index,1])
  lines(p1[odd_index,1], p1[even_index,1])
  plot(p2[odd_index,1], p2[even_index,1])
  lines(p2[odd_index,1], p2[even_index,1])
  plot(p3[odd_index,1], p3[even_index,1])
  lines(p3[odd_index,1], p3[even_index,1])
  plot(p4[odd_index,1], p4[even_index,1])
  lines(p4[odd_index,1], p4[even_index,1])
  plot(p5[odd_index,1], p5[even_index,1])
  lines(p5[odd_index,1], p5[even_index,1])
}

PCAplot(1)
PCAplot(2)
PCAplot(3)
PCAplot(4)

# part 6: 

# Combine two dataset
data_comb = rbind2(data_3, data_8)

# Apply the PCA procedure
pr_new.out = prcomp(data_comb[,1:16], scale=FALSE)
(pr_new.var = pr_new.out$sdev^2)
U_new = pr_new.out$rotation

# Plot the scatter plot of the principle components
Z_new = pr_new.out$x
pairs(Z_new,pch=c(rep(1,1055),rep(3,1055)),col=c(rep("blue",1055),rep("green",1055)))

pve_new = pr_new.var/sum(pr_new.var)
plot(cumsum(pve_new), xlab="Principal Component",
     ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')
abline(h = .9, lty = 2)
title('Cumulative Scree Plot for the combined data')


pairs(Z_new[,1:5],pch=c(rep(1,1055),rep(3,1055)),col=c(rep("blue",1055),rep("green",1055)))

######################################

#  Problem 4

######################################

# For simplicity, we fix the random seed for debugging.
set.seed(100)

# Split the data evenly.
train_idx = sample(1:2110, 1055, replace=FALSE)
train_data = data_comb[train_idx,1:16]
train_flag = data_comb[train_idx,17]
test_data = data_comb[-train_idx,1:16]
# true label for test data
test_flag = data_comb[-train_idx,17]

# Method 1: Linear discriminant analysis
lda_result <- lda(train_flag~ ., train_data, prior = c(1,1)/2, subset = train_idx)
lda_table <- confusionMatrix(factor(predict(lda_result, test_data)$class), factor(test_flag))$table
lda_table
ldamis <- 1- sum(diag(lda_table))/sum(lda_table)
cat("For linear discriminator analysis, the misclassification rate is ",ldamis)

# Method 2: Nearest Centroid rule
train_3_idx = which(data_comb[train_idx,17]==3)
train_8_idx = which(data_comb[train_idx,17]==8)
train_3_data = data_comb[train_3_idx,1:16]
train_8_data = data_comb[train_8_idx,1:16]

NCclassifier<- function(x1,x2,datainput){
  # The input are
  #       x1 = (dataframe) data in group 1
  #       x2 = (dataframe) data in group 2
  #
  x1_bar = colMeans(x1)
  x2_bar = colMeans(x2)
  NC_phi = t(x2_bar-x1_bar)%*%t(datainput-(x1_bar+x2_bar)*0.5)
  return (sign(NC_phi))
}

NC_phi = NCclassifier(train_3_data,train_8_data,test_data)
NC_phi = replace(NC_phi, NC_phi==1, 8)
NC_phi = replace(NC_phi, NC_phi==-1, 3)
NC_table <- confusionMatrix(factor(NC_phi), factor(test_flag))$table
NC_table
NCmis <- 1- sum(diag(NC_table))/sum(NC_table)
cat("For nearest centroid rule, the misclassification rate is ",NCmis)

x1_bar = colMeans(train_3_data)
x2_bar = colMeans(train_8_data)
meandiff = x2_bar-x1_bar
NC_phi = t(meandiff)%*%t(test_data-(x1_bar+x2_bar)*0.5)
sum((NC_phi<0)==(test_flag=='3'))

# Method 3: Quadratic discriminant analysis
qda_result <- qda(train_flag~ ., train_data, prior = c(1,1)/2, subset = train_idx)
qda_table <- confusionMatrix(factor(predict(qda_result, data_comb[-train_idx, 1:16 ])$class), factor(test_flag))$table
qda_table
qdamis <- 1- sum(diag(qda_table))/sum(qda_table)
cat("For quadratic discriminator analysis, the misclassification rate is ",qdamis)

# Method 4: 5-NN method
knn_result <- knn(train_data, test_data, factor(train_flag), k = 5, prob=TRUE)
knn_table <- confusionMatrix(factor(knn_result), factor(test_flag))$table
knn_table
knnmis <- 1- sum(diag(knn_table))/sum(knn_table)
cat("For 5-NN method, the misclassification rate is ",knnmis)

# Method 5: Naive Bayes Rule
library(e1071)
NBclassifier<- naiveBayes(as.factor(train_flag) ~., data= train_data)
default_pred <- predict(NBclassifier, test_data)

NB_table <- table(default_pred, as.factor(test_flag), dnn=c("Prediction","Actual"))
NB_table
NBmis <- 1- sum(diag(NB_table))/sum(NB_table)
cat("For Naive Bayes classifier, the misclassification rate is ",NBmis)



######################################

#  Problem 5

######################################

# Apply the first 5 principle components on the training data
pr5_new.out = prcomp(train_data, scale=FALSE)
U5_new = pr5_new.out$rotation[,1:5]
sqeetrain_data = as.data.frame(as.matrix(train_data-colMeans(train_data))%*%U5_new)
sqeetest_data = as.data.frame(as.matrix(test_data-colMeans(train_data))%*%U5_new)

# Method 1: Linear discriminant analysis on the squeezed data
lda5_result <- lda(train_flag~ ., sqeetrain_data, prior = c(1,1)/2, subset = train_idx)
lda5_table <- confusionMatrix(factor(predict(lda5_result, sqeetest_data)$class), factor(test_flag))$table
lda5_table
lda5mis <- 1- sum(diag(lda5_table))/sum(lda5_table)
cat("For linear discriminator analysis, the misclassification rate is ",lda5mis)

# Method 2: Nearest Centroid method
train5_3_data = sqeetrain_data[train5_3_idx,]
train5_8_data = sqeetrain_data[train5_8_idx,]

NC5_phi = NCclassifier(train5_3_data,train5_8_data,sqeetest_data)
NC5_phi = replace(NC5_phi, NC5_phi==1, 8)
NC5_phi = replace(NC5_phi, NC5_phi==-1, 3)
NC5_table <- confusionMatrix(factor(NC5_phi), factor(test_flag))$table
NC5_table
NC5mis <- 1- sum(diag(NC5_table))/sum(NC5_table)
cat("For nearest centroid rule, the misclassification rate is ",NC5mis)

# Method 3: Quadratic discriminant analysis
qda5_result <- qda(train_flag~ ., sqeetrain_data, prior = c(1,1)/2, subset = train_idx)
qda5_table <- confusionMatrix(factor(predict(qda5_result, sqeetest_data)$class), factor(test_flag))$table
qda5_table
qda5mis <- 1- sum(diag(qda5_table))/sum(qda5_table)
cat("For quadratic discriminator analysis, the misclassification rate is ",qda5mis)

# Method 4: 5-NN method
knn5_result <- knn(sqeetrain_data, sqeetest_data, factor(train_flag), k = 5, prob=TRUE)
knn5_table <- confusionMatrix(factor(knn5_result), factor(test_flag))$table
knn5_table
knn5mis <- 1- sum(diag(knn5_table))/sum(knn5_table)
cat("For 5-NN method, the misclassification rate is ",knn5mis)

# Method 5: Naive Bayes Rule
NB5classifier<- naiveBayes(as.factor(train_flag) ~., data= sqeetrain_data)
default5_pred <- predict(NB5classifier, sqeetest_data)

NB5_table <- table(default5_pred, as.factor(test_flag), dnn=c("Prediction","Actual"))
NB5_table
NB5mis <- 1- sum(diag(NB5_table))/sum(NB5_table)
cat("For Naive Bayes classifier, the misclassification rate is ",NB5mis)

