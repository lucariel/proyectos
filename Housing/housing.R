##Kaggle Dataset Housing prices 
library(tidyverse)
library(ggplot2)
library(caret)

##First let's load the manually made GD and CF

##Vectorized Cost Function
costFunction<- function(X, y, theta){
  m <- nrow(X)
  predictions <- X%*%theta
  sqrErrors <- (predictions-y)^2
  J <- 1/(2*m)*sum(sqrErrors)
  J
}
alpha <-0.01
##Vectorized gradientDescent

gradientDescent<-function(X, y, theta, alpha, num_iter){
  m <- nrow(X)
  for(i in 1:num_iter){
    theta<-as.vector(theta - t(alpha/m*colSums(as.vector(X%*%theta-y)*X)))
    costFunction(X, y, theta)
    
  }
  costFunction(X, y, theta)
  theta
}

##########################
######Loading Data########
test<-read_csv("test.csv")
data<-read_csv("train.csv")
data[is.na(data)]<-0
##For linear regression only choosing the numeric data
nums <- unlist(lapply(data, is.numeric))
n_data<-data[,nums]
nrow(y)
##Creating Target
y <- as.vector(n_data[,length(n_data)])
##Remove target for training data
##Removing target
n_data<-n_data[,1:(length(n_data)-1)]
##Scaling 
n_data<-scale(n_data)
n_data<- as.matrix(n_data)

## ID variable changed to Xo for convinience
n_data[,1] = 1

##Setting up theta
theta<-c(1:ncol(n_data))
theta[c(1:ncol(n_data))] = 0


##
gradientDescent(n_data, y, theta, 0.01, 2009)
class(n_data)

as.matrix(n_data)%*%as.vector(theta)
costFunction(n_data, y, theta)
