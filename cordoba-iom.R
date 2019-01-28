###Cordoba input-output matrix

library(tidyverse)
#Importing Cordoba matrix

matriz1<-read.csv("matriz-insumo-producto.csv", skip = 5)

#Cleaning the headers to get the corresponding square matrix

View(matriz1)
matriz1<-matriz1[,-c(2:3)]
matriz1<-matriz1[c(1:124), c(2:125)]
rownames(matriz1)<-colnames(matriz1)
View(matriz1)
ncol(matriz1)
nrow(matriz1)
##Here I encountered with some limitations: the gov reports 145 columns 
#and 124 rows, so it's not really complete. 
#The aditional columns corresponds to comex and social services which 
#has no corresponden rows. I'll make it square to be able to work with it

class(matriz1$CULTIVOS.DE.CEREALES)#Factor

#Getting the tibble converted as numeric

x<-as.tibble(data.matrix(matriz1))
y<-rbind(x, c(colSums(x[,])))
View(y)
totals<-tail(y,1)
y<-y[,-1]
View(x)
View(totals)
class(x$CULTIVOS.DE.CEREALES)
class(totals$CULTIVOS.DE.CEREALES)
total<-as.numeric(totals[1,])
class(total)
k<-x/total

View(k)
#and now k is a correlation matrix
library(corrplot)
corrplot(as.matrix(k), method = "circle")
nrow(k)
colnames(k)<-c(1:ncol(k))
View(k)
library(zoom)

#What I learn with this is that the matrix has too many entries to be properly visualized.
#In the original file there are groupings. I will do the groupings and try again.
#learner how to add totals, corrplot(), matrix objects and practice data wrangling

