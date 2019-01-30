#This got really messy, lets start again in a V2


###Cordoba input-output matrix

library(tidyverse)
#Importing Cordoba matrix

matriz1<-read.csv("matriz-insumo-producto.csv", skip = 5)

#Cleaning the headers to get the corresponding square matrix

#View(matriz1)
matriz1<-matriz1[,-c(2:3)]
matriz1<-matriz1[c(1:124), c(2:125)]
rownames(matriz1)<-colnames(matriz1)
#View(matriz1)
ncol(matriz1)
nrow(matriz1)
##Here I encountered with some limitations: the gov reports 145 columns 
#and 124 rows, so it's not really complete. 
#The aditional columns corresponds to comex and social services which 
#has no corresponden rows. I'll make it square to be able to work with it

class(matriz1$CULTIVOS.DE.CEREALES)#Factor

#Getting the tibble converted as numeric

x<-as.tibble(data.matrix(matriz1)) ##This was badly converted, this just converted factors to their storage value, no to the writen value. I fixed later

y<-rbind(x, c(colSums(x[,])))
totals<-tail(y,1)
y<-y[,-1]
total<-as.numeric(totals[1,])
k<-x/total

#View(k)
#and now k is a correlation matrix
library(corrplot)
corrplot(as.matrix(k), method = "circle")
nrow(k)
colnames(k)<-c(1:ncol(k))
#View(k)
library(zoom)

#What I learn with this is that the matrix has too many entries to be properly visualized.
#In the original file there are groupings. I will do the groupings and try again.
#learner how to add totals, corrplot(), matrix objects and practice data wrangling

#29/01/2019
#The idea is to create a new matrix from scratch build from the first one

##Properly convert to numeric
library(readxl)
install.packages("readxl")
matriz1<-read_excel("matriz-insumo-productoexcel.xlsx", skip = 5)
View(matriz1)





##testing colSums
rowSums(matriz1[(1:2),], na.rm = T)
?colSums()

x <- matrix(1:20, 4)
rowSums(x, n = 1)


#View(rowSums(matriz1[,(4:5),drop = F], na.rm = T))
#### working with rows########
#The first 16 columns are the sector agro. 
agro<-rowSums(matriz1[,(4:20),drop = F], na.rm = T)
#View(agro)

#the next 3 are from mining. 
mining<-rowSums(matriz1[,(21:25),drop = F], na.rm = T)
#View(mining)

#the 30 next are agroindustries
agroindustries<-rowSums(matriz1[,(26:56),drop = F], na.rm = T)
#next 20 chemestry
chemistry<-rowSums(matriz1[,(57:77),drop = F], na.rm = T)


#next 7 steal_derivatives
steal_derivatives<-rowSums(matriz1[,(78:85),drop = F], na.rm = T)
#metalmecanica 21
steal_mechanic<-rowSums(matriz1[,(86:107),drop = F], na.rm = T)

#next 3 basicservices
basicservices<-rowSums(matriz1[,(108:111),drop = F], na.rm = T)

#next construction
construction<-rowSums(matriz1[,(112:113),drop = F], na.rm = T)

#comerce next 2
commerce<-rowSums(matriz1[,(114:116),drop = F], na.rm = T)


#turism next 8
turism<-rowSums(matriz1[,(117:125),drop = F], na.rm = T)



#next 2 comunications
comunications<-rowSums(matriz1[,(126:128),drop = F], na.rm = T)



#next 2 FS
finances<-rowSums(matriz1[,(129:131),drop = F], na.rm = T)



#next 3 profesional services 

professional_services<-rowSums(matriz1[,(132:135),drop = F], na.rm = T)

#next 6 goverment
gov<-rowSums(matriz1[,(133:139),drop = F], na.rm = T)



nmatriz<-cbind(agro,mining,agroindustries,chemistry,steal_derivatives,steal_mechanic,basicservices, construction, commerce, turism, comunications, finances,professional_services, gov)
#View(nmatriz)
###########Working with columns

#The first 16 columns are the sector agro. 
agro1<-colSums(nmatriz[(4:20),,drop = F], na.rm = T)

#the next 3 are from mining. 
mining1<-colSums(nmatriz[(21:25),,drop = F], na.rm = T)

#the 30 next are agroindustries
agroindustries1<-colSums(nmatriz[(26:56),,drop = F], na.rm = T)
#next 20 chemestry
chemistry1<-colSums(nmatriz[(57:77),,drop = F], na.rm = T)


#next 7 steal_derivatives
steal_derivatives1<-colSums(nmatriz[(78:85),,drop = F], na.rm = T)
#metalmecanica 21
steal_mechanic1<-colSums(nmatriz[(86:107),,drop = F], na.rm = T)

#next 3 basicservices
basicservices1<-colSums(nmatriz[(108:111),,drop = F], na.rm = T)

#next construction
construction1<-colSums(nmatriz[(112:113),,drop = F], na.rm = T)

#comerce next 2
commerce1<-colSums(nmatriz[(114:116),,drop = F], na.rm = T)


#turism next 8
turism1<-colSums(nmatriz[(117:125),,drop = F], na.rm = T)



#next 2 comunications
comunications1<-colSums(nmatriz[(126:128),,drop = F], na.rm = T)



#next 2 FS
finances1<-colSums(nmatriz[(129:131),,drop = F], na.rm = T)



#next 3 profesional services 

professional_services1<-colSums(nmatriz[(132:135),,drop = F], na.rm = T)

#next 6 goverment
gov1<-colSums(nmatriz[(133:139),,drop = F], na.rm = T)



nmatriz<-rbind(agro1,mining1,agroindustries1,chemistry1,steal_derivatives1,steal_mechanic1,basicservices1, construction1, commerce1, turism1, comunications1, finances1,professional_services1, gov1)
#View(nmatriz)

##Col and row of thesame name
rownames(nmatriz)<-colnames(nmatriz)
#View(nmatriz)


#
y<-rbind(nmatriz, c(colSums(nmatriz[,])))
totals<-tail(y,1)
y<-y[,-1]
total<-as.numeric(totals[1,])
k<-nmatriz/total
#View(k)
corrplot(as.matrix(k), method = "circle")
##Now makes more sense the plot next time I'll analyse it
###30/1

