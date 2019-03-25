##Since the other script was really messy, I'll start again
library(tidyverse)
library(readxl)
matriz1<-read_excel("matriz-insumo-productoexcel.xlsx", skip = 5)
matriz1<-matriz1[,-c(1:3)]
matriz1<-matriz1[c(1:124),c(1:124)]
#View(matriz1)


##Lets reduce the matrix to a more workable format, getting the sums of each sector

##Getting summary of columns
agro<-rowSums(matriz1[,(1:11),drop = F], na.rm = T)
mining<-rowSums(matriz1[,(12:14),drop = F], na.rm = T)
manufacturas<-rowSums(matriz1[,(15:92),drop = F], na.rm = T)
elec_gas_water<-rowSums(matriz1[,(93:95),drop = F], na.rm = T)
construction<-rowSums(matriz1[,(96),drop = F], na.rm = T)
commerce<-rowSums(matriz1[,(97:98),drop = F], na.rm = T)
hotel_restaurant<-rowSums(matriz1[,(99:100),drop = F], na.rm = T)
transport_storage<-rowSums(matriz1[,(101:106),drop = F], na.rm = T)
telecom<-rowSums(matriz1[,(107:108),drop = F], na.rm = T)
finances<-rowSums(matriz1[,(109:110),drop = F], na.rm = T)
real_state<-rowSums(matriz1[,(111:112),drop = F], na.rm = T)
gov<-rowSums(matriz1[,(113),drop = F], na.rm = T)
teaching<-rowSums(matriz1[,(114:115),drop = F], na.rm = T)
health<-rowSums(matriz1[,(116:119),drop = F], na.rm = T)
other_soc_act<-rowSums(matriz1[,(120:123),drop = F], na.rm = T)
domestic_services<-rowSums(matriz1[,(124),drop = F], na.rm = T)

##temp io_matriz
nmatriz<-cbind(agro,mining,manufacturas,elec_gas_water,construction,commerce,
               hotel_restaurant,transport_storage,telecom,finances,real_state,
               gov,teaching,health,other_soc_act,domestic_services)

##Getting summary of rows
agro1<-colSums(nmatriz[(1:11),,drop = F], na.rm = T)
mining1<-colSums(nmatriz[(12:14),,drop = F], na.rm = T)
manufacturas1<-colSums(nmatriz[(15:92),,drop = F], na.rm = T)
elec_gas_water1<-colSums(nmatriz[(93:95),,drop = F], na.rm = T)
construction1<-colSums(nmatriz[(96),,drop = F], na.rm = T)
commerce1<-colSums(nmatriz[(97:98),,drop = F], na.rm = T)
hotel_restaurant1<-colSums(nmatriz[(99:100),,drop = F], na.rm = T)
transport_storage1<-colSums(nmatriz[(101:106),,drop = F], na.rm = T)
telecom1<-colSums(nmatriz[(107:108),,drop = F], na.rm = T)
finances1<-colSums(nmatriz[(109:110),,drop = F], na.rm = T)
real_state1<-colSums(nmatriz[(111:112),,drop = F], na.rm = T)
gov1<-colSums(nmatriz[(113),,drop = F], na.rm = T)
teaching1<-colSums(nmatriz[(114:115),,drop = F], na.rm = T)
health1<-colSums(nmatriz[(116:119),,drop = F], na.rm = T)
other_soc_act1<-colSums(nmatriz[(120:123),,drop = F], na.rm = T)
domestic_services1<-colSums(nmatriz[(124),,drop = F], na.rm = T)

##final io_matrix
io_matrix<-rbind(agro1,mining1,manufacturas1,elec_gas_water1,construction1,commerce1,
                   hotel_restaurant1,transport_storage1,telecom1,finances1,real_state1,
                   gov1,teaching1,health1,other_soc_act1,domestic_services1)

##Same name for cols and rows
rownames(io_matrix)<-colnames(io_matrix)

##Transform into coeficient matrix
#View(io_matrix)
io_matrix_coef <- sweep(io_matrix, 2, colSums(io_matrix),`/`)
io_matrix_coef[,ncol(io_matrix_coef)]<-0
#View(io_matrix_coef)
library(corrplot)
corrplot(as.matrix(io_matrix_coef), method = "circle")
View(io_matrix_coef)
