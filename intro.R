#Analisis de Matriz insumo producto. Básico:
##lección: https://www.youtube.com/watch?v=1p3Xlo5hqys


#Una economía cerrada ilustrada por:

C = cbind(c(0.2, 0.3, 0.5), c(0.4, 0.3, 0.3), c(0.6, 0.2, 0.2))

#rename filas y columnas

rownames(C, do.NULL = T, prefix = "row")
rownames(C)<-c("trigo", "textiles", "acero")
colnames(C, do.NULL = T, prefix = "col")
colnames(C)<-c("trigo", "textiles", "acero")



plot(NULL, xlim = c(-1,3), ylim=c(-1,3), ylab="", axes = F)


I1 <-c(0,0);#trigo 
I2<-c(2,0); #textiles
I3<-c(1,2*sin(pi/3))#acero


text(I1[1]-0.6, I1[2]-0.4, "trigo")
text(I2[1]+0.5, I2[2]-0.5, "textiles")
text(I3[1], I3[2]+0.4, "acero")


arrows(I1[1], I1[2],I2[1], I2[2], col = "green")
arrows(I2[1], I2[2],I3[1], I3[2], col = "green")
arrows(I3[1], I3[2],I1[1], I1[2], col = "green")

arrows(I2[1], I2[2]-1,I1[1], I1[2]-1, col = "blue")
arrows(I3[1], I3[2]+1,I2[1], I2[2]+1, col = "blue")
arrows(I1[1], I1[2]+1,I3[1], I3[2]+1, col = "blue")


##Consumo de si mismo
text(I1[1]-0.2,I1[2], C[1,1])
text(I2[1]-0.2,I2[2], C[2,2])
text(I3[1]-0.2,I3[2], C[3,3])


##Consumo de otros
#trigo
text(I1[1], I1[2]+1, C[3,1], col = "blue")
text(I1[1]+0.4, I1[2]+1, C[1,3], col = "green")

text(I2[1], I2[2]+1, C[2,3], col = "blue")
text(I2[1]+0.4, I2[2]+1, C[3,2], col = "green")


text(I3[1], I3[2]+1, C[1,2], col = "blue")
text(I3[1]+0.4, I3[2]+1, C[2,1], col = "green")

eigen(C)$values
library(tidyverse)

eigenvectors<-eigen(C)$vectors
steady_state<-eigenvectors[,1]


generateOpen<-function(t){
  m = NULL
  for(i in 1:t){
      m = cbind(m, c(runif(t, min = 0, max = (1/t))))
    
  }
  return(m)
}

#Generando matriz 5x5
C<-generateOpen(5)

#Generando demanda externa

D<-100*generateOpen(5)[,1];D

p<-solve(diag(5)-C)%*%D;p

barplot(p, main = "Production", beside =T, xlab= "Industries",
        names.arg=c("1","2","3","4","5"))


?barplot()
#Ok ahora que pasa si la demanda extera de 1, 2 y 4 se corta al medio

newp<-solve(diag(5)-C)%*%(D*c(0.5,0.5,1,0.5,1));newp
barplot(newp, main = "Production", beside =T, xlab= "Industries",
        names.arg=c("1","2","3","4","5"))

counts<-cbind(p, newp)
t<-t(counts)
barplot(t, main = "Production", beside =T, xlab= "Industries",
        names.arg=c("1","2","3","4","5"))
#Si solo hubieramos solo a la mitad sin resolver, es decir, sin tener en cuenta la interralcion de insump producto

halvedp<-p*c(0.5,0.5,1,0.5,1)
counts<-cbind(p, newp,halvedp)
t<-t(counts)
barplot(t, main = "Production", beside =T, xlab= "Industries",
        col=c("blue", "green","red"),
        names.arg=c("1","2","3","4","5"))


pminor<-function(A, i) det( A[1:i,1:i] )

hawkinsimmons<-function(A,x){
  if(A[1,1]<0){
    return(F)
  }
  for(i in 2:x){
    if(pminor(A,i)<0){
      return(F)
    }
  }
  return(T)
}
hawkinsimmons(diag(5)-C,5)
