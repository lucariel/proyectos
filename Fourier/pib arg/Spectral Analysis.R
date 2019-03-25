##Spectral Analysis

t<- seq(0,200, by = 0.1)
x<- cos(2*pi*t/16)+0.75*sin(2*pi*t/5)
par(mfrow=c(2,1))

spectrum(x)
#graphics.off() 
#par("mar") 
#par(mar=c(1,1,1,1))
plot(t,x,'l')
