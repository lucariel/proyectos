##Fourier transform for Argentina PIB per capita.
library(ggplot2)
library(TSA)
library(stats)
library(readxl)
library(stats)
library(tidyverse)
library(TTR)

global<-read_excel("API_NY.GDP.PCAP.KD.ZG_DS2_es_excel_v2_10399774.xls", skip= 3)
names(global)[1]="Country"

arg<-global%>%
  filter(
    Country == "Argentina"
  )
pib_var_arg<-cbind(as.numeric(arg[,6:(length(arg)-1)]),as.numeric(names(global[,6:(length(global)-1)])))
pib_var_argser<-as.numeric(arg[,6:(length(arg)-1)])
colnames(pib_var_arg)<- c("ppc_var", "year")

pib_var_arg<-as.data.frame(pib_var_arg)

##Visualization of how changed the per capita pib in argentine over the years
ggplot(data =pib_var_arg, aes(x = year, y =ppc_var ))+geom_line(na.rm = T)



pib_var_argserts<-ts(pib_var_argser, start =1961, end = 2017, freq = 1)
class(pib_var_argserts)
plot.ts(pib_var_argserts)
plot.ts(SMA(pib_var_argserts)) ##here you can see that the trend for ppc pib variation had a droping tendency until late 80', grew until the 2000 and again until 2014 

FF<-abs(fft(pib_var_argserts)/sqrt(57))^2
P = (4/57)*FF[1:65] # Only need the first (n/2)+1 values of the FFT result.
periodogram(pib_var_argserts)
f = (0:64)/57 
plot(f, P, type="l")
