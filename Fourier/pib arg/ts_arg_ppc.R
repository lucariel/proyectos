#TS ppc arg
#Source of analysis https://newonlinecourses.science.psu.edu/stat510/node/47/


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

pib_var_arg%>%
  ggplot(aes(x = year, y = ppc_var))+
    geom_line()

# AR(1) model, standing for autoregressive model of order 1
#A start in evaluating whether an AR(1) might work is to plot values of the series against lag 1 values of the series.

#Creating the column
pib_var_arg$ppc_var_lagged = 1
#given the first value a 0
pib_var_arg$ppc_var_lagged[1] = 0

#Creating the lag column
for(i in 2:length(pib_var_arg$ppc_var)){
  pib_var_arg$ppc_var_lagged[i] =  pib_var_arg$ppc_var[i-1]
}
#Ploting variation agains itself (lagged)
pib_var_arg%>%ggplot(aes(x = ppc_var_lagged, y = ppc_var))+geom_point()
##Growth in a random year doesn't look dependent of the performance of the previous

#Checking correlation 
correlation_lagged<-cor(pib_var_arg$ppc_var, pib_var_arg$ppc_var_lagged)
####0.1127218
##Almost 11%

ar1fit=lm(pib_var_arg[,1]~pib_var_arg[,2])

summary(ar1fit) 
acf(ar1fit$residuals, xlim=c(1,18))
plot(ar1fit)
## ok there is not much relation between growth in one year and the next. Too high p-value. A too big standard error. and corraltion of 11%.
## and and R-squared of -0.01818 
#Nothing to see here folks. Moving on


##Removing the lagged column and year
pib_var_arg<-pib_var_arg[,1]

##Decomposition

#Let's assume presidencies of 4 years, as currently happen, so lets make that our frequenc 
series_pib<-ts(pib_var_arg, freq = 4)

arg_decomposed<-decompose(series_pib, type = "additive")
plot(arg_decomposed$trend) #Ploting trends every 4 year

#Regaining the column year
pib_var_arg2<-as.data.frame(cbind(as.vector(arg_decomposed$trend),pib_var_arg$year))
names(pib_var_arg2) <- c("four_year_trend", "year")

#Ploting trends with years
pib_var_arg2%>%ggplot(aes(x = year, y = four_year_trend))+
  geom_line(na.rm = T) 
##The target of this plot it is to see which was the tendency for each year, considering groupings of 4 years (currently a term for a president)
