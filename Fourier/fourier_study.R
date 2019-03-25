##First followup of http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html#complex-wave

#Creating 2 waves
xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

#which can be linearly combined into a complex wave:
wave.3<-0.5*wave.1+0.25*wave.2 #Arbitrary coeficient.
plot(xs,wave.3,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

#Putting a roof to the extension of the wave
wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

##But remember that the fourier series only shows this:
########any PERIODIC wave can be represented by a sum of simple sine waves
#A periodic wave has a frequency f and a wavelength λ are defined by the repeating pattern. 
#A non-periodic wave does not have a frequency or wavelength.



#Some concepts:
  
#T    #:The fundamental period, T, is the period of all the samples taken,
#N/T  #:sampling rate, sr., is the number of samples(N) taken over a time period (aka acquisition frequency)->T/N
#1/T  #:Fundamental frequency f0:1/T


repeat.xs     <- seq(-2*pi,0,pi/100)
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)

#wave.3 is the weighted sum of wave.1 and wave.2. This equation is the Fourier Series for wave.3:
  
  #f(t)=0.5×sin(3wt)+0.25×sin(10wt)

#w is the angular frequency (aka angular speed) in radians/second
#f.0is the fundamental frequency of the complex wave.


#Plotting trajectories given a Fourier series
plot.fourier<-function(fourier.series, f.0, ts){
  #w is the angular frequency (aka angular speed) in radians/second
  #f.0is the fundamental frequency of the complex wave.
  w<-2*pi*f.0 
  trajectory<-sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)");abline(h=0,lty=3)
}

#EG:
plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 

#plotting f(t)=0.5×sin(3wt)+0.25×sin(10wt) using fourier series:
acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz)
dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c(.5,.25)    # strength of signal components


f <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts)  
##The DC term is the 0 Hz term and is equivalent to the average of all the samples in the window



library(stats)
fft(c(1,1,1,1)) / 4 
###Phase Shifts

#Phase shifts are translations in the x-axis for a given wave component. These shifts are measured in angles (radians).


##DC Components

#This concept deals with translations over the y-axis