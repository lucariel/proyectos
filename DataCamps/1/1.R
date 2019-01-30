##Projects DataCamp

#personal parenthesis = <pp><\pp>

# This sets plot images to a nice size.
options(repr.plot.width = 4, repr.plot.height = 4)

# Loading in the ggplot2 package
# .... YOUR CODE FOR TASK 1 ....
library(ggplot2)

t<-seq(0, 2*pi, length.out = 50)
?seq()
##<pp>Just testing what is it doing
f<-cbind(t,c(1:50))

colnames(f)<- c("uno", "dos")
f<-as.data.frame(f)
nrow(f)

f%>%ggplot(aes(x=uno, y = dos))+
  geom_line()
##</pp>
x<-sin(t)
y<-cos(t)
df<-data.frame(t,x,y)
df
# Make a scatter plot of points in a circle
p <- ggplot(df, aes(x, y))
p + geom_point()


# Defining the number of points
# .... YOUR CODE FOR TASK 3 ....
points<-500
# Defining the Golden Angle
# .... YOUR CODE FOR TASK 3 ....
angle<-pi*(3-sqrt(5))
t <- (1:points) *angle
x <- sin(t)
y <-cos(t)
df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
p + geom_point()# .... YOUR CODE FOR TASK 3 ....



df <- data.frame(t, x, y)

# Make a scatter plot of points in a spiral
p <- ggplot(df, aes(x*t, y*t))
  p + geom_point() + theme(panel.background = element_rect(fill = "white"), panel.grid = element_blank())
p <- ggplot(df, aes(x*t, y*t))
p + geom_point(aes(size = t),alpha = 0.5, color = "black", shape = 1) +  theme(panel.background = element_rect(fill = "white"), panel.grid = element_blank(),legend.position = "none")
# .