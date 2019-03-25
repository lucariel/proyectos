####Maping https://www.computerworld.com/article/3038270/data-analytics/create-maps-in-r-in-10-fairly-easy-steps.html
#install.packages("tmap")
#install.packages("tmaptools")
#install.packages("maptools")
#install.packages("sp")
#install.packages("rgdal")


library(rgdal)
library(raster)
library(ggplot2)
  library(sp)
shp<-readOGR(".","RMBA")
map <- ggplot() + geom_polygon(data = shp, aes(x = long, y = lat, group = group), color = "black", fill = NA)
map
