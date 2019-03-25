
setwd("~/Documents/R/Proyectos/CortesLuz/gadm36_ARG_shp")
library(broom)
library(rgdal)
library(raster)
library(ggplot2)
library(sp)
library(maptools)
library(tidyverse)

#https://data.humdata.org/dataset/argentina-administrative-level-0-boundaries
argentina<-readOGR(".","gadm36_ARG_2")
b.aires <- argentina[argentina$NAME_1=="Ciudad de Buenos Aires"| argentina$NAME_1=="Buenos Aires",]
amba <- b.aires[b.aires$NAME_2=="Distrito Federal"|
                  b.aires$NAME_2=="Lanús"|b.aires$NAME_2=="Avellaneda"|
                  b.aires$NAME_2=="Esteban Echeverría"|
                  b.aires$NAME_2=="Florencio Varela"|
                  b.aires$NAME_2=="General San Martín"|
                  b.aires$NAME_2=="General Sarmiento"|
                  b.aires$NAME_2=="Tigre"|
                  b.aires$NAME_2=="Lomas de Zamora"|
                  b.aires$NAME_2=="Morón"|
                  b.aires$NAME_2=="Quilmes"|
                  b.aires$NAME_2=="San Fernando"|
                  b.aires$NAME_2=="San Isidro"|
                  b.aires$NAME_2=="Vicente López"|
                  b.aires$NAME_2=="La Matanza"|
                  b.aires$NAME_2=="Moreno"|
                  b.aires$NAME_2=="Almirante Brown"|
                  b.aires$NAME_2=="Merlo"|
                  b.aires$NAME_2=="San Miguel"|
                  b.aires$NAME_2=="Tres de Febrero"|
                  b.aires$NAME_2=="Ezeiza"|
                  b.aires$NAME_2=="Malvinas Argentinas"|
                  b.aires$NAME_2=="Berazategui"|
                  b.aires$NAME_2=="Hurlingham"|
                  b.aires$NAME_2=="General Rodríguez"|
                  b.aires$NAME_2=="Luján"|
                  b.aires$NAME_2=="Marcos Paz"|
                  b.aires$NAME_2=="Pilar"|
                  b.aires$NAME_2=="Ensenada"|
                  b.aires$NAME_2=="Berisso"|
                  b.aires$NAME_2=="La Plata"|
                  b.aires$NAME_2=="Zárate"|
                  b.aires$NAME_2=="Escobar"|
                  b.aires$NAME_2=="Campana"|
                  b.aires$NAME_2=="Exaltación de la Cruz",]

cortes_bt= read.csv("cortes_bt.csv", stringsAsFactors = FALSE)
cortes_mt= read.csv("cortes_mt.csv", stringsAsFactors = FALSE)
mapa_data = amba@data

mapa_data<-data.frame(lapply(mapa_data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))
View(mapa_data)


cortes_bt$partido[3] = "ESTEBAN ECHEVERRÍA"
cortes_bt$partido[1] = "DISTRITO FEDERAL"
mapa_data$NAME_2 = as.character(mapa_data$NAME_2)

mapdat2<-mapa_data%>%
  left_join(cortes_bt, c("NAME_2"="partido"))


amba$polyID <- sapply(slot(amba, "polygons"), function(x) slot(x, "ID"))
amba$us <-mapdat2$usuarios 
sf <- amba
sf@data$id <- rownames(sf@data)
sf.points <- fortify(sf, region="id")  ##Converts the spdf to a df
sf.df <- right_join(sf.points, sf@data, by="id") ##Puts toghether both dataframes
##And now, with  all the information in one place, I can plot

ggplot(sf.df,aes(x=long, y=lat, fill=us)) + 
  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group))

