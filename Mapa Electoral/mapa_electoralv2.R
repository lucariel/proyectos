#Mapa Electoral
setwd("~/Documents/Cuantitativo/R/Proyectos/Mapa Electoral")
library(rgdal)
library(ggplot2)
library(tidyverse)

#
argentina_shp<-readOGR("./gadm36_ARG_shp","gadm36_ARG_1")
argentina_shp@data<-argentina_shp@data%>%
  mutate(f = rnorm(length(argentina_shp@data$NAME_0)))

argentina_df<- fortify(argentina_shp, region="GID_1")
as.tibble(argentina_df)
as.tibble(argentina_shp@data)


argentina_df<-right_join(argentina_df, argentina_shp@data, by=c("id"="GID_1"))

map<-ggplot(argentina_df,aes(x=long, y=lat)) + 
  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group, fill= f))
names(argentina_df)[10]<-"Provincia"
argentina_df$Provincia<-iconv(argentina_df$Provincia,to="ASCII//TRANSLIT")%>%toupper()
as.tibble(argentina_df)
argentina_df$Provincia[argentina_df$Provincia=="CIUDAD DE BUENOS AIRES"]="CAPITAL FEDERAL"
#################

elecciones83<-read_csv("./datasets/elecciones83.csv")
filtro_ucr<-elecciones83$`FORMULAS - AGRUPACIONES    POLITICAS`[elecciones83$`FORMULAS - AGRUPACIONES    POLITICAS`%>%grepl("ALFONSIN.", .)]
elecciones83_UCR<-elecciones83[elecciones83$`FORMULAS - AGRUPACIONES    POLITICAS` %in% filtro_ucr,]
elecciones83_UCR$Provincia<-iconv(elecciones83_UCR$Provincia,to="ASCII//TRANSLIT")%>%toupper()
argentina_df2<-argentina_df%>%group_by(Provincia)%>%
  nest()%>%left_join(elecciones83_UCR)%>%unnest()
map_83_UCR<-ggplot(argentina_df2,aes(x=long, y=lat)) + 
  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group, fill= `%`))+theme_void()
map_83_UCR + scale_fill_continuous(low="#ffffff",high="#800000")


argentina_df2%>%group_by(Provincia)%>%
  nest()%>%filter(Provincia == "NEUQUEN")%>%unnest()


filtro_pj<-elecciones83$`FORMULAS - AGRUPACIONES    POLITICAS`[elecciones83$`FORMULAS - AGRUPACIONES    POLITICAS`%>%grepl("LUDER", .)]
elecciones83_PJ<-elecciones83[elecciones83$`FORMULAS - AGRUPACIONES    POLITICAS` %in% filtro_pj,]

argentina_df3<-argentina_df%>%group_by(Provincia)%>%
  nest()%>%left_join(elecciones83_PJ)%>%unnest()

map_83_PJ<-ggplot(argentina_df3,aes(x=long, y=lat)) + 
  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group, fill= `%`))+theme_void()
map_83_PJ + scale_fill_continuous(low="#ffffff",high="#000066")

