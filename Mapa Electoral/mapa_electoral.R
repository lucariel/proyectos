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
as.tibble(argentina_df)
names(argentina_df)[10]<-"provincia"
#################
elecciones2007<-read_delim("./datasets/2007-departamento.csv", delim = ",")
elecciones2007<-elecciones2007%>%
  filter(!grepl("VOTOS",partido), eleccion == "Presidente")

elecciones2009<-read_delim("./datasets/elecciones2009.csv", delim = ",")
elecciones2009<-elecciones2009%>%
  filter(!grepl("VOTOS",partido))

elecciones2011<-read_delim("./datasets/2011-generales.csv", delim = ",")
elecciones2011<-elecciones2011%>%
  filter(!grepl("VOTOS",partido), !grepl("TOTAL VOTANTES", partido))




elecciones2015<-read_delim("./datasets/elecciones2015.csv", delim = ",")



elecciones2017<-read_delim("./datasets/2017-generales.csv", delim = ";")
elecciones2017$VOTOS<-gsub("\\.","",elecciones2017$VOTOS)%>%trimws()
elecciones2017$VOTOS<-as.numeric(elecciones2017$VOTOS)
colnames(elecciones2017)<-c("DISTRITO","PARTIDO","VOTOS")
names(elecciones2017)

elecciones2017<-elecciones2017%>%
  filter(!grepl("VOTOS",PARTIDO))%>%
  filter(!grepl("ELECTORES", PARTIDO))%>%
  filter(!grepl("TOTAL", PARTIDO))
sum(elecciones2017$VOTOS, na.rm = T)



#################RESULTADOS POR PROVINCIA#####################

elecciones2007_xpcia<-elecciones2007%>%
  group_by(provincia,provinciaId, partido)%>%
  summarise(votos = sum(votos, na.rm = T))

elecciones2007_xpcia<-elecciones2007_xpcia%>%
  group_by(provincia)%>%
  mutate(votos_porcentaje = votos/sum(votos, na.rm = T))%>%
  arrange(provincia,desc(votos_porcentaje))


elecciones2009_xpcia<-elecciones2009%>%
  group_by(provincia,provinciaId, partido)%>%
  summarise(votos = sum(votos, na.rm = T))

elecciones2009_xpcia<-elecciones2009_xpcia%>%
  group_by(provincia)%>%
  mutate(votos_porcentaje = votos/sum(votos, na.rm = T))%>%
  arrange(provincia,desc(votos_porcentaje))






names(elecciones2011)

elecciones2011_xpcia<-elecciones2011%>%
  group_by(provincia,provinciaId, partido, eleccion)%>%
  summarise(votos = sum(votos, na.rm = T))

elecciones2011_xpcia<-elecciones2011_xpcia[!duplicated(elecciones2011_xpcia$partido),]


elecciones2015<-elecciones2015%>%
  mutate(partido = case_when(
    cod_voto=="0131"~"FPV",
    cod_voto=="0135"~"Cambiemos"
  ))

elecciones2011_xpcia<-elecciones2011_xpcia%>%
  group_by(provincia)%>%
  mutate(votos_porcentaje = votos/sum(votos, na.rm = T))%>%
  arrange(provincia,desc(votos_porcentaje))

elecciones2011_xpcia<-elecciones2011_xpcia%>%filter(partido!="TOTALES")


elecciones2015<-elecciones2015%>%
  filter(!grepl("9", cod_voto))

elecciones2015_xpcia<-elecciones2015%>%group_by(nombre, cod_pcia,partido)%>%
  summarise(votos = sum(votos, na.rm = T))%>%arrange(cod_pcia)
View(elecciones2015)
unique(elecciones2015$cod_voto)

elecciones2015_xpcia<-elecciones2015_xpcia%>%
  group_by(nombre)%>%
  mutate(votos_porcentaje = votos/sum(votos, na.rm = T))%>%
  arrange(nombre,desc(votos_porcentaje))


elecciones2017_xpcia<-elecciones2017%>%
  group_by(DISTRITO,PARTIDO)%>%
  summarise(votos = sum(VOTOS,na.rm=T))

elecciones2017_xpcia<-elecciones2017_xpcia%>%
  group_by(DISTRITO)%>%
  mutate(votos_porcentaje = votos/sum(votos, na.rm = T))%>%
  arrange(DISTRITO,desc(votos_porcentaje))





##Checking if they are all in same scale
sum(elecciones2017_xpcia$votos)
sum(elecciones2007_xpcia$votos)
sum(elecciones2011_xpcia$votos)
sum(elecciones2015_xpcia$votos)

elecciones2007_xpcia<-elecciones2007_xpcia%>%arrange(provincia, desc(votos))
elecciones2009_xpcia<-elecciones2009_xpcia%>%arrange(provincia, desc(votos))
elecciones2011_xpcia<-elecciones2011_xpcia%>%arrange(provincia, desc(votos))
elecciones2015_xpcia<-elecciones2015_xpcia%>%arrange(nombre, desc(votos))
elecciones2017_xpcia<-elecciones2017_xpcia%>%arrange(DISTRITO, desc(votos))



elecciones2007_xpcia$provincia <- sapply(elecciones2007_xpcia$provincia,toupper)
elecciones2009_xpcia$provincia <- sapply(elecciones2009_xpcia$provincia,toupper)
elecciones2011_xpcia$provincia <- sapply(elecciones2011_xpcia$provincia,toupper)
argentina_df$provincia <- sapply(argentina_df$provincia,toupper)

argentina_df$provincia<-iconv(argentina_df$provincia,to="ASCII//TRANSLIT")



#########FPV 2007##############


elecciones2007_xpcia_fpv<-elecciones2007_xpcia%>%
  filter(partido=="FTE. PARA LA VICTORIA"|
           partido=="PJ - AL.FTE.P/VICTORIA"|
           partido=="FTE. PARA LA VICTORIA - ON"|
           partido=="AL. FTE. PARA LA VICTORIA (ON)"|
           partido=="AL. FTE P/ LA VICTORIA - AL. FTE. JUST. P/ LA VICT."|
           partido=="AL. FTE. PARA LA VICTORIA"|
           partido=="FTE. P/ LA VICTORIA / AL. FTE. P/ LA VICT.")

argentina_df_fpv07<-argentina_df%>%left_join(elecciones2007_xpcia_fpv)
argentina_df_fpv07$partido <- "PJ"
map_PJ_07<-ggplot(argentina_df_fpv07,aes(x=long, y=lat)) + 
  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group, fill= -votos_porcentaje))+
  theme_void()
  

(unique(argentina_df_fpv07$provincia))

##FPV 2009

elecciones2009_xpcia_fpv<-elecciones2009_xpcia%>%
  filter(partido=="FTE. JUSTICIALIS.P/LA VICTORIA"|
           partido=="AL. FTE. PARA LA VICTORIA"|
           partido=="FTE. PARA LA VICTORIA"|
           partido=="AL. FTE. PARA LA VICTORIA"|
           partido=="FTE. JUST. P/LA VICTORIA"|
           partido=="FRENTE PARA LA VICTORIA"|
           partido=="AL. CONCERT. FTE. P/ LA VICTORIA"|
           partido=="AL. FTE. JUST. CHACO MERECE MAS"|
           partido=="JUSTICIALISTA"|
           partido=="FTE. JUSTICIALISTA ENTRERRIANO"|
           partido == "FTE. JUSTICIALISTA DEL PUEBLO"|
           partido == "FTE. JUST.DE LA DIG.Y EL PROG"|
           partido == "FTE. POR LA INTEGRACION"|
           partido == "AL. ENCUENTRO POP. PARA LA VICTORIA")

length(unique(elecciones2009_xpcia_fpv$provincia))
elecciones2009_xpcia_fpv$partido <- "PJ"
elecciones2009_xpcia_fpv$provincia[2]<-"CIUDAD DE BUENOS AIRES"

argentina_df_fpv09<-argentina_df%>%left_join(elecciones2009_xpcia_fpv)


map_PJ_09<-ggplot(argentina_df_fpv09,aes(x=long, y=lat)) + 
  coord_equal() + 
  geom_polygon(colour="black", size=0.1, aes(group=group, fill= -votos_porcentaje))+
  theme_void()

###############################################################################
#(elecciones2011_xpcia$partido[elecciones2011_xpcia$partido%>%grepl("VICT.",.,)])


elecciones2011_xpcia_11fpv<-elecciones2011_xpcia%>%
  filter(partido=="FRENTE PARA LA VICTORIA"|
        partido == "FRENTE P/ LA VICTORIA RIOJANA"|
        partido == "AL. FRENTE PARA LA VICTORIA"|
          partido == "FTE. JUST. P/LA VICTORIA"|
        partido == "PARTIDO JUSTICIALISTA")

unique(elecciones2011_xpcia$partido)
unique(elecciones2011_xpcia_11fpv$provincia)

elecciones2011_xpcia$partido[elecciones2011_xpcia$partido%>%grepl("JUST.", .)]
