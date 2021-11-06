########################################################
#Mapa de polígonos | Bicicletas en Mazatlan nivel AGEB
#Elaborado por: Naim Manriquez Garcia 
#               Twitter: @naimmanriquez
#               Github: @naimmanriquez
########################################################

##############
#Configuración
rm(list = ls())
library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(ineq, haven, readr, readxl, ggplot2, shiny, tidyverse, expss, 
       DescTools, lmtest, MASS, knitr,gmodels)


##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())
library("colorspace")
#hcl_palettes(plot = TRUE)

##############
#Abrir capas para mapas
##############

##############
#Municipios
mun_sin <- st_read("sin_municipal.shp")

#Crear variable para filtrar por municipios de interés
mun_sin$mun <- substr(mun_sin$CVEGEO, 3, 5)
#Filtrar a Mazatlan
mun_sin <- mun_sin %>% 
  filter(mun=="012")

##############
#AGEBs
ageb_sin <- st_read("sin_ageb_urb.shp")

#Crear variable para filtrar por municipios de interés
ageb_sin$mun <- substr(ageb_sin$CVEGEO, 3, 5)
#Filtrar a ZMG
ageb_sin <- ageb_sin %>% 
  filter(mun=="012")


#Seleccionar variables de interés
ageb_sin_2 <- ageb_sin %>% 
  dplyr::select(CVEGEO, mun)

#Leer los datos complementarios
datos_complemen <- read_dta("sin_ageb_urb.dta")

#Unir bases
ageb_complemen<-merge(x=ageb_sin_2,y=datos_complemen,by="CVEGEO")

#Seleccionar variables de interés
ageb_sin_2 <- ageb_complemen %>% 
  dplyr::select(CVEGEO, mun, y, z, gmarginacion, bici, isinsal2020)

#Mapa simpl
ggplot () +
  theme_void()+
  geom_sf(data = ageb_sin_2, aes(fill = bici), lwd = 0) +
  scale_fill_distiller(palette = "Reds") +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Hogares con bicicleta en la ciudad de Mazatlán", subtitle = "Cantidad de bicicletas") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Bicicletas") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "#")

library(RColorBrewer)

#Mapa marginacion
indicemarginacion <- ggplot () +
  theme_void()+
  geom_sf(data = ageb_sin_2, aes(fill = gmarginacion), lwd = 0) +
  scale_fill_gradientn(colors = viridis::viridis(20)) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Grado de marginación en Mazatlán por AGEB", subtitle = "En escala de 1 muy bajo y 5 muy alto") +
  labs ( caption = "Fuente: Elaborado por @naimmanriquez, con datos del Censo 2020.",
         fill = "Grado de marginación") 

indicemarginacion + theme(legend.position = c(0.8, 0.8))


## Autocorrelacion espacial

library(tmap)

tm_shape(ageb_sin_2) + tm_polygons(style="quantile", col = "gmarginacion") +
  tm_legend(outside = TRUE, text.size = .8)

tm_shape(ageb_sin_2) + 
  tm_fill("gmarginacion",
          palette = "Reds", 
          style = "quantile", 
          title = "Grado de marginación") +
  tm_borders(alpha=.4)  

neighbours <- poly2nb(ageb_sin_2)
neighbours

plot(ageb_sin_2, border = 'lightgrey')

neighbours2 <- poly2nb(ageb_sin_2, queen = FALSE)
neighbours2

plot(ageb_sin_2, border = 'lightgrey')

listw <- nb2listw(neighbours2)

globalMoran <- moran.test(ageb_sin_2$gmarginacion, listw)

globalMoran

globalMoran[["estimate"]][["Moran I statistic"]]

globalMoran[["p.value"]]

moran <- moran.plot(ageb_sin_2$gmarginacion, listw = nb2listw(neighbours2, style = "W"))

local <- localmoran(x = ageb_sin_2$gmarginacion, listw = nb2listw(neighbours2, style = "W"))

moran.map <- cbind(ageb_sin_2, local)

tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "quantile",
          title = "local moran statistic")










# Datos para nombres de municipios
mun_sin <- cbind(mun_sin, st_coordinates(st_centroid(mun_sin)))

