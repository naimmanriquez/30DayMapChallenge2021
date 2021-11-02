library(sf)
library(tidyverse)
library(ggplot2)
library(extrafont)

rutasarg <- st_read('https://ide.transporte.gob.ar/geoserver/observ/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=observ:_3.4.1.7.red_vial_ign_ont_a_prov_view&maxFeatures=22400&outputFormat=application%2Fjson')

head(rutasarg)


## Convertir

rutasarg$caracteristica <- factor(rutasarg$caracteristica)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      # Nota! Instalen esta fuente primero!!
      text = element_text(family = "Georgia", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "#e0fffc", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12, color = "#419067"), 
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#419067"),
      ...
    )
}

ggplot(rutasarg) +
  geom_sf(aes(fill = caracteristica, col = caracteristica)) +
  ggtitle("Rutas provinciales por caracteristica") + theme_map() +
  theme(legend.position = "bottom") + 
  labs(title = "Rutas provinciales", 
       subtitle = "Datos Abiertos del Ministerio de Transporte, 2018", 
       caption = "Elaborado por @naimmanriquez")





