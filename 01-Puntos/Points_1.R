## Gasto trimestral en agua por municipio en Sinaloa

## Cargamos librerias 

library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, survey, dplyr, ggplot2, sf, RColorBrewer)

## Carga de base de datos

concentradohogar <- readRDS("concentradohogar.rds")

## Filtramos para Sinaloa

datos_sinaloa <- filter(concentradohogar, ubica_geo >= "25001" & ubica_geo <= "25018")

## Gastos de agua por trimestre
agua_mpios <- aggregate(x = datos_sinaloa$agua,     # Especificar la variable de interes
          by = list(datos_sinaloa$ubica_geo),       # Especificar el grupo
          FUN = mean)                               # Especificar que queremos la media

# Cambiar de CVEGEO a nombre
agua_mpios$Group.1[1] <- "Ahome"
agua_mpios$Group.1[2] <- "Angostura"
agua_mpios$Group.1[3] <- "Concordia"
agua_mpios$Group.1[4] <- "Cosalá"
agua_mpios$Group.1[5] <- "Culiacán"
agua_mpios$Group.1[6] <- "Choix"
agua_mpios$Group.1[7] <- "Elota"
agua_mpios$Group.1[8] <- "El Fuerte"
agua_mpios$Group.1[9] <- "Guasave"
agua_mpios$Group.1[10] <- "Mazatlán"
agua_mpios$Group.1[11] <- "Mocorito"
agua_mpios$Group.1[12] <- "El Rosario"
agua_mpios$Group.1[13] <- "San Ignacio"
agua_mpios$Group.1[14] <- "Sinaloa"
agua_mpios$Group.1[15] <- "Navolato"


agua_mpios <- rename(agua_mpios, NAME_2 = Group.1)
agua_mpios <- rename(agua_mpios, consumo = x)


# Obtención de los limites de Sinaloa

limites_sinaloa <- st_read("https://raw.githubusercontent.com/angelnmara/geojson/master/Municipios/25_Sinaloa.json")

## Merge de las bases de datos

consumo_agua <- left_join(limites_sinaloa, agua_mpios)

# Mapas base
ggplot(data = consumo_agua) + 
  geom_sf() + 
  geom_sf_text(aes(label = NAME_2))

ggplot(consumo_agua) +
  geom_sf()

ggplot(consumo_agua) +
  geom_sf() +
  stat_sf_coordinates()

# Mapa final
ggplot(consumo_agua) +
  geom_sf(data = consumo_agua, fill = "white") +
  geom_point(
    aes(color = consumo, size = consumo, geometry = geometry),
    stat = "sf_coordinates"
  ) +
  ggtitle("Gasto por concepto de consumo de agua en hogares") +
  labs(title = "Gasto por concepto de consumo de agua en hogares",  #Título principal
       subtitle = "Promedio trimestral reportado en la ENIGH, 2020", 
       caption = "twitter y github \n @naimmanriquez") +
  scale_color_viridis_c(option = "C") +
  theme(legend.position = "bottom") +
  theme_void() + 
  theme(axis.text=element_text(colour="#000000"), 
        plot.title = element_text(color = "#e50914", size = 12, #color y tamaño título
                                  hjust = .5, face = "bold", family = "Georgia"),
        plot.subtitle = element_text(color = "#000000", hjust = .5,
                                     face = "bold")) #título centrado negrita
