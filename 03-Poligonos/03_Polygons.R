# Mapa de poligonos

# Cargar librerias

library(sf)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(viridis)

# Estados de la republica mexicana

edosmx <- st_read('https://raw.githubusercontent.com/angelnmara/geojson/master/mexicoHigh.json')

# Cargamos datos de pobreza extrema

pobrezaextrema <- read_excel("pobrezaextrema.xlsx")

# Union de datos

pobreza <- left_join(edosmx, pobrezaextrema)

## Mapa

ggplot(pobreza) +
  geom_sf(aes(fill = pob_extrema)) +
  ggtitle("Porcentaje de población en pobreza extrema") +
  scale_fill_gradientn(colors=brewer.pal(name="Oranges", n=6)) + theme_void()

## Modificaciones
# Generamos los breaks de la leyenda
pretty_breaks <- c(5, 8, 10, 15, 20, 22, 26)

# Valor minimo
minVal <- min(pobreza$pob_extrema, na.rm = T)
# Valor maximo
maxVal <- max(pobreza$pob_extrema, na.rm = T)

# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}
labels <- labels[1:length(labels)-1]

# define a new variable on the data set just as above
pobreza$brks <- cut(pobreza$pob_extrema, 
                 breaks = brks, 
                 include.lowest = TRUE, 
                 labels = labels)
brks_scale <- levels(pobreza$brks)
labels_scale <- rev(brks_scale)

# Ahora hacemos el mapa chido
# Ahora si, hacemos el mapa. 
pobreza %>% 
  ggplot(aes(fill = brks)) +
  geom_sf(color = "white") + 
  #scale_fill_distiller(palette = "Greens", direction = 1) + 
  theme_void() +
  theme(legend.position = "bottom") + 
  labs(title = "Pobreza extrema en México", 
       subtitle = "Porcentaje de población en pobreza extrema", 
       # tag  = "Pobreza: ",
       caption = "Fuente: CONEVAL, 2020 - twitter: @naimmanriquez") + 
  scale_fill_manual(
    # in manual scales, one has to define colors, well, manually
    # I can directly access them using viridis' magma-function
    values = rev(inferno(length(brks))),
    breaks = rev(brks_scale),
    name = "Porcentaje",
    drop = FALSE,
    labels = labels_scale,
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed 
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = T,
      label.position = "bottom"
    )
  )  

