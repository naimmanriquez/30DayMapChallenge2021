library(mxmaps)
library(ggthemes)
library(ggplot2)

data("df_mxstate_2020")

options(scipen = 999)

df_mxstate_2020$value = df_mxstate_2020$pop

mxhexbin_choropleth(df_mxstate_2020, num_colors = 1,
                    title = "Poblaci�n por entidad federativa",
                    legend = "N�mero de habitantes") + theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "Poblaci�n en M�xico por entidad federativa", 
       subtitle = "Datos del Censo de Poblaci�n y Vivienda, 2020",
       caption = "Elaborado por @naimmanriquez")
  
## Percepci�n de seguridad

library(tidyverse)

transporte <- left_join(df_mxstate_2020, percepcion_envipe)

transporte$value = transporte$transporte_p�blico

theme_map <- function(...) {
  theme_minimal() +
    theme(
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
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

mxhexbin_choropleth (transporte, num_colors = 1,
                    title = "Percepci�n de inseguridad en el transporte p�blico",
                    legend = "Porcentaje") + theme_map() +
  theme(legend.position = "right") +
  labs(title = "Percepci�n de inseguridad en el transporte p�blico", 
       subtitle = "Porcentaje de la poblaci�n que menciona sentirse insegura",
       caption = "Elaborado por @naimmanriquez con datos de la ENVIPE 2021")

