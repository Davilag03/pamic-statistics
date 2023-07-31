## Instalación de librerías
library(gtools)
library(readr)
library(tidyverse)
library(datasets)
library(foreign)
library(ggrepel)
library(ggplot2)
library(scales)
library(reshape2)
library(ggrepel)
library(extrafont)
library(pastecs)
library(ape)
library(ggdendro)
library(cluster)
library(plyr)
library(dplyr)
library(rpivotTable)
library(knitr)
library(extrafont)
library(ggmap)
library(rgdal)
library(pastecs)
library(tidyverse)
library(hrbrthemes)
library(patchwork)
library(GGally)
library(viridis)
library(caret)
library (shiny)

setwd("C:/Users/usuario/Dropbox/PAMIC/Scripts/La Antigua_brecha")

# Se carga la base de datos La Antigua_sub.csv
Antigua<- read_csv("La Antigua_sub.csv")

#Se calcula el %
Antigua$PROP_MUN <- Antigua$AREA_MUN / Antigua$AREA_HA

#Reducimos decimales
Antigua$PROP_MUN  <- round(Antigua$PROP_MUN, 2)


#Unimos el ICSE por municipio a la base de datos

# Se carga la base de datos Brecha_GEO_indice.csv
Brecha_GEO_indice<- read_csv("Brecha_GEO_indice.csv")

#transforma en caracter
Brecha_GEO_indice$CVEGEO <- as.character(Brecha_GEO_indice$CVEGEO) 
Antigua$CVEGEO <- as.character(Antigua$CVEGEO)

#Seleccionamos las columnas
indice<- Brecha_GEO_indice %>% select(CVEGEO, indice)

# Se une la columna de índice a la base Antigua
Antigua<- Antigua %>%
  inner_join(indice, by="CVEGEO")

# Para calcular el ICSE por subcuenca multiplicamos el areá proporcinal del municipio por el ICSE municipal
Antigua$indice_sub <- Antigua$PROP_MUN * Antigua$indice

#Exportamos la base de datos con los cálculos
write.csv(Antigua, "Brecha_Antigua_calculos.csv", row.names = FALSE)

#Calculamos el ICSE por subcuenca
indice_subcuencas <- aggregate(indice_sub ~ IDENTIFI, data = Antigua, FUN = sum)

#Reducimos los decimales
indice_subcuencas$indice_sub <- round(indice_subcuencas$indice_sub, 2)

#Vemos el resultado
indice_subcuencas

#Exportamos la tabla indice
write.csv(indice_subcuencas, "Brecha_indice_Antigua.csv", row.names = FALSE)