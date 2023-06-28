# Tarea-3-Procesamiento-de-Datos-Geogr-ficos.
---
title: "Tarea 3 PDG"
author: "Daniela Hidalgo

Ferdy Salazar"
date: "28/6/2023"
format: 
 html:
  theme: sketchy
  toc: true
  toc_floot: true
editor: visual
---

# 1.Introduccióm:

En esta tarea para el curso *GF-0604 Procesamiento de datos Geograficoss*, se usaran dos archivos de datos. el primero es un archivo json de [regiones Socioeconomicas de Costa Rica](https://repositoriotec.tec.ac.cr/handle/2238/6749?show=full), cuya fuente es el **Atlas Digital de Costa Rica de 2014**. El segundo archivo, es un csv de [Presencia y observación de mamiferos en Costa Rica](https://www.gbif.org/occurrence/download/0031158-230530130749713), estos se obtuvieron en el portal de datos de observaciones de **GBIF**.

Aunque en principio, las regiones no son usadas en temas de ecologia, se usaran para una mejor comprension de las observaciones. Ademas, se vera una relacion entre la llamada **"mancha urbana"** y la presencia de mamiferos.

# 2.Carga de paquetes:

```{r}
#| label: carga-paquetes
#| warning: false
#| code-fold: true 
#| message: false
 
library(tidyverse)
library(DT)
library(sf)
library(rgdal)
library(raster)
library(terra)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(viridis)
library(devtools)
```

# 3.Carga de datos:

```{r}
#| label: carga-datos
#| warning: false
#| code-fold: true 
#| message: false

###Regiones

regiones <- 
  st_read("datos/regiones.geojson", quiet = TRUE)

###Mamiferos

mamiferos <-
  st_read(
    "datos/mamiferos.csv.csv",
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude", # columna de longitud decimal
      "Y_POSSIBLE_NAMES=decimalLatitude"   # columna de latitud decimal
    ),
    quiet = TRUE
  )

# Cambio de sistema de coordenadas
regiones <-
  regiones |>
  st_transform(4326)

st_crs(mamiferos) <- 4326

```



# 4.Uniones

```{r}
#| label: carga-uniones-uno
#| warning: false
#| code-fold: true 
#| message: false


mamiferos_union_region <-
  st_join(
    x = mamiferos,
    y = dplyr::select(regiones, region),
    join = st_within
  )

```

```{r}
#| label: carga-uniones-dos
#| warning: false
#| code-fold: true 
#| message: false

riqueza_especies_mamiferos <-
  mamiferos_union_region |>
  st_drop_geometry() |>
  group_by(region) |>
  summarise(riqueza_especies_mamiferos = n_distinct(species, na.rm = TRUE))

```

```{r}
#| label: carga-uniones-tres
#| warning: false
#| code-fold: true 
#| message: false

region_union_riqueza <-
  left_join(
    x = regiones,
    y = riqueza_especies_mamiferos,
    by = "region"
  ) |>
  replace_na(list(riqueza_especies_mamiferos = 0))
```

# 5.Mapa de riqueza de especies de mamíferos en regiones socioeconómicas

```{r}
#| label: carga-mapa
#| warning: false
#| code-fold: true 
#| message: false

# Paleta de colores de riqueza de mamiferos
colores_riqueza_especies <-
  colorNumeric(
    palette = "blue",
    domain = region_union_riqueza$riqueza_especies_mamiferos,
    na.color = "transparent"
  )

# Paleta de colores de especies
colores_especies <- colorFactor(
  palette = viridis(length(unique(mamiferos$species))), 
  domain = mamiferos$species
)

# Mapa leaflet
leaflet() |>
  setView(
    lng = -84.19452,
    lat = 9.572735,
    zoom = 7) |>
  addTiles(group = "Mapa general (OpenStreetMap)") |>
  addProviderTiles(
    providers$Esri.WorldImagery, 
    group = "Imágenes satelitales (ESRI World Imagery)"
  ) |> 
  addPolygons(
    data = region_union_riqueza,
    fillColor = ~ colores_riqueza_especies(region_union_riqueza$riqueza_especies_mamiferos),
    fillOpacity = 0.8,
    color = "black",
    stroke = TRUE,
    weight = 1.0,
    popup = paste(
      paste("<strong>Riqueza de mamiferos:</strong>", region_union_riqueza$riqueza_especies_mamiferos),
      sep = '<br/>'
    ),
    group = "Riqueza de mamiferos"
  ) |>
  addScaleBar(
    position = "bottomleft", 
    options = scaleBarOptions(imperial = FALSE)
  ) |>    
  addLegend(
    position = "bottomleft",
    pal = colores_riqueza_especies,
    values = region_union_riqueza$riqueza_especies_mamiferos,
    group = "Riqueza de mamiferos",
    title = "Riqueza de mamiferos"
  ) |>
  addCircleMarkers(
    data = mamiferos,
    stroke = F,
    radius = 4,
    fillColor = ~colores_especies(mamiferos$species),
    fillOpacity = 1.0,
    popup = paste(
      paste0("<strong>Especie: </strong>", mamiferos$species),
      paste0("<strong>Localidad: </strong>", mamiferos$locality),
      paste0("<strong>Fecha: </strong>", mamiferos$eventDate),
      paste0("<strong>Fuente: </strong>", mamiferos$institutionCode),
      paste0("<a href='", mamiferos$occurrenceID, "'>Más información</a>"),
      sep = '<br/>'
    ),    
    group = "Registros de presencia"
  ) |>  
  addLayersControl(
    baseGroups = c(
      "Mapa general (OpenStreetMap)", 
      "Imágenes satelitales (ESRI World Imagery)"
    ),
    overlayGroups = c(
      "Riqueza de mamiferos",
      "Registros de presencia"
    )
  ) |>
  addResetMapButton() |>
  addSearchOSM() |>
  addMouseCoordinates() |>
  addFullscreenControl() |>
  hideGroup("Registros de presencia") 
```
