library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(hgchmagic)
library(lazyeval)
library(zip)
library(RSQLite)
library(datafringe)
library(leaflet.extras)
library(leaflet.minicharts)
library(DT)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- 'Descargar Imagen'
hcoptslang$printChart <- "Imprimir Gráfico"
hcoptslang$downloadJPEG <- "Descarga en JPEG"
hcoptslang$downloadPNG <- "Descarga en PNG"
hcoptslang$downloadPDF <- "Descarga en PDF"
hcoptslang$downloadSVG <- "Descarga en SVG"
hcoptslang$thousandsSep <- ","

options(highcharter.lang = hcoptslang)

dicMapas <- read_csv('data/botones/generalDic.csv', col_types = cols(.default = "c"))
dicSelct <- read_csv('data/botones/selecBotones.csv')
caf_theme <- hc_theme(
  colors = c('#2A7F62','#C3ACCE', '#538083', '#89909F', '#DFD9E2', '#2c6444'),#c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2'),
  chart = list(
    backgroundColor = "transparent"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Open Sans",
      textDecoration= 'none'
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = '',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)



loc <- topojson_read("data/topoFiles/localidades-bog.topojson")
loc@data$Localidad <- toupper(loc@data$localidad)
loc@data$Localidad <- plyr::revalue(loc@data$Localidad, c('LA CANDELARIA' = 'CANDELARIA', 'RAFAEL URIBE URIBE' = 'RAFAEL URIBE',
                                                          'ENGATIVÁ' = 'ENGATIVA', 'USAQUÉN' = 'USAQUEN'))


