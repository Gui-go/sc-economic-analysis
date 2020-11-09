# R-script 05-pipesul.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())

# Packages ----------------------------------------------------------------

if(!require("readr")){install.packages("readr")}
if(!require("plyr")){install.packages("plyr")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("janitor")){install.packages("janitor")}
if(!require("sf")){install.packages("sf")}
if(!require("sp")){install.packages("sp")}
if(!require("st")){install.packages("st")}
if(!require("leaflet")){install.packages("leaflet")}
if(!require("mongolite")){install.packages("mongolite")}
if(!require("readxl")){install.packages("readxl")}
if(!require("janitor")){install.packages("janitor")}
if(!require("spdep")){install.packages("spdep")}
if(!require("vroom")){install.packages("vroom")}
if(!require("httr")){install.packages("httr")}
if(!require("jsonlite")){install.packages("jsonlite")}
if(!require("rgdal")){install.packages("rgdal")}
library(httr)
library(jsonlite)
library(geojson)
library(geojsonio)
library(spdep)
if(!require("readxl")){install.packages("readxl")}
if(!require("zoo")){install.packages("zoo")}
# Functions ---------------------------------------------------------------

source(file = "R/fct_shpToHexagon.R")
source(file = "R/fct_centroid.R")
# source(file = "R/111-shTranslator.R")
# source(file = "R/fct_MoransI.R")
source(file = "R/fct_palleter.R")

# Dados -------------------------------------------------------------------


polygon <- sf::st_read("data/br_municipios_20200807/") %>%
  janitor::clean_names() %>% 
  # dplyr::filter(sigla_uf%in%c("SC")) %>%
  # dplyr::filter(cd_mun%in%c("4205407")) %>%
  dplyr::filter(sigla_uf%in%c("PR")) %>%
  dplyr::filter(cd_mun%in%c("4106902")) %>%
  dplyr::mutate(cd_mun = as.integer(cd_mun))


polygonHEX <- shpToHexagon(polygon, cellsize = .013)


t30 <- read_csv("data/tabela30ElevaExp.csv") %>%
  dplyr::select('co_entidade', 'lat', 'lng')
t30 <- t30[!is.na(t30$lat), ]
t30 <- t30 %>% 
  sf::st_as_sf(x = ., coords = c("lng", "lat")) %>%
  sf::st_set_crs(4326)

polygonINFO <- sf::st_join(polygonHEX, t30, join = sf::st_contains) %>%  
  dplyr::group_by(hexagon) %>% 
  dplyr::summarise(
    ntotal = dplyr::n(),
    .groups = 'drop'
  ) %>% 
  dplyr::mutate(
    area_km2 = as.integer(sf::st_area(geometry))/1000000,
    area_m2 = as.integer(sf::st_area(geometry))
  )

labs <- function(dfinfo){
  return(
    paste0(
      "<b>Hexágono: </b>", base::as.character(dfinfo[["hexagon"]]), "<br/>", 
      "<b>Número de escolas: </b>", round(as.numeric(dfinfo[["ntotal"]]), digits = 0), "<br/>"
    )
  )
}

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  # leaflet::addProviderTiles(input$type_graph_serv) %>% 
  leaflet::addPolygons(
    data = polygonINFO,
    fill = TRUE,
    stroke = TRUE,
    fillOpacity = 0.6,
    color = ~paletter(polygonINFO, 'ntotal'),
    weight = 1,
    highlight = leaflet::highlightOptions(
      weight = 3,
      fillOpacity = 0,
      color = "black",
      opacity = 1),
    label = lapply(labs(polygonINFO), htmltools::HTML)
    ) #%>% 
  leaflet::addCircleMarkers(
    data = pointsf,
    lng = ~lng,
    lat = ~lat,
    radius = 1,
    opacity = opa_pi,
    color = "red",
    popup = popup1(pointsf)
  ) %>%  # Deixar trecho abaixo comentado para poder conferir onde há imóveis
  # leaflet::addCircleMarkers(
  #   data = imoveisf,
  #   lng = ~lng,
  #   lat = ~lat,
  #   radius = 2,
  #   opacity = 0.75,
  #   color = "green"#,
  #   # popup = paste0(
  #   #   "<b>Distrito_pt:</b>",
  #   #   imoveisf[["distrito_pt"]]#,
  #   #   # "<br><b>Tipo:</b>",
#   #   # imoveisf[["tipo"]],
#   #   # "<br><b>Categoria:</b>",
#   #   # imoveisf[["categoria"]],
#   #   # "<br><b>Endereço:</b>",
#   #   # imoveisf[["formattedAddress"]]
#   # )
# ) %>%
leaflet::addCircleMarkers(
  data = dief,
  lng = ~lng,
  lat = ~lat,
  radius = 1,
  opacity = opa_die,
  color = "purple",
  popup = popup2(dief)
) %>% 
  leaflet::addLegend(
    position = "bottomleft",
    title = translator(as.character(input$input_serv_hexvar)),
    colors = lc(polygonINFO, input$input_serv_hexvar)[["colors"]],
    labels = lc(polygonINFO, input$input_serv_hexvar)[["legend"]], 
    opacity = .6
  )
  