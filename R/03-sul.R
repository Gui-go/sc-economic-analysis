# R-script 03-sul.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
theme_set(ggplot2::theme_minimal())

# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(sf)){install.packages("sf")}
if(!require(sp)){install.packages("sp")}
if(!require(st)){install.packages("st")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(janitor)){install.packages("janitor")}
if(!require(spdep)){install.packages("spdep")}
if(!require(vroom)){install.packages("vroom")}


# Functions ---------------------------------------------------------------

shpToHexagon <- function(shp, cellsize = 0.8, square = F, crs = 4326){
  hex <- sf::st_make_grid(
    shp, 
    cellsize = cellsize, 
    square = square
  ) %>% 
    base::data.frame(., hexagon = paste0("H", stringr::str_pad(1:length(.), 3, pad = "0"))) %>% 
    sf::st_as_sf() %>% 
    sf::st_set_crs(crs)
  return(hex)
}

cent_as_cols <- function(polygonx, names = c("centlat", "centlng")){
  centroids_plus <- do.call(rbind, st_centroid(polygonx$geometry)) %>% 
    tibble::as_tibble() %>% stats::setNames(c(names[1],names[2])) %>% dplyr::bind_cols(polygonx, .)
  return(centroids_plus)
}


# Data --------------------------------------------------------------------

# Para obter o shp das cidades de todo o Brasil
# GeoCiencias > Downloads > organizacao_do_territorio > malhas_territoriais > malhas_municipais > municipio_2019 > Brasil > BR > br_municipios.zip

# shp_br_uf
shp_br_uf <- sf::st_read("data/br_unidades_da_federacao/") %>%
  janitor::clean_names()
plot(shp_br_uf["nm_uf"])

# shp_br_meso
shp_br_meso <- sf::st_read("data/br_mesorregioes/") %>%
  janitor::clean_names()
plot(shp_br_meso["nm_meso"])

# shp_br_micro
shp_br_micro <- sf::st_read("data/br_microrregioes/") %>%
  janitor::clean_names()
plot(shp_br_micro["nm_micro"])

# shp_br_muni
shp_br_muni <- sf::st_read("data/br_municipios_20200807/") %>%
  janitor::clean_names()
plot(shp_br_muni["nm_mun"])



# one SHP -----------------------------------------------------------------


shp_br_muni2 <- cent_as_cols(shp_br_muni)

shp_br_muni3 <- shp_br_muni2 %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  sf::st_as_sf(x = ., coords = c("centlng", "centlat")) %>%
  sf::st_set_crs(4326)


# joined <- sf::st_join(shp_br_muni3, shp_br_micro %>% sf::st_set_crs(4326), join = st_contains) %>% 
#   dplyr::group_by(cd_mun) %>% 
#   dplyr::summarise(cd_micro=cd_micro)
# 
# tail(joined, 10)
# st_join(shp_br_muni3 %>% sf::st_set_crs(4326), shp_br_micro %>% sf::st_set_crs(4326), left = T, join = st_contains)

shp_sul <- shp_br_muni2 %>% 
  dplyr::filter(sigla_uf%in%c("SC", "RS", "PR"))

one_shp_sul <- shp_sul %>% 
  group_by(sigla_uf) %>% 
  dplyr::summarise()

hex_sul <- shpToHexagon(shp = one_shp_sul, cellsize = 1.6)
plot(hex_sul["hexagon"])
# Analysis Sul ------------------------------------------------------------

expcm <- vroom::vroom(file = "data/EXP_COMPLETA_MUN.csv")

expcmt <- expcm %>% 
  janitor::clean_names() %>% 
  # dplyr::filter(CO_ANO>=2010) %>% 
  # dplyr::filter(CO_ANO%in%2020) %>% 
  dplyr::filter(sg_uf_mun%in%c("SC", "RS", "PR")) %>%
  dplyr::group_by(co_mun) %>% 
  dplyr::summarise(sum_vl_fob = sum(vl_fob)) %>% 
  dplyr::mutate("cd_mun"=as.character(co_mun))

expcmt[is.na(expcmt)] <- 0

sul <- left_join(shp_sul, expcmt, by="cd_mun") %>% 
  dplyr::select(-co_mun)
sul[is.na(sul)] <- 0

plot(sul["sum_vl_fob"])

hex_sul_x <- sf::st_join(hex_sul %>% sf::st_set_crs(4326), sul %>% sf::st_set_crs(4326), join = sf::st_contains) %>%  
  dplyr::group_by(hexagon) %>% 
  dplyr::summarise(
    sum_hex_sul_x = sum(sum_vl_fob)
  )
hex_sul_x$sum_hex_sul_x[is.na(hex_sul_x$sum_hex_sul_x)] <- 0
plot(hex_sul_x["sum_hex_sul_x"])

# geom_sf_label(aes(label = NAME))
  
ggplot() +
  # geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
  geom_sf(aes(fill=sum_hex_sul_x), data = hex_sul_x, color = "black", alpha=.5) +
  geom_sf(data = one_shp_sul, color = "black", fill = NA, size = .8)+
  scale_fill_gradientn(colours = c("#a10000", "#33a100")) +
  theme_minimal()
c("#e3e3e3", "#5c5c5c", "#303030", "#000000") # Cinza claro, Cinza médio, Cinza escuro, Preto
c("#a10000", "#a19600", "#33a100") # Vermelho, Amarelo, Verde

br_muni <- sf::st_read("data/br_municipios_20200807/BR_Municipios_2019.shp")



