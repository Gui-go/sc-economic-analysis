# R-script 05-pipesul.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)
theme_set(ggplot2::theme_minimal())

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

# Functions ---------------------------------------------------------------
source(file = "R/fct_shpToHexagon.R")
source(file = "R/fct_centroid.R")

# Dados -------------------------------------------------------------------

# Malha BR
shp_br_muni <- sf::st_read("data/br_municipios_20200807/") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(cd_mun = as.numeric(cd_mun))

shp_sul_uf <- sf::st_read("data/br_unidades_da_federacao/") %>%
  janitor::clean_names() %>% 
  dplyr::filter(sigla_uf%in%c("SC", "RS", "PR"))

# Get everything
api_get <- GET(url = "https://servicodados.ibge.gov.br/api/v1/localidades/distritos")
api_content <- content(api_get, as = "text")
api_df <- fromJSON(api_content, flatten = TRUE)
localidadesBR <- api_df %>% 
  janitor::clean_names() %>% 
  dplyr::rename("cd_mun"="municipio_id") %>% 
  dplyr::filter(municipio_microrregiao_mesorregiao_uf_regiao_nome=="Sul")

# Secex data
expcm <- vroom::vroom(file = "data/EXP_COMPLETA_MUN.csv")
expcmt <- expcm %>% 
  janitor::clean_names() %>% 
  dplyr::filter(co_ano>=2010) %>%
  # dplyr::filter(CO_ANO%in%2020) %>% 
  dplyr::filter(sg_uf_mun%in%c("SC", "RS", "PR")) %>%
  dplyr::group_by(co_mun) %>% 
  dplyr::summarise(sum_vl_fob = sum(vl_fob)) %>% 
  dplyr::mutate("cd_mun"=as.numeric(co_mun))

df <- left_join(localidadesBR, expcmt, by="cd_mun")
df_sul <- left_join(shp_br_muni, df, by="cd_mun") %>% 
  dplyr::filter(municipio_microrregiao_mesorregiao_uf_regiao_nome=="Sul")
df_sul$sum_vl_fob[is.na(df_sul$sum_vl_fob)] <- 0
# plot(df_sul["sum_vl_fob"])

sul_hex <- df_sul %>% 
  dplyr::group_by(municipio_microrregiao_mesorregiao_uf_regiao_nome) %>% 
  dplyr::summarise() 
sul_hexs <- sul_hex %>% 
  shpToHexagon(shp = ., cellsize = 1.15)
sul_meso <- df_sul %>% 
  dplyr::group_by(municipio_microrregiao_mesorregiao_nome) %>% 
  dplyr::summarise() 
sul_micro <- df_sul %>% 
  dplyr::group_by(municipio_microrregiao_nome) %>% 
  dplyr::summarise() 


sul_hex_x <- sf::st_join(sul_hexs %>% sf::st_set_crs(4326), df_sul %>% sf::st_set_crs(4326), join = sf::st_contains) %>%  
  dplyr::group_by(hexagon) %>% 
  dplyr::summarise(
    sum_hex_sul_x = sum(sum_vl_fob)
  ) %>% 
  dplyr::mutate(sum_hex_sul_x = if_else(!is.na(sum_hex_sul_x), sum_hex_sul_x, 0)) %>% 
  dplyr::mutate(
    sum_hex_sul_hexcolor = case_when(
      sum_hex_sul_x <= 2*10^9 ~ "Exportações <= 2e+9",
      sum_hex_sul_x > 2*10^9 & sum_hex_sul_x <= 4*10^9 ~ "2e+9 > Exportações <= 4e+9",
      sum_hex_sul_x > 4*10^9 & sum_hex_sul_x <= 6*10^9 ~ "4e+9 > Exportações <= 6e+9",
      sum_hex_sul_x > 6*10^9 & sum_hex_sul_x <= 8*10^9 ~ "6e+9 > Exportações <= 8e+9",
      sum_hex_sul_x > 8*10^9 & sum_hex_sul_x <= 10*10^9 ~ "8e+9 > Exportações <= 1e+10",
      sum_hex_sul_x > 10*10^9 ~ "Exportações >= 1e+10"
    ),
    sum_hex_sul_hexcat = case_when(
      sum_hex_sul_x <= 2*10^9 ~ 0,
      sum_hex_sul_x > 2*10^9 & sum_hex_sul_x <= 4*10^9 ~ 1,
      sum_hex_sul_x > 4*10^9 & sum_hex_sul_x <= 6*10^9 ~ 2,
      sum_hex_sul_x > 6*10^9 & sum_hex_sul_x <= 8*10^9 ~ 3,
      sum_hex_sul_x > 8*10^9 & sum_hex_sul_x <= 10*10^9 ~ 4,
      sum_hex_sul_x > 10*10^9 ~ 5
    )
  )

sul_meso_x <- sf::st_join(sul_meso %>% sf::st_set_crs(4326), df_sul %>% sf::st_set_crs(4326), join = sf::st_contains) %>%  
  dplyr::group_by(municipio_microrregiao_mesorregiao_nome.y) %>% 
  dplyr::summarise(
    sum_hex_sul_x = sum(sum_vl_fob)
  ) %>% 
  dplyr::mutate(sum_hex_sul_x = if_else(!is.na(sum_hex_sul_x), sum_hex_sul_x, 0)) %>% 
  dplyr::mutate(
    sum_hex_sul_hexcolor = case_when(
      sum_hex_sul_x <= 2*10^9 ~ "Exportações <= 2e+9",
      sum_hex_sul_x > 2*10^9 & sum_hex_sul_x <= 4*10^9 ~ "2e+9 > Exportações <= 4e+9",
      sum_hex_sul_x > 4*10^9 & sum_hex_sul_x <= 6*10^9 ~ "4e+9 > Exportações <= 6e+9",
      sum_hex_sul_x > 6*10^9 & sum_hex_sul_x <= 8*10^9 ~ "6e+9 > Exportações <= 8e+9",
      sum_hex_sul_x > 8*10^9 & sum_hex_sul_x <= 10*10^9 ~ "8e+9 > Exportações <= 1e+10",
      sum_hex_sul_x > 10*10^9 ~ "Exportações >= 1e+10"
    )
  )

sul_micro_x <- sf::st_join(sul_micro %>% sf::st_set_crs(4326), df_sul %>% sf::st_set_crs(4326), join = sf::st_contains) %>%  
  dplyr::group_by(municipio_microrregiao_nome.y) %>% 
  dplyr::summarise(
    sum_hex_sul_x = sum(sum_vl_fob)
  ) %>% 
  dplyr::mutate(sum_hex_sul_x = if_else(!is.na(sum_hex_sul_x), sum_hex_sul_x, 0)) %>% 
  dplyr::mutate(
    sum_hex_sul_hexcolor = case_when(
      sum_hex_sul_x <= 2*10^9 ~ "Exportações <= 2e+9",
      sum_hex_sul_x > 2*10^9 & sum_hex_sul_x <= 4*10^9 ~ "2e+9 > Exportações <= 4e+9",
      sum_hex_sul_x > 4*10^9 & sum_hex_sul_x <= 6*10^9 ~ "4e+9 > Exportações <= 6e+9",
      sum_hex_sul_x > 6*10^9 & sum_hex_sul_x <= 8*10^9 ~ "6e+9 > Exportações <= 8e+9",
      sum_hex_sul_x > 8*10^9 & sum_hex_sul_x <= 10*10^9 ~ "8e+9 > Exportações <= 1e+10",
      sum_hex_sul_x > 10*10^9 ~ "Exportações >= 1e+10"
    )
  )



gh <- ggplot() +
  # geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
  # geom_sf(aes(fill=sum_hex_sul_x), data = sul_hex_x, color = "black", alpha=1) +
  # geom_sf(aes(fill=sum_hex_sul_hexcolor), data = sul_hex_x, color = "black", alpha=1) +
  geom_sf(aes(fill=sum_hex_sul_hexcat), data = sul_hex_x, color = "black", alpha=1) +
  geom_sf(data = shp_sul_uf, color = "black", fill = NA, size = .8)+
  # scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) +
  labs(
    title = "Exportações do Sul do Brasil", 
    subtitle = "Exportações de Jan/2010 à ",
    caption = "Dados Secex",
    # tag = "",
    fill = "Legenda"
  )+
  theme_void() # scala discreta
gh

gm1 <- ggplot() +
  # geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
  geom_sf(aes(fill=sum_hex_sul_x), data = sul_meso_x, color = "black", alpha=.5) +
  geom_sf(data = shp_sul_uf, color = "black", fill = NA, size = .8)+
  # scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) +
  labs(fill = "Legenda")+
  theme_void() # scala discreta

gm2 <- ggplot() +
  # geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
  geom_sf(aes(fill=sum_hex_sul_hexcolor), data = sul_micro_x, color = "black", alpha=.5) +
  geom_sf(data = shp_sul_uf, color = "black", fill = NA, size = .8)+
  # scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) +
  labs(fill = "Legenda")+
  theme_void() # scala discreta


ggpubr::ggarrange(gm1, gh, gm2, ncol = 3)




library(spdep)
w <- poly2nb(sul_hex_x, row.names=sul_hex_x["hexagon"])
ww <-  nb2listw(w, style='B')
xy <- coordinates(as_Spatial(cent_as_cols(sul_hex_x["hexagon"])[, c("centlat", "centlng")]))
class(w)
summary(w)
plot(sul_hex_x[, c("hexagon", "geometry")], col='gray', border='blue', lwd=2)
plot(w, xy, col='red', lwd=2, add=T)
moran(sul_hex_x$sum_hex_sul_x, ww, n=length(ww$neighbours), S0=Szero(ww))$I
moran.test(sul_hex_x$sum_hex_sul_x, ww, randomisation=FALSE)
#
if(!require(spdep)){install.packages("spdep")}
w <- poly2nb(polygonINFO, queen = F)
ww <-  nb2listw(w, style='B')

plot(polygonINFO[, c("med_price_m2", "geometry")], col='gray', border='blue', lwd=2)
plot(w, xy, col='red', lwd=2, add=TRUE)

moran(polygonINFO$med_price_m2, ww, n=length(ww$neighbours), S0=Szero(ww))$I
moran.test(polygonINFO$med_price_m2, ww, randomisation=FALSE)
