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
library(httr)
library(jsonlite)
library(geojson)
library(geojsonio)
# Functions ---------------------------------------------------------------

source(file = "R/fct_shpToHexagon.R")
source(file = "R/fct_centroid.R")

# Dados -------------------------------------------------------------------

# Divisão política minucipal+ do Sul
# https://servicodados.ibge.gov.br/api/docs/localidades?versao=1
api_get_dmunisul <- httr::GET(url = "https://servicodados.ibge.gov.br/api/v1/localidades/estados/41|42|43/distritos")
api_content_dmunisul <- httr::content(api_get_dmunisul, as = "text")
api_df_dmunisul <- jsonlite::fromJSON(api_content_dmunisul, flatten = TRUE)
df_dmuni_sul <- api_df_dmunisul %>% 
  janitor::clean_names() %>% 
  dplyr::rename("cd_mun" = "municipio_id")
rm(c(api_get_dmunisul, api_content_dmunisul, api_df_dmunisul))
# Secex data
# http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download
# Exportação > Volume único
exp_comex <- vroom::vroom(file = "data/EXP_COMPLETA_MUN.csv")
exp_comex_sul10 <- exp_comex %>% 
  janitor::clean_names() %>% 
  dplyr::filter(co_ano>=2010) %>%
  dplyr::filter(sg_uf_mun%in%c("SC", "RS", "PR")) %>%
  dplyr::group_by(co_mun) %>% 
  dplyr::summarise(sum_vl_fob = sum(vl_fob)) %>% 
  dplyr::mutate("cd_mun"=as.integer(co_mun)) %>% 
  dplyr::select(cd_mun, sum_vl_fob)
rm(exp_comex)
# Malha geográfica
# IBGE > GeoCiencias > Downloads > organizacao_do_territorio > malhas_territoriais > malhas_municipais > municipio_2019 > Brasil > BR > br_municipios_20200807.zip
# Municípios do Sul
sf_sul_muni <- sf::st_read("data/br_municipios_20200807/") %>%
  janitor::clean_names() %>% 
  dplyr::filter(sigla_uf%in%c("PR", "SC", "RS")) %>% 
  dplyr::mutate(cd_mun = as.integer(cd_mun))
# plot(sf_sul_muni["cd_mun"])

# UFs do Sul
sf_sul_uf <- sf_sul_muni %>% 
  dplyr::group_by(sigla_uf) %>% 
  dplyr::summarise()
# plot(sf_sul_uf["sigla_uf"])

sf_sul_hex <- shpToHexagon(shp = sf_sul_uf, cellsize = 1.15)

# Joins & Summaries -------------------------------------------------------
sul_muni <- left_join(df_dmuni_sul, exp_comex_sul10, by = "cd_mun") %>%
  right_join(sf_sul_muni, ., by = "cd_mun") %>%
  dplyr::mutate(sum_vl_fob = dplyr::if_else(is.na(sum_vl_fob), 0, sum_vl_fob)) %>% 
  sf::st_set_crs(4326)

sul_micro <- sul_muni %>% 
  dplyr::group_by(municipio_microrregiao_nome) %>% 
  dplyr::summarise(sum_hex_vl_fob = sum(sum_vl_fob)) %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(
    sum_hex_vl_fob = if_else(is.na(sum_hex_vl_fob), 0, sum_hex_vl_fob),
    sum_hex_vl_fob_cat1 = case_when(
      sum_hex_vl_fob <= 2*10^9 ~ "Exportações <= 2e+9",
      sum_hex_vl_fob > 2*10^9 & sum_hex_vl_fob <= 4*10^9 ~ "2e+9 > Exportações <= 4e+9",
      sum_hex_vl_fob > 4*10^9 & sum_hex_vl_fob <= 6*10^9 ~ "4e+9 > Exportações <= 6e+9",
      sum_hex_vl_fob > 6*10^9 & sum_hex_vl_fob <= 8*10^9 ~ "6e+9 > Exportações <= 8e+9",
      sum_hex_vl_fob > 8*10^9 & sum_hex_vl_fob <= 10*10^9 ~ "8e+9 > Exportações <= 1e+10",
      sum_hex_vl_fob > 10*10^9 ~ "Exportações >= 1e+10"
    ),
    sum_hex_vl_fob_cat2 = case_when(
      sum_hex_vl_fob <= 2*10^9 ~ 0,
      sum_hex_vl_fob > 2*10^9 & sum_hex_vl_fob <= 4*10^9 ~ 1,
      sum_hex_vl_fob > 4*10^9 & sum_hex_vl_fob <= 6*10^9 ~ 2,
      sum_hex_vl_fob > 6*10^9 & sum_hex_vl_fob <= 8*10^9 ~ 3,
      sum_hex_vl_fob > 8*10^9 & sum_hex_vl_fob <= 10*10^9 ~ 4,
      sum_hex_vl_fob > 10*10^9 ~ 5
    )
  )

sul_meso <- sul_muni %>% 
  dplyr::group_by(municipio_microrregiao_mesorregiao_nome) %>% 
  dplyr::summarise(sum_hex_vl_fob = sum(sum_vl_fob)) %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(
    sum_hex_vl_fob = if_else(is.na(sum_hex_vl_fob), 0, sum_hex_vl_fob),
    sum_hex_vl_fob_cat1 = case_when(
      sum_hex_vl_fob <= 2*10^9 ~ "Exportações <= 2e+9",
      sum_hex_vl_fob > 2*10^9 & sum_hex_vl_fob <= 4*10^9 ~ "2e+9 > Exportações <= 4e+9",
      sum_hex_vl_fob > 4*10^9 & sum_hex_vl_fob <= 6*10^9 ~ "4e+9 > Exportações <= 6e+9",
      sum_hex_vl_fob > 6*10^9 & sum_hex_vl_fob <= 8*10^9 ~ "6e+9 > Exportações <= 8e+9",
      sum_hex_vl_fob > 8*10^9 & sum_hex_vl_fob <= 10*10^9 ~ "8e+9 > Exportações <= 1e+10",
      sum_hex_vl_fob > 10*10^9 ~ "Exportações >= 1e+10"
    ),
    sum_hex_vl_fob_cat2 = case_when(
      sum_hex_vl_fob <= 2*10^9 ~ 0,
      sum_hex_vl_fob > 2*10^9 & sum_hex_vl_fob <= 4*10^9 ~ 1,
      sum_hex_vl_fob > 4*10^9 & sum_hex_vl_fob <= 6*10^9 ~ 2,
      sum_hex_vl_fob > 6*10^9 & sum_hex_vl_fob <= 8*10^9 ~ 3,
      sum_hex_vl_fob > 8*10^9 & sum_hex_vl_fob <= 10*10^9 ~ 4,
      sum_hex_vl_fob > 10*10^9 ~ 5
    )
  )

sul_hex <- sul_muni %>% 
  dplyr::group_by(municipio_microrregiao_mesorregiao_uf_regiao_nome) %>% 
  dplyr::summarise() %>% 
  shpToHexagon(shp = ., cellsize = 1.15) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_join(., sul_muni, join = sf::st_contains) %>%  
  dplyr::group_by(hexagon) %>% 
  dplyr::summarise(sum_hex_vl_fob = sum(sum_vl_fob)) %>% 
  dplyr::mutate(
    sum_hex_vl_fob = if_else(is.na(sum_hex_vl_fob), 0, sum_hex_vl_fob),
    sum_hex_vl_fob_cat1 = case_when(
      sum_hex_vl_fob <= 2*10^9 ~ "Exportações <= 2e+9",
      sum_hex_vl_fob > 2*10^9 & sum_hex_vl_fob <= 4*10^9 ~ "2e+9 > Exportações <= 4e+9",
      sum_hex_vl_fob > 4*10^9 & sum_hex_vl_fob <= 6*10^9 ~ "4e+9 > Exportações <= 6e+9",
      sum_hex_vl_fob > 6*10^9 & sum_hex_vl_fob <= 8*10^9 ~ "6e+9 > Exportações <= 8e+9",
      sum_hex_vl_fob > 8*10^9 & sum_hex_vl_fob <= 10*10^9 ~ "8e+9 > Exportações <= 1e+10",
      sum_hex_vl_fob > 10*10^9 ~ "Exportações >= 1e+10"
    ),
    sum_hex_vl_fob_cat2 = case_when(
      sum_hex_vl_fob <= 2*10^9 ~ 0,
      sum_hex_vl_fob > 2*10^9 & sum_hex_vl_fob <= 4*10^9 ~ 1,
      sum_hex_vl_fob > 4*10^9 & sum_hex_vl_fob <= 6*10^9 ~ 2,
      sum_hex_vl_fob > 6*10^9 & sum_hex_vl_fob <= 8*10^9 ~ 3,
      sum_hex_vl_fob > 8*10^9 & sum_hex_vl_fob <= 10*10^9 ~ 4,
      sum_hex_vl_fob > 10*10^9 ~ 5
    )
  )


# Plots -------------------------------------------------------------------
gc()

(g1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_hex, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
   ggplot2::labs(
    title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    caption = "Dados Secex",
    fill = "Legenda"
  ))

(g2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_hex, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    caption = "Dados Secex",
    fill = "Legenda"
  ))

(g3 <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_meso, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    caption = "Dados Secex",
    fill = "Legenda"
  ))

(g4 <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_micro, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    caption = "Dados Secex",
    fill = "Legenda"
  ))

(g5 <- ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill=sum_vl_fob), data = sul_muni, color = "black", alpha=1) +
    ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
    ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5)
    ) +
    ggplot2::labs(
      title = "Exportações do Sul do Brasil agregadas em hexágonos", 
      subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
      caption = "Dados Secex",
      fill = "Legenda"
    ))

ggpubr::ggarrange(
  g3, 
  g4, 
  # g5, 
  g2,
  # g1, 
  ncol = 5
)
