# R-script 01-dados.R

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

# shp_grint
shp_rgint <- sf::st_read("data/br_regioes_geograficas_intermediarias/") %>%
  janitor::clean_names()
plot(shp_rgint["nm_rgint"])

# shp_grime
shp_rgime <- sf::st_read("data/br_regioes_geograficas_imediatas/") %>%
  janitor::clean_names()
plot(shp_rgime["nm_rgi"])


# SC ----------------------------------------------------------------------

# shp_gri
shp_sc_uf <- sf::st_read("data/SC/SC_UF_2019.shp") %>%
  janitor::clean_names()
plot(shp_sc_uf["nm_uf"])

shp_sc_meso <- sf::st_read("data/SC/SC_Mesorregioes_2019.shp") %>%
  janitor::clean_names()
plot(shp_sc_meso["nm_meso"])

shp_sc_micro <- sf::st_read("data/SC/SC_Microrregioes_2019.shp") %>%
  janitor::clean_names()
plot(shp_sc_micro["nm_micro"])

shp_sc_muni <- sf::st_read("data/SC/SC_Municipios_2019.shp") %>%
  janitor::clean_names()
plot(shp_sc_muni["nm_mun"])

shp_sc_rgime <- sf::st_read("data/SC/SC_RG_Imediatas_2019.shp") %>%
  janitor::clean_names()
plot(shp_sc_rgime["nm_rgi"])

shp_sc_rgint <- sf::st_read("data/SC/SC_RG_Intermediarias_2019.shp") %>%
  janitor::clean_names()
plot(shp_sc_rgint["nm_rgint"])
