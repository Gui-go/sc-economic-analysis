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
source(file = "R/fct_MoransI.R")
# Dados -------------------------------------------------------------------

# Divisão política minucipal+ do Sul
# https://servicodados.ibge.gov.br/api/docs/localidades?versao=1
api_get_dmunisul <- httr::GET(url = "https://servicodados.ibge.gov.br/api/v1/localidades/estados/41|42|43/distritos")
api_content_dmunisul <- httr::content(api_get_dmunisul, as = "text")
api_df_dmunisul <- jsonlite::fromJSON(api_content_dmunisul, flatten = TRUE)
df_dmuni_sul <- api_df_dmunisul %>% 
  janitor::clean_names() %>% 
<<<<<<< HEAD
  dplyr::rename("cd_mun" = "municipio_id")
=======
  dplyr::rename("cd_mun" = "municipio_id") %>% 
  dplyr::group_by(cd_mun, municipio_nome, municipio_microrregiao_id, municipio_microrregiao_nome, municipio_microrregiao_mesorregiao_id, municipio_microrregiao_mesorregiao_nome, municipio_microrregiao_mesorregiao_uf_id, municipio_microrregiao_mesorregiao_uf_regiao_sigla) %>% 
  dplyr::summarise() %>% 
  dplyr::rename("nm_muni"="municipio_nome", "cd_micro"="municipio_microrregiao_id", "nm_micro"="municipio_microrregiao_nome", "cd_meso"="municipio_microrregiao_mesorregiao_id", "mn_Meso"="municipio_microrregiao_mesorregiao_nome", "cd_uf"="municipio_microrregiao_mesorregiao_uf_id", "sg_uf"="municipio_microrregiao_mesorregiao_uf_regiao_sigla")
>>>>>>> fc97b056abe114154665ac9ad72567caaf96ea97
rm(list = c("api_get_dmunisul", "api_content_dmunisul", "api_df_dmunisul"))

# Secex data
# http://www.mdic.gov.br/index.php/comercio-exterior/estatisticas-de-comercio-exterior/base-de-dados-do-comercio-exterior-brasileiro-arquivos-para-download
# 2. Base de dados detalhada por Município da empresa exportadora/importadora e Posição do Sistema Harmonizado (SH4)
# Exportação > Volume único
exp_comex <- vroom::vroom(file = "data/EXP_COMPLETA_MUN.csv")
exp_comex_sul10 <- exp_comex %>% 
  janitor::clean_names() %>% 
  dplyr::filter(co_ano>=2010) %>%
  dplyr::filter(sg_uf_mun%in%c("SC", "RS", "PR")) %>%
  dplyr::mutate("sh2" = substr(sh4, 1, 2)) %>%
  dplyr::group_by(co_mun, sh2) %>% 
  dplyr::summarise(
    sum_vl_fob = sum(vl_fob)
  ) %>% 
  dplyr::mutate(
    "cd_mun"=as.integer(co_mun)
  ) %>% 
  dplyr::select(cd_mun, sh2, sum_vl_fob) 
rm(exp_comex)

# Descrição SH
sh <- readxl::read_excel("data/Tabela-de-códigos-de-mercadorias-NCM-SH2-e-SH4-–-Atualizada-em-10-05-2016.xls", skip = 3)  %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    codigo_ncm_sh2 = zoo::na.locf(codigo_ncm_sh2),
    descricao = zoo::na.locf(descricao)
  ) %>% 
  dplyr::rename(
    "descricao_sh2" = descricao,
    "descricao_sh4" = descricao_1,
    "sh2" = codigo_ncm_sh2,
    "sh4" = codigo_ncm_sh4
  ) %>% 
  dplyr::select(sh2, descricao_sh2) %>% 
  dplyr::group_by(sh2) %>% 
  dplyr::summarise(descricao_sh2 = dplyr::first(descricao_sh2))

exp_muni <- left_join(exp_comex_sul10, sh, by = "sh2") #%>% 
  # dplyr::filter(sh2%in%"26")

rm(sh)

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
sul_muni <- dplyr::left_join(sf_sul_muni, exp_muni, by = "cd_mun") %>%
  left_join(., df_dmuni_sul, by = "cd_mun") %>%
  # dplyr::summarise(sum_vl_fob = sum(sum_vl_fob)) %>% 
  dplyr::mutate(sum_vl_fob = dplyr::if_else(is.na(sum_vl_fob), 0, sum_vl_fob)) %>%
  sf::st_set_crs(4326)

sul_micro <- sul_muni %>% 
  # sf::st_set_crs(4326) %>%
  dplyr::group_by(cd_micro) %>% 
  dplyr::summarise(sum_hex_vl_fob = sum(sum_vl_fob)) %>% 
  dplyr::mutate(
    meanSum_hex_vl_fob = mean(sum_hex_vl_fob, na.rm = T),
    median_hex_vl_fob = median(sum_hex_vl_fob, na.rm = T),
    sd_hex_vl_fob = sd(sum_hex_vl_fob, na.rm = T),
    max_hex_vl_fob = max(sum_hex_vl_fob, na.rm = T),
    min_hex_vl_fob = min(sum_hex_vl_fob, na.rm = T),
    pc20_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .2, na.rm = T),
    pc40_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .4, na.rm = T),
    pc60_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .6, na.rm = T),
    pc80_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .8, na.rm = T),
    pc96_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .96, na.rm = T)
  ) %>% 
  dplyr::mutate(
    sum_hex_vl_fob = if_else(is.na(sum_hex_vl_fob), 0, sum_hex_vl_fob),
    log_hex_vl_fob = log10(x = sum_hex_vl_fob),
    sum_hex_vl_fob_cat1 = case_when(
      sum_hex_vl_fob <= pc20_hex_vl_fob ~ paste0("Exportações <= ", round(pc20_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc20_hex_vl_fob & sum_hex_vl_fob <= pc40_hex_vl_fob ~ paste0(round(pc20_hex_vl_fob, 0), " > Exportações <= ", round(pc40_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc40_hex_vl_fob & sum_hex_vl_fob <= pc60_hex_vl_fob ~ paste0(round(pc40_hex_vl_fob, 0), " > Exportações <= ", round(pc60_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc60_hex_vl_fob & sum_hex_vl_fob <= pc80_hex_vl_fob ~ paste0(round(pc60_hex_vl_fob, 0), " > Exportações <= ", round(pc80_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc80_hex_vl_fob & sum_hex_vl_fob <= pc96_hex_vl_fob ~ paste0(round(pc80_hex_vl_fob, 0), " > Exportações <= ", round(pc96_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc96_hex_vl_fob ~ paste0("Exportações > ", round(pc96_hex_vl_fob, 0))
    ),
    sum_hex_vl_fob_cat2 = case_when(
      sum_hex_vl_fob <= pc20_hex_vl_fob ~ 0,
      sum_hex_vl_fob > pc20_hex_vl_fob & sum_hex_vl_fob <= pc40_hex_vl_fob ~ 1,
      sum_hex_vl_fob > pc40_hex_vl_fob & sum_hex_vl_fob <= pc60_hex_vl_fob ~ 2,
      sum_hex_vl_fob > pc60_hex_vl_fob & sum_hex_vl_fob <= pc80_hex_vl_fob ~ 3,
      sum_hex_vl_fob > pc80_hex_vl_fob & sum_hex_vl_fob <= pc96_hex_vl_fob ~ 4,
      sum_hex_vl_fob > pc96_hex_vl_fob ~ 5
    )
  ) %>% 
  dplyr::mutate(
    sum_hex_vl_fob_cat3 =  factor(
      x = sum_hex_vl_fob_cat2, 
      levels = c("5", "4", "3", "2", "1", "0"), 
      labels = c("Exportações > pc96", "pc80 > Exportações <= pc96", "pc60 > Exportações <= pc80", "pc40 > Exportações <= pc60", "pc20 > Exportações <= pc40", "Exportações <= pc20")
    )
  )

sul_meso <- sul_muni %>% 
  dplyr::group_by(cd_meso) %>% 
  dplyr::summarise(
    sum_hex_vl_fob = sum(sum_vl_fob)
  ) %>%
  dplyr::mutate(
    meanSum_hex_vl_fob = mean(sum_hex_vl_fob, na.rm = T),
    median_hex_vl_fob = median(sum_hex_vl_fob, na.rm = T),
    sd_hex_vl_fob = sd(sum_hex_vl_fob, na.rm = T),
    max_hex_vl_fob = max(sum_hex_vl_fob, na.rm = T),
    min_hex_vl_fob = min(sum_hex_vl_fob, na.rm = T),
    pc20_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .2, na.rm = T),
    pc40_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .4, na.rm = T),
    pc60_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .6, na.rm = T),
    pc80_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .8, na.rm = T),
    pc96_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .96, na.rm = T)
  ) %>% 
  dplyr::mutate(
    sum_hex_vl_fob = if_else(is.na(sum_hex_vl_fob), 0, sum_hex_vl_fob),
    log_hex_vl_fob = log10(x = sum_hex_vl_fob),
    sum_hex_vl_fob_cat1 = case_when(
      sum_hex_vl_fob <= pc20_hex_vl_fob ~ paste0("Exportações <= ", round(pc20_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc20_hex_vl_fob & sum_hex_vl_fob <= pc40_hex_vl_fob ~ paste0(round(pc20_hex_vl_fob, 0), " > Exportações <= ", round(pc40_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc40_hex_vl_fob & sum_hex_vl_fob <= pc60_hex_vl_fob ~ paste0(round(pc40_hex_vl_fob, 0), " > Exportações <= ", round(pc60_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc60_hex_vl_fob & sum_hex_vl_fob <= pc80_hex_vl_fob ~ paste0(round(pc60_hex_vl_fob, 0), " > Exportações <= ", round(pc80_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc80_hex_vl_fob & sum_hex_vl_fob <= pc96_hex_vl_fob ~ paste0(round(pc80_hex_vl_fob, 0), " > Exportações <= ", round(pc96_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc96_hex_vl_fob ~ paste0("Exportações > ", round(pc96_hex_vl_fob, 0))
    ),
    sum_hex_vl_fob_cat2 = case_when(
      sum_hex_vl_fob <= pc20_hex_vl_fob ~ 0,
      sum_hex_vl_fob > pc20_hex_vl_fob & sum_hex_vl_fob <= pc40_hex_vl_fob ~ 1,
      sum_hex_vl_fob > pc40_hex_vl_fob & sum_hex_vl_fob <= pc60_hex_vl_fob ~ 2,
      sum_hex_vl_fob > pc60_hex_vl_fob & sum_hex_vl_fob <= pc80_hex_vl_fob ~ 3,
      sum_hex_vl_fob > pc80_hex_vl_fob & sum_hex_vl_fob <= pc96_hex_vl_fob ~ 4,
      sum_hex_vl_fob > pc96_hex_vl_fob ~ 5
    )
  ) %>% 
  dplyr::mutate(
    sum_hex_vl_fob_cat3 =  factor(
      x = sum_hex_vl_fob_cat2, 
      levels = c("5", "4", "3", "2", "1", "0"), 
      labels = c("Exportações > pc96", "pc80 > Exportações <= pc96", "pc60 > Exportações <= pc80", "pc40 > Exportações <= pc60", "pc20 > Exportações <= pc40", "Exportações <= pc20")
    )
  )

sul_hex <- sf_sul_uf %>%
  sf::st_set_crs(4326) %>%
  shpToHexagon(shp = ., cellsize = 1.15) %>%
  sf::st_join(., sul_muni, join = sf::st_contains) %>%
  dplyr::group_by(hexagon) %>%
  dplyr::summarise(
    sum_hex_vl_fob = sum(sum_vl_fob)
  ) %>%
  dplyr::mutate(
    meanSum_hex_vl_fob = mean(sum_hex_vl_fob, na.rm = T),
    median_hex_vl_fob = median(sum_hex_vl_fob, na.rm = T),
    sd_hex_vl_fob = sd(sum_hex_vl_fob, na.rm = T),
    max_hex_vl_fob = max(sum_hex_vl_fob, na.rm = T),
    min_hex_vl_fob = min(sum_hex_vl_fob, na.rm = T),
    pc20_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .2, na.rm = T),
    pc40_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .4, na.rm = T),
    pc60_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .6, na.rm = T),
    pc80_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .8, na.rm = T),
    pc96_hex_vl_fob = quantile(sum_hex_vl_fob, probs = .96, na.rm = T)
  ) %>% 
  dplyr::mutate(
    sum_hex_vl_fob = if_else(is.na(sum_hex_vl_fob), 0, sum_hex_vl_fob),
    log_hex_vl_fob = log10(x = sum_hex_vl_fob),
    sum_hex_vl_fob_cat1 = case_when(
      sum_hex_vl_fob <= pc20_hex_vl_fob ~ paste0("Exportações <= ", round(pc20_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc20_hex_vl_fob & sum_hex_vl_fob <= pc40_hex_vl_fob ~ paste0(round(pc20_hex_vl_fob, 0), " > Exportações <= ", round(pc40_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc40_hex_vl_fob & sum_hex_vl_fob <= pc60_hex_vl_fob ~ paste0(round(pc40_hex_vl_fob, 0), " > Exportações <= ", round(pc60_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc60_hex_vl_fob & sum_hex_vl_fob <= pc80_hex_vl_fob ~ paste0(round(pc60_hex_vl_fob, 0), " > Exportações <= ", round(pc80_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc80_hex_vl_fob & sum_hex_vl_fob <= pc96_hex_vl_fob ~ paste0(round(pc80_hex_vl_fob, 0), " > Exportações <= ", round(pc96_hex_vl_fob, 0)),
      sum_hex_vl_fob > pc96_hex_vl_fob ~ paste0("Exportações > ", round(pc96_hex_vl_fob, 0))
    ),
    sum_hex_vl_fob_cat2 = case_when(
      sum_hex_vl_fob <= pc20_hex_vl_fob ~ 0,
      sum_hex_vl_fob > pc20_hex_vl_fob & sum_hex_vl_fob <= pc40_hex_vl_fob ~ 1,
      sum_hex_vl_fob > pc40_hex_vl_fob & sum_hex_vl_fob <= pc60_hex_vl_fob ~ 2,
      sum_hex_vl_fob > pc60_hex_vl_fob & sum_hex_vl_fob <= pc80_hex_vl_fob ~ 3,
      sum_hex_vl_fob > pc80_hex_vl_fob & sum_hex_vl_fob <= pc96_hex_vl_fob ~ 4,
      sum_hex_vl_fob > pc96_hex_vl_fob ~ 5
    )
  ) %>% 
  dplyr::mutate(
    sum_hex_vl_fob_cat3 =  factor(
      x = sum_hex_vl_fob_cat2, 
      levels = c("5", "4", "3", "2", "1", "0"), 
      labels = c("Exportações > pc96", "pc80 > Exportações <= pc96", "pc60 > Exportações <= pc80", "pc40 > Exportações <= pc60", "pc20 > Exportações <= pc40", "Exportações <= pc20")
    )#,
    # euclid_vlx_logvlx = as.numeric(dist(rbind(sum_hex_vl_fob, log_hex_vl_fob)))
  ); dplyr::glimpse(sul_hex); gc()


# Plots -------------------------------------------------------------------
gc()

ghexcont1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_hex, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  # annotate("text", x = -56.4, y = -26.6, label = MoransI(SDF = sul_hex, vec = "sum_hex_vl_fob", style = "B"), size=4) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
   ggplot2::labs(
    # title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    # subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    # caption = "Dados Secex",
    fill = "Exportações FOB (US$)"
  ); ghexcont1


ghexcont2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_hex, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  # annotate("text", x = -56.4, y = -26.6, label = MoransI(SDF = sul_hex, vec = "sum_hex_vl_fob", style = "B"), size=4) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#000e3d")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    # title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    # subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    # caption = "Dados Secex",
    fill = "Exportações FOB (US$)"
  ); ghexcont2

g_bw_cat <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob_cat3), data = sul_hex, color = "black", alpha=.8) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  scale_fill_manual(values = colorRampPalette(c("#000000", "#e3e3e3"))(6))+ # Greys, RdBu, Reds, RdGy***, BrBG
  # annotate("text", x = -56.4, y = -26.6, label = MoransI(SDF = sul_hex, vec = "sum_hex_vl_fob_cat2", style = "B"), size=4) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    fill = "Exportações FOB (US$)"
  ); g_bw_cat


g_col_cat <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob_cat3), data = sul_hex, color = "black", alpha=.8) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  scale_fill_manual(values = colorRampPalette(c("#000e3d", "#e3e3e3"))(6))+ # Greys, RdBu, Reds, RdGy***, BrBG, #000000, #e3e3e3, #00a308
  # annotate("text", x = -56.4, y = -26.6, label = MoransI(SDF = sul_hex, vec = "sum_hex_vl_fob_cat2", style = "B"), size=4) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    fill = "Exportações FOB (US$)"
  ); g_col_cat


g_bw_meso <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_meso, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
  annotate("text", x = -56, y = -26.6, label = MoransI(SDF = sul_meso, vec = "sum_hex_vl_fob", style = "B"), size=4) +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    # title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    # subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    # caption = "Dados Secex",
    fill = "Exportações FOB (US$)"
  ); g_bw_meso; gc()

g_col_meso <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_meso, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  annotate("text", x = -56, y = -26.6, label = MoransI(SDF = sul_meso, vec = "sum_hex_vl_fob", style = "B"), size=4) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#000e3d")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    # title = "Exportações do Sul do Brasil agregadas em hexágonos", 
    # subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
    # caption = "Dados Secex",
    fill = "Exportações FOB (US$)"
  ); g_col_meso; gc()

g_bw_micro <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_micro, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  annotate("text", x = -56, y = -26.6, label = MoransI(SDF = sul_micro, vec = "sum_hex_vl_fob", style = "B"), size=4) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    fill = "Exportações FOB (US$)"
  ); g_bw_micro; gc()

g_col_micro <- ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill=sum_hex_vl_fob), data = sul_micro, color = "black", alpha=1) +
  ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
  annotate("text", x = -56, y = -26.6, label = MoransI(SDF = sul_micro, vec = "sum_hex_vl_fob", style = "B"), size=4) +
  ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#000e3d")) + #
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  ) +
  ggplot2::labs(
    fill = "Exportações FOB (US$)"
  ); g_col_micro; gc()

# g5 <- ggplot2::ggplot() +
#     ggplot2::geom_sf(ggplot2::aes(fill=sum_vl_fob), data = sul_muni, color = "black", alpha=1) +
#     ggplot2::geom_sf(data = sf_sul_uf, color = "black", fill = NA, size = 1) +
#     ggplot2::scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) + #
#     ggplot2::theme_void() +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(hjust = 0.5),
#       plot.subtitle = ggplot2::element_text(hjust = 0.5)
#     ) +
#     ggplot2::labs(
#       # title = "Exportações do Sul do Brasil agregadas em hexágonos", 
#       # subtitle = "Distribuição contínua do somatório de jan/2010 à set/2020",
#       # caption = "Dados Secex",
#       fill = "Exportações FOB (US$)"
#     ); g5; gc()

ggpubr::ggarrange(
  g_col_meso,
  g_col_micro,
  ghexcont2,
  g_col_cat,
  ncol = 2, nrow = 2
); gc()

class(sul_hex)
# st_write(sul_hex, "data/sul_hex/sul_hex.shp")
# shex <- st_read("data/sul_hex/sul_hex.shp")
# plot(shex["sm_hx__"])
summary(lm(sul_hex$meanSum_hex_vl_fob ~ sul_hex$median_hex_vl_fob))
# Moran'1 -----------------------------------------------------------------

library(spdep)
sul_hex
w <- spdep::poly2nb(sul_hex, row.names=colnames(sul_hex))
ww <-  spdep::nb2listw(w, style='B')
xy <- sp::coordinates(sf::as_Spatial(get_centroid(sul_hex)[, c("centlat", "centlng")]))
m1 <- spdep::moran.test(sul_hex$sum_hex_vl_fob, ww, randomisation=FALSE)$estimate[[1]]
m1pv <- spdep::moran.test(sul_hex$sum_hex_vl_fob, ww, randomisation=FALSE)$p.value
