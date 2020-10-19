# R-script 01-comexstat-try.R

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

EXP_1997_2020_20201018 <- read_excel("data/EXP_1997_2020_20201018.xlsx") %>% 
  janitor::clean_names()
# View(EXP_1997_2020_20201018)

unique(EXP_1997_2020_20201018[, c("codigo_secao", "descricao_secao")]) %>% arrange(codigo_secao) %>% knitr::kable()
# I - Animais vivos e produtos do reino animal
# XVI - Máquinas e aparelhos, material elétrico e suas partes; Aparelhos de gravação ou reprodução de som, aparelhos de gravação ou reprodução de imagens e de som em televisão, e suas partes e acessórios
# II
# IV


scx <- EXP_1997_2020_20201018 %>% 
  dplyr::rename("uf"="uf_do_municipio", "mun"="municipio", "codsec"="codigo_secao", "section"="descricao_secao", "v20"="x2020_valor_fob_us", "v19"="x2019_valor_fob_us", "v18"="x2018_valor_fob_us", "v17"="x2017_valor_fob_us", "v16"="x2016_valor_fob_us", "v15"="x2015_valor_fob_us", "v14"="x2014_valor_fob_us", "v13"="x2013_valor_fob_us", "v12"="x2012_valor_fob_us", "v11"="x2011_valor_fob_us", "v10"="x2010_valor_fob_us") %>% 
  # dplyr::mutate("X"=sum(v20, v19, v18, v17, v16, v15, v14, v13, v12, v11, v10)) %>% 
  # dplyr::filter(codsec=="XVI") %>%
  dplyr::group_by(mun) %>%
  dplyr::summarise(
    "X"=sum(v20, v19, v18, v17, v16, v15, v14, v13, v12, v11, v10)
  ) %>% 
  # dplyr::mutate(mun = substr(mun, 0, nchar(mun)-5))
  arrange(desc(X))
scx

shp_municipios <- sf::st_read("data/sc_municipios/") %>% 
  dplyr::rename("codmun"="CD_MUN", "mun"="NM_MUN", "uf"="SIGLA_UF", "areakm2"="AREA_KM2") %>% 
  dplyr::mutate(mun=paste0(mun, " - ", uf))


df <- left_join(shp, scx, by="mun")
# View(df)
table(is.na(df$X))
glimpse(df)
# df[which(is.na(df$X)), ]

# mesoregiões
shp_meso <- sf::st_read("data/sc_mesorregioes/") %>%
  janitor::clean_names()
  dplyr::rename("codmun"="CD_MUN", "mun"="NM_MUN", "uf"="SIGLA_UF", "areakm2"="AREA_KM2") %>% 
  dplyr::mutate(mun=paste0(mun, " - ", uf))

# pal <- leaflet::colorNumeric(c("lightgrey", "hotpink4"), 0:1)
# varint <- "total"

# leaflet::leaflet() %>%
#   leaflet::addTiles() %>%
#   leaflet::addPolygons(
#     data = df,
#     fill = TRUE,
#     stroke = TRUE,
#     fillOpacity = 0.60,
#     color = "blue",
#     weight = 1,
#     label = "BR"
#   ) %>% 
#   leaflet::addPolygons(
#     data = df,
#     fill = TRUE,
#     stroke = TRUE,
#     fillOpacity = 0.60,
#     # color= "blue",
#     # color = ~pal(normalize(br[[as.character("total")]])),
#     weight = 1#,
#     # label = lapply(labs(br, varint), htmltools::HTML)
#   )
arrange(df, desc(X))
ggplot(df) +
  geom_sf(aes(fill = X)) +
  # geom_sf_label(aes(label = NAME))
  theme_minimal()

w <- poly2nb(df, queen = T)
ww <-  nb2listw(w, style='B')

# xy <- coordinates(as_Spatial(polygonINFO[, c("centlat", "centlng")]))
# plot(polygonINFO[, c("med_price_m2", "geometry")], col='gray', border='blue', lwd=2)
# plot(w, xy, col='red', lwd=2, add=TRUE)

df$X[is.na(df$X)] <- 0
moran(df$X, ww, n=length(ww$neighbours), S0=Szero(ww))$I
moran.test(df$X, ww, randomisation=FALSE)



# mongo -------------------------------------------------------------------


mongo_url <- "mongodb+srv://Guigo:Liberdade13@cluster0.vckod.gcp.mongodb.net/netxm"
mongolite::mongo(url = mongo_url)$run('{"listCollections":1}')$cursor$firstBatch %>% as_tibble()

db = mongolite::mongo(collection = "expc", url = mongo_url)
df <- data.frame(
  "a"=1,
  "b"=2
)
# Finding the data
# data <- db$find()
# data
db$insert(expc)



# rascunho ----------------------------------------------------------------


# expc <- readr::read_csv(file = "data/EXP_COMPLETA.csv")

expc <- vroom::vroom(file = "data/EXP_COMPLETA.csv")
head(expc)
sapply(expc[, c("CO_VIA")], unique)

# expcm
expcm <- vroom::vroom(file = "data/EXP_COMPLETA_MUN.csv")
head(expcm)
unique(substr(x = expcmt2$CO_MUN, 4, 4))
# gf 420005
# f  4205407

expcmt <- expcm %>% 
  # dplyr::filter(CO_ANO>=2010) %>% 
  # dplyr::filter(CO_ANO%in%2020) %>% 
  # dplyr::filter(SG_UF_MUN=="SC") %>%
  dplyr::group_by(SG_UF_MUN, CO_ANO, CO_MES) %>% 
  dplyr::summarise(
    # CO_MES = as.numeric(CO_MES),
    sum_VL_FOB = sum(VL_FOB)
  ) %>% 
  tidyr::spread(SG_UF_MUN, sum_VL_FOB)

expcmt[is.na(expcmt)] <- 0

expcmt <- expcmt %>% 
  dplyr::mutate(navec = NA)

expcmts <- stats::ts(data = expcmt[, 3:ncol(expcmt)], start = c(expcmt$CO_ANO[1], as.numeric(expcmt$CO_MES[1])), frequency = 12)

ggplot2::autoplot(expcmts[, "navec"])+
  ggplot2::autolayer(expcmts[, "SC"])+
  ggplot2::autolayer(expcmts[, "RS"])
  
g1 <- ggplot2::autoplot(expcmts[, "navec"])+
  labs(y="Exportações")
g1
input <- c("SC", "PR")
if("SC" %in% input){g1 <- g1+ggplot2::autolayer(expcmts[, "SC"])}
if("RS" %in% input){g1 <- g1+ggplot2::autolayer(expcmts[, "RS"])}
if("PR" %in% input){g1 <- g1+ggplot2::autolayer(expcmts[, "PR"])}
g1+
  theme(legend.position="bottom")+
  labs(color="Legenda")

ggplot2::autoplot(expcmts[, "navec"])+
  ggplot2::autolayer(expcmts[, "SC"], series="SC")+
  ggplot2::autolayer(expcmts[, "RS"])+
  theme(legend.position="bottom")+
  labs(
    y = "Exportações",
    x = "Tempo",
    color = "Legenda"
  )



# Municipios --------------------------------------------------------------

expcmtm <- expcm %>% 
  dplyr::filter(SG_UF_MUN=="SC") %>%
  dplyr::group_by(CO_MUN, CO_ANO, CO_MES) %>% 
  dplyr::summarise(
    # CO_MES = as.numeric(CO_MES),
    sum_VL_FOB = sum(VL_FOB)
  ) %>% 
  tidyr::spread(CO_MUN, sum_VL_FOB)

expcmtm[is.na(expcmtm)] <- 0

expcmtm <- expcmtm %>% 
  dplyr::mutate(navec = NA)

expcmtms <- stats::ts(data = expcmtm[, 3:ncol(expcmtm)], start = c(expcmtm$CO_ANO[1], as.numeric(expcmtm$CO_MES[1])), frequency = 12)

names(expcmtms)
ggplot2::autoplot(expcmtms[, "navec"])+
  ggplot2::autolayer(expcmtms[, "4218756"], series="4218756")+
  ggplot2::autolayer(expcmtms[, "4219101"], series="4219101")


g1 <- ggplot2::autoplot(expcmtms[, "navec"])
g1
input <- c("4218608", "4218756", "4219101")
expcmtms[, input[1]]
if("4218756" %in% input){g1 <- g1+ggplot2::autolayer(expcmts[, "4218756"])}
if("4219101" %in% input){g1 <- g1+ggplot2::autolayer(expcmts[, "4219101"])}
if(input[1] %in% input){g1 <- g1+ggplot2::autolayer(expcmtms[, input[1]])}
g1+
  ggplot2::theme(legend.position="bottom")+
  ggplot2::labs(color="Legenda")

