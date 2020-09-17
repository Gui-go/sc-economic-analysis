# R-script 00-rascunho.R

# Setup -------------------------------------------------------------------
rm(list = ls())
gc()
options(stringsAsFactors = F)

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

# Data --------------------------------------------------------------------

tx_alfa <- readr::read_csv(file = "data/sidra_data/limpo/tabela1383_limpo.csv") %>% 
  dplyr::filter(cod > 4200 & cod < 4300) %>% 
  dplyr::select(cod, mesoregiao, tx_alfabetizacao)











# SHP ---------------------------------------------------------------------


shp_sc <- sf::st_read("data/shp_data/microrregioes_e_mesorregioes_geograficas_1990/") %>% 
  janitor::clean_names() %>%
  dplyr::filter(uf == "42") %>% 
  dplyr::group_by(codmeso) %>% 
    dplyr::summarise(
      coduf = first(uf),
      estado = "Santa Catarina",
      nomemeso = dplyr::first(nomemeso)
    )

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(data = shp_sc)


# IBGE --------------------------------------------------------------------

library("shiny")
library("shinydashboard")
library("ggplot2")
library("DT")
library("forecast")
library("moments")
library("tseries")
library("Quandl")
library("leaflet")

# BCB Series:



# bcbsraw <- Quandl::Quandl("BCB/1477", type = "raw", collapse = "monthly", api_key = "gGdvN9gXsx9hxHMTWPNL")
bcbsraw2 <- bcbsraw[seq(dim(bcbsraw)[1],1),]
bcbsraw3 <- bcbsraw2[bcbsraw2[,1] >= as.Date("2016-01-01") & bcbsraw2[,1] <= today(), ]
bcbsts <- ts(bcbsraw3[,2], start = c(as.numeric(substring(bcbsraw3[1,1],1,4)), as.numeric(substring(bcbsraw3[1,1],6,7))), frequency = 12)
bcbsts



  

autoplot(bcbsts)+
  geom_hline(yintercept = median(bcbsts), color = "blue")+
  geom_hline(yintercept = mean(bcbsts), color = "red")+
  labs(title = "Gráfico de linha da série temporal",
       x = "Tempo",
       y = "Y")+
  theme_minimal()

ggseasonplot(bcbsts)+
  labs(title = "Gráfico de linhas sazonais",
       x = "Tempo",
       y = "Y")+
  theme_minimal()

# ggsubseriesplot(bcbsts)+
#   labs(title = "Gráfico de subséries",
#        x = "Tempo",
#        y = "Y")+
#   theme_minimal()

ggAcf(bcbsts, lag.max = 60)+
  labs(title = "Função de auto-correlação")+
  theme_minimal()

ggPacf(bcbsts, lag.max = 60)+
  labs(title = "Função de auto-correlação parcial")+
  theme_minimal()

ARIMA.model <- Arima(bcbsts, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))

FF <- forecast(ARIMA.model, h = 12)
autoplot(FF)+
  autolayer(FF$mean, color = "blue")+
  labs(y = "Y", x = "Tempo")+
  theme(legend.position = "none")+
  theme_minimal()

FIT <- data.frame(OBSERVADO = bcbsts,
                  AJUSTE = ARIMA.model$fitted,
                  TEMPO = bcbsraw3[ ,1])
ggplot(FIT)+
  geom_line(aes(y = FIT$OBSERVADO,
                x = FIT$TEMPO),
            alpha = 0.8, size = 1.2)+
  geom_line(aes(y = FIT$AJUSTE,
                x = FIT$TEMPO),
            alpha = 0.6, size = 1.2, col = "blue")+
  labs(y = "Y",
       x = "Observação",
       title = "Ajuste do modelo")+
  theme_minimal()

DFA <- data.frame(res = residuals(ARIMA.model))
ggplot(DFA)+
  geom_segment(aes(x = c(1:length(DFA$res)), y = 0, xend = c(1:length(DFA$res)), yend = DFA$res))+
  geom_hline(yintercept = c(sd(DFA$res), -sd(DFA$res)),linetype = "dashed", alpha = 0.3)+
  geom_hline(yintercept = c(2*sd(DFA$res), -2*sd(DFA$res)),linetype = "dashed", alpha = 0.3)+
  geom_hline(yintercept = c(3*sd(DFA$res), -3*sd(DFA$res)),linetype = "dashed", alpha = 0.3)+
  geom_hline(yintercept = 0)+
  labs(title = "Distância entre o modelo e o observado",
       y = "Resíduos",
       x = "Observações")+
  theme_minimal()

ND <- bcbsts
NF <- forecast(Arima(ND, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12)), h = 12)
NB <- bcbsts

autoplot.zoo(ND)+
  autolayer(NF$mean, size = 0.8, color = "blue", alpha = 0.6)+
  autolayer(as.ts(NB), size = 0.6, color = "red", alpha = 0.3)+
  theme_minimal()

aa <- auto.arima(bcbsts, seasonal = T, trace = F, ic = "aic")
