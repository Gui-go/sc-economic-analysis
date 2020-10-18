# R-script 01-comexstat-try.R

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
if(!require(mongolite)){install.packages("mongolite")}


# Data --------------------------------------------------------------------


library(readxl)
EXP_2016_2020_20201017 <- read_excel("data/netxm/EXP_2016_2020_20201017.xlsx")
