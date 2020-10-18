# R-script 02.R

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
options(scipen = 13)
options(scipen = 0)

# Grande Florianopolis
X_gf <- 1.65*10^8   
(X_gf/tx)*100
M_gf <- 1.57*10^9   #1.57 Bilhão
NX_gf <- X_gf - M_gf

# Vale do Itajaí
X_vi <- 8.65*10^9   
(X_vi/tx)*100
M_vi <- 8.72*10^9   
NX_vi <- X_vi - M_vi

# Norte catarinense
X_nc <- 3.12*10^9
(X_nc/tx)*100
M_nc <- 4.17*10^9   
NX_nc <- X_nc - M_nc

# Oeste Catarinense
X_oc <- 9.77*10^8   
(X_oc/tx)*100
M_oc <- 3.92*10^8   
NX_oc <- X_oc - M_oc

# Sul Catarinense
X_sul <- 7.06*10^8   
(X_sul/tx)*100
M_sul <- 5.19*10^8   
NX_sul <- X_sul - M_sul

# Serra Catarinense
X_serra <- 5.11*10^8   
(X_serra/tx)*100
M_serra <- 6.13*10^7   
NX_serra <- X_serra - M_serra

# Santa Catarina
X_sc <- 1.41*10^10   
M_sc <- 1.54*10^10  
NX_sc <- X_sc - M_sc

#
tx <- X_gf + X_vi + X_nc + X_oc + X_sul + X_serra

