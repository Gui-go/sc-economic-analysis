# R-script 0.R

# Setup -------------------------------------------------------------------
# rm(list = ls())
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
if(!require(readxl)){install.packages("readxl")}
if(!require(janitor)){install.packages("janitor")}
if(!require(spdep)){install.packages("spdep")}
if(!require(vroom)){install.packages("vroom")}

if(!require(shiny)){install.packages("shiny")}
if(!require(shinydashboard)){install.packages("shinydashboard")}
if(!require(shinyWidgets)){install.packages("shinyWidgets")}
if(!require(DT)){install.packages("DT")}
if(!require(forecast)){install.packages("forecast")}
if(!require(moments)){install.packages("moments")}
if(!require(tseries)){install.packages("tseries")}
if(!require(devtools)){install.packages("devtools")}

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "My Template",
    titleWidth = 300
  ),

# Sidebar -----------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "HOME", icon = icon("asterisk")),
      menuItem("Apresentação", tabName = "AP", icon = icon("star")),
      menuItem("Metodologia", tabName = "MET", icon = icon("cog")),
      menuItem("Tema definitivo", tabName = "DEFF", icon = icon("plus"),
               menuSubItem("Contextualização", tabName = "DEFF1", icon = icon("check")),
               menuSubItem("Análise Descritiva", tabName = "DEFF2", icon = icon("check")),
               menuSubItem("Análise Indutiva", tabName = "DEFF3", icon = icon("check"))),
      menuItem("Referências", tabName = "REF", icon = icon("check")),
      menuItem("Sobre", tabName = "SOBRE", icon = icon("paperclip")),
      menuItem("Contato", tabName = "CTT", icon = icon("send"))
    )
  ),
  dashboardBody(
    tabItems(

# HOME --------------------------------------------------------------------
      tabItem(
        tabName = "HOME",
        fluidRow(h1(strong("Home")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12,
            h1(class = "neon","Hoooomie")
          )
        )
      ),

# ap ----------------------------------------------------------------------
      tabItem(
        tabName = "AP",
        fluidRow(h1(strong("Apresentação")), align = "center"),
        fluidPage(
          h2(strong("AP")),
          box(
            width = 12,
            h3("Apresentação")
          )
        )
      ),

# met ---------------------------------------------------------------------
      tabItem(
        tabName = "MET",
        fluidRow(h1(strong("Metodologia")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      ),
      

# def1 --------------------------------------------------------------------
      tabItem(
        tabName = "DEFF1",
        fluidRow(h1(strong("Contextualização")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      ),

# def2 --------------------------------------------------------------------

      
      tabItem(
        tabName = "DEFF2",
        fluidRow(h1(strong("Análise Descritiva")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      ),
      
      ####################### DEFF3 Análise Indutiva #####################
      tabItem(
        tabName = "DEFF3",
        fluidRow(h1(strong("Análise Indutiva")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      ),
      
      
      ####################### REF  ########################
      tabItem(
        tabName = "REF",
        fluidRow(h1(strong("Referências")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      ),
      
      #######################  SOBRE ##################
      tabItem(
        tabName = "SOBRE",
        fluidRow(h1(strong("SOBRE")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      ),
      
      ############################ CTT #####################
      tabItem(
        tabName = "CTT",
        fluidRow(h1(strong("Contato")), align = "center"),
        fluidPage(
          h2(strong("Início")),
          box(
            width = 12
          )
        )
      )
      
      
    )
  )
)

server <- function(input, output){
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)









