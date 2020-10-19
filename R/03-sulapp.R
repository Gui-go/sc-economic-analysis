# R script - 

# Setup -------------------------------------------------------------------
# rm(list = ls())
gc()
options(encoding = "UTF-8", stringsAsFactors = F)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  shiny::uiOutput(outputId = "input_ui_estado"),
  shiny::uiOutput(outputId = "input_ui_mun"),
  shiny::plotOutput(outputId = "tsplot"),
  shiny::plotOutput(outputId = "tsplot_mun")
)
# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  reac <- shiny::reactive({
    
    shp_br_muni <- br_muni %>%
      janitor::clean_names() %>% 
      cent_as_cols(.) %>% 
      as.data.frame() %>% 
      dplyr::select(-geometry) %>% 
      sf::st_as_sf(x = ., coords = c("centlng", "centlat")) %>%
      sf::st_set_crs(4326) %>% 
      # dplyr::filter(sigla_uf%in%input$input_serv_estado) %>% 
      group_by(sigla_uf) %>% 
      dplyr::summarise()
    
    hex_sul <- shpToHexagon(shp = shp_br_muni, cellsize = 1)
    
    #
    expcmt <- expcm %>% 
      janitor::clean_names() %>% 
      # dplyr::filter(CO_ANO>=2010) %>% 
      # dplyr::filter(CO_ANO%in%2020) %>% 
      # dplyr::filter(sg_uf_mun%in%input$input_serv_estado) %>%
      dplyr::group_by(co_mun) %>% 
      dplyr::summarise(sum_vl_fob = sum(vl_fob)) %>% 
      dplyr::mutate("cd_mun"=as.character(co_mun))
    expcmt[is.na(expcmt)] <- 0
    sul <- left_join(shp_sul, expcmt, by="cd_mun") %>% 
      dplyr::select(-co_mun)
    sul[is.na(sul)] <- 0
    hex_sul_x <- sf::st_join(hex_sul %>% sf::st_set_crs(4326), sul %>% sf::st_set_crs(4326), join = sf::st_contains) %>%  
      dplyr::group_by(hexagon) %>% 
      dplyr::summarise(
        sum_hex_sul_x = sum(sum_vl_fob)
      )
    hex_sul_x$sum_hex_sul_x[is.na(hex_sul_x$sum_hex_sul_x)] <- 0
    
    
    reaclist <- list(
      "shp_br_uf" = shp_br_muni,
      "hex_sul" = hex_sul,
      "hex_sul_x" = hex_sul_x
    )
    
  })
  
  output$input_ui_estado <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_serv_estado", 
      label = "Escolha os estados", 
      choices = c("SC", "RS", "PR", "SP", "RJ"), 
      selected = "SC", 
      multiple = T, 
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Todos",
        deselectAllText = "Nenhum" 
      )
    )
  })
  
  output$tsplot <- renderPlot({
    reac_br_uf <- reac()[["shp_br_uf"]]
    reac_hex_sul <- reac()[["hex_sul"]]
    reac_hex_sul_x <- reac()[["hex_sul_x"]]
    ggplot() +
      # geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
      # geom_sf(aes(fill=sum_hex_sul_x), data = reac_hex_sul_x, color = "black", alpha=.5) +
      # geom_sf(data = reac_hex_sul, color = "black", fill = NA, size = .8)+
      geom_sf(data = reac_br_uf, color = "black", fill = NA, size = .8)+
      # scale_fill_gradientn(colours = c("#a10000", "#a19600", "#33a100")) +
      theme_minimal()
  })
  
  output$input_ui_mun <- shiny::renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "input_serv_mun", 
      label = "Escolha os municípios", 
      choices = "colnames(expcmtm)[-c(1, 2, length(expcmtm))]", 
      selected = "colnames(expcmtm)[-c(1, 2, length(expcmtm))]", 
      multiple = T, 
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Todos",
        deselectAllText = "Nenhum" 
      )
    )
  })
  
  # output$tsplot_mun <- renderPlot({
  #   inputs <- input$input_serv_mun
  #   # inputs <- c("4208708", "4208807", "4208906")
  #   g2 <- ggplot2::autoplot(expcmtms[, "navec"])
  #   for(i in seq_along(inputs)) {
  #     g2 <- g2+ggplot2::autolayer(expcmtms[, inputs[i]])
  #   }
  #   g2+
  #     theme(legend.position="bottom")+
  #     theme_minimal()+
  #     labs(
  #       y = "Exportações",
  #       x = "Tempo",
  #       color = "Legenda"
  #     )
  #   
  # })
  
  
}
# App ---------------------------------------------------------------------
shinyApp(ui, server)