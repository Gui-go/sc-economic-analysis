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
    
    
    df_sul <- left_join(shp_br_muni, df, by="cd_mun") %>% 
      dplyr::filter(sigla_uf=="SC")
    df_sul$sum_vl_fob[is.na(df_sul$sum_vl_fob)] <- 0
    sul_hex <- df_sul %>% 
      dplyr::group_by(sigla_uf) %>% 
      dplyr::summarise() 
    sul_hexs <- sul_hex %>% 
      shpToHexagon(shp = ., cellsize = 1.15)
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
        )
      )

    list(
      "sul_hex_x" = sul_hex_x
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
    # reac_br_uf <- reac()[["shp_br_uf"]]
    # reac_hex_sul <- reac()[["hex_sul"]]
    sul_hex_x <- reac()[["sul_hex_x"]]
    ggplot() +
      # geom_raster(aes(fill = sum_hex_sul_x), data = hex_sul_x)
      geom_sf(aes(fill=sum_hex_sul_hexcolor), data = sul_hex_x, color = "black", alpha=.5) +
      geom_sf(data = sul_hex_x, color = "black", fill = NA, size = .8)+
      # scale_fill_gradientn(colours = c("#e3e3e3", "#5c5c5c", "#303030", "#000000")) +
      labs(fill = "Legenda")+
      # ggplot2::scale_colour_manual(name = "grp",values = RColorBrewer::brewer.pal(length(unique(sul_hex_x$sum_hex_sul_hexcolor)),"Set1"))+
      theme_void()
  })
  
  # output$input_ui_mun <- shiny::renderUI({
  #   
  #   shinyWidgets::pickerInput(
  #     inputId = "input_serv_mun", 
  #     label = "Escolha os municípios", 
  #     choices = "colnames(expcmtm)[-c(1, 2, length(expcmtm))]", 
  #     selected = "colnames(expcmtm)[-c(1, 2, length(expcmtm))]", 
  #     multiple = T, 
  #     options = shinyWidgets::pickerOptions(
  #       actionsBox = TRUE,
  #       selectAllText = "Todos",
  #       deselectAllText = "Nenhum" 
  #     )
  #   )
  # })
  
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