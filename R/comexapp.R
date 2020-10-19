# R script - Simplest Shiny App

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
    
    ts <- expcm %>% 
      # dplyr::filter(CO_ANO>=2010) %>% 
      # dplyr::filter(CO_ANO%in%2020) %>% 
      dplyr::filter(SG_UF_MUN=="SC") %>% 
      dplyr::group_by(CO_ANO, CO_MES) %>% 
      dplyr::summarise(
        sum_VL_FOB = sum(VL_FOB)
      )
    
    reaclist <- list(
      "ts_estado" = ts
    )
    
  })
  
  output$input_ui_estado <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_serv_estado", 
      label = "Escolha os estados", 
      choices = c("SC", "RS", "PR"), 
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
    input <- input$input_serv_estado
    df1 <- reac()[["ts_estado"]]
    g1 <- ggplot2::autoplot(df1[, "navec"])
    if("SC" %in% input){g1 <- g1+ggplot2::autolayer(df1[, "SC"], series="SC")}
    if("RS" %in% input){g1 <- g1+ggplot2::autolayer(df1[, "RS"], series="RS")}
    if("PR" %in% input){g1 <- g1+ggplot2::autolayer(df1[, "PR"], series="PR")}
    g1+
      theme(legend.position="bottom")+
      labs(
        y = "Exportações",
        x = "Tempo",
        color = "Legenda"
      )
    
  })
  
  output$input_ui_mun <- shiny::renderUI({
    
    shinyWidgets::pickerInput(
      inputId = "input_serv_mun", 
      label = "Escolha os municípios", 
      choices = colnames(expcmtm)[-c(1, 2, length(expcmtm))], 
      selected = colnames(expcmtm)[-c(1, 2, length(expcmtm))], 
      multiple = T, 
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Todos",
        deselectAllText = "Nenhum" 
      )
    )
  })
  
  output$tsplot_mun <- renderPlot({
    inputs <- input$input_serv_mun
    # inputs <- c("4208708", "4208807", "4208906")
    g2 <- ggplot2::autoplot(expcmtms[, "navec"])
    for(i in seq_along(inputs)) {
      g2 <- g2+ggplot2::autolayer(expcmtms[, inputs[i]])
    }
    g2+
      theme(legend.position="bottom")+
      theme_minimal()+
      labs(
        y = "Exportações",
        x = "Tempo",
        color = "Legenda"
      )

  })
  
  
}
# App ---------------------------------------------------------------------
shinyApp(ui, server)