# Module UI
  
#' @title   mod_data_vis_ui and mod_data_vis_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_vis
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_vis_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, align = "center",
        selectInput(ns("estado"), "Selecciona el estado:", choices = c("NACIONAL", levels(casos_positivos$estado)))
        )
    ),
    fluidRow(
      col_4(align = "center",
        valueBox(value = ns("n_casos"), subtitle = "Casos confirmados.", color = "white", icon = "vial"),
      ),
      col_4(align = "center",
        valueBox(value = ns("n_estim"), subtitle = "Casos estimados por el modelo centinela.", color = "white", icon = "search")
      ),
      col_4(align = "center",
        valueBox(value = ns("corrf"), subtitle = "Factor de corrección.", color = "white", icon = "calculator")
      )
    ),
    hr(),
    tabsetPanel(
      tabPanel("Comportamiento",
               fluidRow(
                 col_6(plotly::plotlyOutput(ns("casos_acum"))),
                 col_6(plotly::plotlyOutput(ns("casos_freq")))
               )
               ),
      tabPanel("Demográficos",
               fluidRow(
                 col_6(plotly::plotlyOutput(ns("casos_gen"))),
                 col_6(plotly::plotlyOutput(ns("casos_edad")))
               )
               )
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_vis
#' @export
#' @keywords internal
    
mod_data_vis_server <- function(input, output, session){
  ns <- session$ns
  
  casos_f <- reactive({
    if(input$estado == "NACIONAL"){
      return(casos_positivos)
    }else{
      filtro <- casos_positivos %>% 
        dplyr::filter(estado == input$estado)
      return(filtro)
    }
  })
  
  casos_fecha <- reactive({
    
    casos_fecha <- table(casos_f()$fecha) %>% 
      as.data.frame()
    
    casos_fecha <- janitor::clean_names(casos_fecha)
    
    names(casos_fecha) <- c("fecha", "freq")
    
    casos_fecha <- casos_fecha %>% 
      dplyr::mutate(fecha = lubridate::dmy(fecha))
    
    casos_fecha <- dplyr::arrange(casos_fecha, fecha)
    
    casos_fecha <- casos_fecha %>% 
      dplyr::mutate(cum_freq = cumsum(freq))
    
    return(casos_fecha)
  })
  
  output$casos_acum <- plotly::renderPlotly({
    
    plotly::ggplotly(ggplot2::ggplot(casos_fecha())+
                       ggplot2::geom_line(ggplot2::aes(fecha, cum_freq), colour = "#6A7C8E")+
                       ggplot2::geom_point(ggplot2::aes(fecha, cum_freq), colour = "#6A7C8E")+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos acumulados por fecha de inicio de síntomas.")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90),
                                      axis.text = ggplot2::element_text(colour = "#343A40"),
                                      title = ggplot2::element_text(colour = "#343A40"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
  
  output$casos_freq <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_fecha())+
                       ggplot2::geom_line(ggplot2::aes(fecha, freq), colour = "#6A7C8E")+
                       ggplot2::geom_point(ggplot2::aes(fecha, freq), colour = "#6A7C8E")+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos por fecha de inicio de síntomas.")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), 
                                      axis.text = ggplot2::element_text(colour = "#343A40"),
                                      title = ggplot2::element_text(colour = "#343A40"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
  
  output$casos_gen <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_f(), ggplot2::aes(genero, fill = genero))+
                       ggplot2::geom_bar(position = "stack")+
                       ggplot2::geom_text(ggplot2::aes(y = (..count..)-((..count..)/8),label = scales::percent((..count..)/sum(..count..))), 
                                          stat = "count")+
                       ggplot2::coord_flip()+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos por género.")+
                       ggplot2::theme_minimal()+
                       ggplot2::scale_fill_manual("Género", values = c("#6A7C8E", "#C09086"))+
                       ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank(), 
                                      axis.line = ggplot2::element_blank(),
                                      title = ggplot2::element_text(colour = "#343A40"),
                                      legend.text = ggplot2::element_text(colour = "#343A40"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
  
  output$casos_edad <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_f())+
                       ggplot2::geom_bar(ggplot2::aes(edad), fill = "#6A7C8E")+
                       ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10))+
                       ggplot2::labs(x = "Edad", y = "Casos")+
                       ggplot2::ggtitle("Casos por edad.")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text = ggplot2::element_text(colour = "#343A40"),
                                      title = ggplot2::element_text(colour = "#343A40"))) %>% 
      plotly::config(displayModeBar = TRUE, modeBarButtons = list(list("toImage")))
  })
  
  output$n_casos <- renderText({
    scales::comma(nrow(casos_f()))
  })
  
  output$n_estim <- renderText({
    scales::comma(nrow(casos_f())*8.885342226)
  })
  
  output$corrf <- renderText({
    round(8.885342226, 2)
  })
  
}
    
## To be copied in the UI
# mod_data_vis_ui("data_vis_ui_1")
    
## To be copied in the server
# callModule(mod_data_vis_server, "data_vis_ui_1")
 
