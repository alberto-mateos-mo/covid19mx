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
    # textOutput(ns("n_casos")),
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
  
  casos_fecha <- table(casos_positivos$fecha) %>% 
    as.data.frame()
  
  casos_fecha <- janitor::clean_names(casos_fecha)
  
  names(casos_fecha) <- c("fecha", "freq")
  
  casos_fecha <- casos_fecha %>% 
    dplyr::mutate(fecha = lubridate::dmy(fecha))
  
  casos_fecha <- dplyr::arrange(casos_fecha, fecha)
  
  casos_fecha <- casos_fecha %>% 
    dplyr::mutate(cum_freq = cumsum(freq))
  
  output$casos_acum <- plotly::renderPlotly({
    
    plotly::ggplotly(ggplot2::ggplot(casos_fecha)+
                       ggplot2::geom_line(ggplot2::aes(fecha, cum_freq))+
                       ggplot2::geom_point(ggplot2::aes(fecha, cum_freq))+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos acumulados por fecha de inicio de síntomas")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)))
  })
  
  output$casos_freq <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_fecha)+
                       ggplot2::geom_line(ggplot2::aes(fecha, freq))+
                       ggplot2::geom_point(ggplot2::aes(fecha, freq))+
                       ggplot2::scale_x_date(breaks = "week")+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos por fecha de inicio de síntomas")+
                       ggplot2::theme_minimal()+
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)))
  })
  
  output$casos_gen <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_positivos)+
                       ggplot2::geom_bar(ggplot2::aes("genero", fill = genero), position = "fill")+
                       ggplot2::coord_flip()+
                       ggplot2::labs(x = "", y = "")+
                       ggplot2::ggtitle("Casos por género")+
                       ggplot2::theme_minimal()+
                       ggplot2::scale_fill_discrete("Género")+
                       ggplot2::theme(axis.text = ggplot2::element_blank(), 
                             panel.grid = ggplot2::element_blank(), 
                             axis.ticks = ggplot2::element_blank(), 
                             axis.line = ggplot2::element_blank()))
  })
  
  output$casos_edad <- plotly::renderPlotly({
    plotly::ggplotly(ggplot2::ggplot(casos_positivos)+
                       ggplot2::geom_bar(ggplot2::aes(edad))+
                       ggplot2::labs(x = "Edad", y = "Casos")+
                       ggplot2::ggtitle("Casos por edad")+
                       ggplot2::theme_minimal())
  })
  
  # output$n_casos <- renderText({
  #   h3(paste(nrow(casos_positivos), "\n", "Casos confirmados."))
  # })
  
}
    
## To be copied in the UI
# mod_data_vis_ui("data_vis_ui_1")
    
## To be copied in the server
# callModule(mod_data_vis_server, "data_vis_ui_1")
 
