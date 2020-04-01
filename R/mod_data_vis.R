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
    mutate(fecha = lubridate::dmy(fecha))
  
  casos_fecha <- arrange(casos_fecha, fecha)
  
  casos_fecha <- casos_fecha %>% 
    mutate(cum_freq = cumsum(freq))
  
  output$casos_acum <- plotly::renderPlotly({
    
    plotly::ggplotly(ggplot(casos_fecha)+
                       geom_line(aes(fecha, cum_freq))+
                       geom_point(aes(fecha, cum_freq))+
                       scale_x_date(breaks = "week")+
                       labs(x = "", y = "")+
                       ggtitle("Casos acumulados por fecha de inicio de síntomas")+
                       theme_unam()+
                       theme(axis.text.x = element_text(angle = 90)))
  })
  
  output$casos_freq <- plotly::renderPlotly({
    plotly::ggplotly(ggplot(casos_fecha)+
                       geom_line(aes(fecha, freq))+
                       geom_point(aes(fecha, freq))+
                       scale_x_date(breaks = "week")+
                       labs(x = "", y = "")+
                       ggtitle("Casos por fecha de inicio de síntomas")+
                       theme_unam()+
                       theme(axis.text.x = element_text(angle = 90)))
  })
  
  output$casos_gen <- plotly::renderPlotly({
    plotly::ggplotly(ggplot(casos_positivos)+
                       geom_bar(aes("genero", fill = genero), position = "fill")+
                       coord_flip()+
                       labs(x = "", y = "")+
                       ggtitle("Casos por género")+
                       theme_unam()+
                       scale_fill_discrete("Género")+
                       theme(axis.text = element_blank(), 
                             panel.grid = element_blank(), 
                             axis.ticks = element_blank(), 
                             axis.line = element_blank()))
  })
  
  output$casos_edad <- plotly::renderPlotly({
    plotly::ggplotly(ggplot(casos_positivos)+
                       geom_bar(aes(edad))+
                       labs(x = "Edad", y = "Casos")+
                       ggtitle("Casos por edad")+
                       theme_unam())
  })
  
}
    
## To be copied in the UI
# mod_data_vis_ui("data_vis_ui_1")
    
## To be copied in the server
# callModule(mod_data_vis_server, "data_vis_ui_1")
 
