# Module UI
  
#' @title   mod_data_map_ui and mod_data_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("mapa"), height = 600)
  )
}
    
# Module Server
    
#' @rdname mod_data_map
#' @export
#' @keywords internal
    
mod_data_map_server <- function(input, output, session){
  ns <- session$ns
  
  output$mapa <- leaflet::renderLeaflet({
    leaflet::leaflet(mapa_data) %>% 
      leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>% 
      leaflet::addCircleMarkers(lat = ~lat, lng = ~lon, color = ~casosCol(casos_clase), fillOpacity = 1, 
                       popup = ~paste(sep = " ",
                                      "<b>Estado:</b>:", estado, "<br/>",
                                      "<b>Casos:</b>", casos, "<br/>",
                                      "<b>Edad promedio:</b>", round(edad_prom,1), "<br/>",
                                      "<b>Edad mediana:</b>", round(edad_med,1), "<br/>",
                                      "<b>Casos masculinos:</b>", n_M, "<br/>",
                                      "<b>Casos femeninos:</b>", n_F
                       ), 
                       label = ~htmltools::htmlEscape(paste(casos, "casos"))) %>% 
      leaflet::addLegend('topright', pal = casosCol, values = mapa_data$casos_clase,
                title = 'Número de casos',
                opacity = 1)
  })
}
    
## To be copied in the UI
# mod_data_map_ui("data_map_ui_1")
    
## To be copied in the server
# callModule(mod_data_map_server, "data_map_ui_1")
 
