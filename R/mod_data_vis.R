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
    mainPanel(
      plotOutput(ns("casos_acum"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_vis
#' @export
#' @keywords internal
    
mod_data_vis_server <- function(input, output, session){
  ns <- session$ns
  
}
    
## To be copied in the UI
# mod_data_vis_ui("data_vis_ui_1")
    
## To be copied in the server
# callModule(mod_data_vis_server, "data_vis_ui_1")
 
