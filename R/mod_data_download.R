# Module UI
  
#' @title   mod_data_download_ui and mod_data_download_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_download
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_download_ui <- function(id){
  ns <- NS(id)
  tagList(
    mainPanel(
      DT::DTOutput(ns("datos"))
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_download
#' @export
#' @keywords internal
    
mod_data_download_server <- function(input, output, session){
  ns <- session$ns
  
  output$datos <- DT::renderDT(server = FALSE, {
    DT::datatable(casos_positivos,
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
  })
}
    
## To be copied in the UI
# mod_data_download_ui("data_download_ui_1")
    
## To be copied in the server
# callModule(mod_data_download_server, "data_download_ui_1")
 
