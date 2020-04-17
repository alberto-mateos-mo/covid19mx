# Module UI
  
#' @title   mod_data_tables_ui and mod_data_tables_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_data_tables
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_data_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, align = "center",
             h2("Casos por municipio")
      ),
      column(width = 12, align = "center",
             selectInput(ns("estado"), "Selecciona el estado de residencia:", 
                         choices = levels(as.factor(covid_data$entidad_res)))
      )
    ),
    fluidRow(
      column(width = 12, align = "center",
             DT::DTOutput(ns("tabla"), width = "50%")
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_data_tables
#' @export
#' @keywords internal
    
mod_data_tables_server <- function(input, output, session){
  ns <- session$ns
  
  covid_edo <- reactive({
    covid_data %>% 
      filter(resultado == "Positivo SARS-CoV-2") %>% 
      filter(entidad_res == input$estado) %>% 
      group_by(municipio_res) %>% 
      summarise(casos = n())
  })
  
  output$tabla <- DT::renderDT(server = FALSE, {
    DT::datatable(covid_edo(),
                  rownames = FALSE,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
  })  
  
}
    
## To be copied in the UI
# mod_data_tables_ui("data_tables_ui_1")
    
## To be copied in the server
# callModule(mod_data_tables_server, "data_tables_ui_1")
 
