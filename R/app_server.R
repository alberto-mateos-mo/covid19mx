#' @import shiny
app_server <- function(input, output,session) {
  callModule(mod_data_vis_server, "data_vis_ui_1")
}
