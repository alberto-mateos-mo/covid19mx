#' @import shiny
app_server <- function(input, output,session) {
  callModule(mod_data_vis_server, "data_vis_ui_1")
  callModule(mod_data_map_server, "data_map_ui_1")
  callModule(mod_data_download_server, "data_download_ui_1")
}
