#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(
      title = ("covid19 MX"),
      tabPanel("Reporte",
               mod_data_vis_ui("data_vis_ui_1")
               ),
      tabPanel("Mapa",
               mod_data_map_ui("data_map_ui_1")
               ),
      tabPanel("Datos",
               mod_data_download_ui("data_download_ui_1")
               ),
      inverse = TRUE,
      footer = "Fecha de actualización: 16/04/2020"
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  # addResourcePath(
  #   'www', system.file('app/www', package = 'covid19mx')
  # )
 
  bootstraplib::bs_theme_new(version = "4+3", bootswatch = "lux")
  bootstraplib::bs_theme_add_variables(`font-size-base` = "1rem")
  bootstraplib::bs_theme_add_variables(`input-border-color` = "#6a7c8e", 
                                       primary = "#343a40", default = "#6a7c8e", secondary = "#c09086", 
                                       light = "#f2d0b7", `gray-800` = "#333333", 
                                       black = "#343a40")
  
  tags$head(
    golem::activate_js(),
    bootstraplib::bootstrap(minified = FALSE)
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
