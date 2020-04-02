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
               )
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
  bootstraplib::bs_theme_add_variables(`body-color` = "#002B7A", 
                                       `input-border-color` = "#002B7A", primary = "#D59F0F", 
                                       default = "#D59F0F", secondary = "#D59F0F", `gray-900` = "#002B7A")
  
  tags$head(
    golem::activate_js(),
    bootstraplib::bootstrap(minified = FALSE)
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
