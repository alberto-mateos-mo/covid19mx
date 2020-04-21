## code to prepare covid19 MX data
## The data needs to be updated daily

require(tidyverse)
require(datoscovid19mx)

positivos <- covid_clean %>% 
  filter(resultado == "Positivo SARS-CoV-2")

positivos_df <- data.frame(caso = 1:nrow(positivos), estado = positivos$entidad_res, genero = positivos$sexo, 
                           edad = as.numeric(positivos$edad), fecha = lubridate::ymd(positivos$fecha_sintomas))

casos_positivos <- positivos_df

casos_positivos <- fastDummies::dummy_cols(casos_positivos, select_columns = c("genero"))

mapa_data <- casos_positivos %>% 
  dplyr::group_by(estado) %>% 
  dplyr::summarise(casos = dplyr::n(), edad_prom = mean(edad, na.rm = TRUE), edad_med = median(edad, na.rm = TRUE), 
            n_M = sum(genero_HOMBRE, na.rm = TRUE), n_F = sum(genero_MUJER, na.rm = TRUE))

mapa_data$estado <- as.character(mapa_data$estado) %>% trimws()

mapa_data <- left_join(mapa_data, covid19mx::estados_coords, by = "estado")

mapa_data$casos_clase <- cut(mapa_data$casos, 
                             c(1,50,100,250,500,1000, 2000, 3000), include.lowest = T,
                             labels = c('1-50', '51-100', '101-250', '251-500', '501-1000', '1001-2000', '2001-3000'))


usethis::use_data(casos_positivos, overwrite = TRUE)
usethis::use_data(mapa_data, overwrite = TRUE)
covid_data <- datoscovid19mx::covid_clean
usethis::use_data(covid_data, overwrite = TRUE)

# estados_coords <- clipr::read_clip_tbl(header = TRUE)
# estados_coords$lat <- estados_coords$lat %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
# estados_coords$lon <- estados_coords$lon %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
# usethis::use_data(estados_coords, overwrite = TRUE)
