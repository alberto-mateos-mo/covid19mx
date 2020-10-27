## code to prepare covid19 MX data
## The data needs to be updated daily

require(plyr)
require(dplyr)
require(magrittr)
require(datoscovid19mx)

# Data recoding

datos <- covidmx

data("diccionarios")

diccionarios %<>% lapply(., function(x) janitor::clean_names(x))

datos$municipio_res <- as.numeric(paste0(datos$municipio_res, datos$entidad_res))

diccionarios$municipios$clave2 <- as.numeric(paste0(as.numeric(diccionarios$municipios$clave_municipio), as.numeric(diccionarios$municipios$clave_entidad)))

datos$origen <- mapvalues(datos$origen, from = diccionarios$origen$clave, to = diccionarios$origen$descripcion)

datos$sector <- mapvalues(datos$sector, from = diccionarios$sector$clave, to = diccionarios$sector$descripcion)

datos$sexo <- mapvalues(datos$sexo, from = diccionarios$sexo$clave, to = diccionarios$sexo$descripcion)

datos$entidad_um <- mapvalues(datos$entidad_um, from = as.numeric(diccionarios$entidad$clave_entidad), to = diccionarios$entidad$entidad_federativa)

datos$entidad_nac <- mapvalues(datos$entidad_nac, from = as.numeric(diccionarios$entidad$clave_entidad), to = diccionarios$entidad$entidad_federativa)

datos$entidad_res <- mapvalues(datos$entidad_res, from = as.numeric(diccionarios$entidad$clave_entidad), to = diccionarios$entidad$entidad_federativa)

datos$municipio_res <- mapvalues(datos$municipio_res, from = diccionarios$municipios$clave2, to = diccionarios$municipios$municipio)

datos$tipo_paciente <- mapvalues(datos$tipo_paciente, from = diccionarios$paciente$clave, to = diccionarios$paciente$descripcion)

datos$intubado <- mapvalues(datos$intubado, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$neumonia <- mapvalues(datos$neumonia, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$nacionalidad <- mapvalues(datos$nacionalidad, from = diccionarios$nacionalidad$clave, to = diccionarios$nacionalidad$descripcion)

datos$embarazo <- mapvalues(datos$embarazo, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$habla_lengua_indig <- mapvalues(datos$habla_lengua_indig, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$diabetes <- mapvalues(datos$diabetes, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$epoc <- mapvalues(datos$epoc, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$asma <- mapvalues(datos$asma, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$inmusupr <- mapvalues(datos$inmusupr, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$hipertension <- mapvalues(datos$hipertension, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$otra_com <- mapvalues(datos$otra_com, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$cardiovascular <- mapvalues(datos$cardiovascular, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$obesidad <- mapvalues(datos$obesidad, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$renal_cronica <- mapvalues(datos$renal_cronica, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$tabaquismo <- mapvalues(datos$tabaquismo, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$otro_caso <- mapvalues(datos$otro_caso, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$resultado <- mapvalues(datos$resultado, from = diccionarios$resultado$clave, to = diccionarios$resultado$descripcion)

datos$migrante <- mapvalues(datos$migrante, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

datos$uci <- mapvalues(datos$uci, from = diccionarios$sino$clave, to = diccionarios$sino$descripcion)

covid_clean <- datos

rm(datos)

positivos_anteriores <- nrow(covid19mx::casos_positivos)

positivos <- covid_clean %>% 
  filter(resultado_lab == 1)

casos_positivos <- data.frame(caso = 1:nrow(positivos), estado = positivos$entidad_res, genero = positivos$sexo, 
                           edad = as.numeric(positivos$edad), fecha = lubridate::ymd(positivos$fecha_sintomas))

casos_positivos <- fastDummies::dummy_cols(casos_positivos, select_columns = c("genero"))

mapa_data <- casos_positivos %>% 
  dplyr::group_by(estado) %>% 
  dplyr::summarise(casos = dplyr::n(), edad_prom = mean(edad, na.rm = TRUE), edad_med = median(edad, na.rm = TRUE), 
            n_M = sum(genero_HOMBRE, na.rm = TRUE), n_F = sum(genero_MUJER, na.rm = TRUE))

mapa_data$estado <- as.character(mapa_data$estado) %>% trimws()

mapa_data <- left_join(mapa_data, covid19mx::estados_coords, by = "estado")

mapa_data$casos_clase <- cut(mapa_data$casos, 
                             c(1, 5000, 10000, 20000, 30000, 40000, 50000, 75000, 100000, 200000), include.lowest = T,
                             labels = c('1-5000', '5001-10000', '10001-20000', '20001-30000', '30001-40000', '40001-50000', '50001-75000', 
                                        '75001-100000', '+100000'))


incremento <- nrow(casos_positivos)-positivos_anteriores

usethis::use_data(casos_positivos, overwrite = TRUE)
usethis::use_data(mapa_data, overwrite = TRUE)
covid_data <- positivos
usethis::use_data(covid_data, overwrite = TRUE)

# estados_coords <- clipr::read_clip_tbl(header = TRUE)
# estados_coords$lat <- estados_coords$lat %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
# estados_coords$lon <- estados_coords$lon %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
# usethis::use_data(estados_coords, overwrite = TRUE)
