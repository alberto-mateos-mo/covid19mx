## code to prepare covid19 MX data
## The data needs to be updated daily

require(pdftools)
require(tidyverse)

#url_sospechosos <- "https://www.gob.mx/cms/uploads/attachment/file/544539/Tabla_casos_sospechosos_COVID-19_2020.03.31.pdf"

# download.file(url_sospechosos, 'casos_sospechosos.pdf', mode="wb")

url_positivos <- "https://www.gob.mx/cms/uploads/attachment/file/545211/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.03.pdf"

download.file(url_positivos, "casos_positivos.pdf", mode = "wb")

doc <- pdf_text("casos_positivos.pdf") %>% 
  readr::read_lines(skip_empty_rows = TRUE, skip = 6) %>% 
  trimws() %>% 
  covid19mx::strsplit2(., "\\s[FM]\\s", type = "before")

estados <- lapply(doc,
                  function(x){
                    trimws(str_remove(x[1], "\\d{1,}"))
                  }) %>% unlist()

tmp <- lapply(doc,
              function(x){
                x[2]
              })

tmp <- unlist(tmp)

tmp <- trimws(gsub("\\s+", " ", tmp))

generos <- lapply(covid19mx::strsplit2(tmp, "^[MF]", type = "after"),
                  function(x){
                    trimws(x[1])
                  }) %>% unlist()

tmp <- lapply(covid19mx::strsplit2(tmp, "^[MF]", type = "after"),
              function(x){
                trimws(x[2])
              }) %>% unlist()

edades <- lapply(covid19mx::strsplit2(tmp, " ", type = "after"),
                 function(x){
                   trimws(x[1])
                 }) %>% unlist()

fechas <- str_extract(doc, pattern = "\\d{1,2}\\/\\d{1,2}\\/\\d{4}")

positivos_df <- data.frame(caso = 1:length(doc), estado = estados, genero = generos, edad = as.numeric(edades), fecha = fechas) %>% 
  na.omit() %>% 
  mutate(caso = 1:length(estado))

casos_positivos <- positivos_df

casos_positivos <- fastDummies::dummy_cols(casos_positivos, select_columns = c("genero"))

mapa_data <- casos_positivos %>% 
  group_by(estado) %>% 
  summarise(casos = n(), edad_prom = mean(edad, na.rm = TRUE), edad_med = median(edad, na.rm = TRUE), 
            n_M = sum(genero_M, na.rm = TRUE), n_F = sum(genero_F, na.rm = TRUE))

mapa_data <- right_join(mapa_data, covid19mx::estados_coords)

mapa_data$casos_clase <- cut(mapa_data$casos, 
                             c(1,50,100,250,500,1000), include.lowest = T,
                             labels = c('1-50', '51-100', '101-250', '251-500', '501-1000'))


usethis::use_data(casos_positivos, overwrite = TRUE)
usethis::use_data(mapa_data, overwrite = TRUE)


# estados_coords <- clipr::read_clip_tbl(header = TRUE)
# estados_coords$lat <- estados_coords$lat %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
# estados_coords$lon <- estados_coords$lon %>% str_replace(pattern = ",", replacement = ".") %>% as.numeric()
# usethis::use_data(estados_coords, overwrite = TRUE)
