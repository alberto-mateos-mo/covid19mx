## code to prepare covid19 MX data
## The data needs to be updated daily

require(pdftools)
require(tidyverse)

#url_sospechosos <- "https://www.gob.mx/cms/uploads/attachment/file/544539/Tabla_casos_sospechosos_COVID-19_2020.03.31.pdf"

download.file(url_sospechosos, 'casos_sospechosos.pdf', mode="wb")

url_positivos <- "https://www.gob.mx/cms/uploads/attachment/file/544743/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.04.01.pdf"

download.file(url_positivos, "casos_positivos.pdf", mode = "wb")

doc <- pdf_text("casos_positivos.pdf") %>% 
  readr::read_lines(skip_empty_rows = TRUE, skip = 6) %>% 
  trimws() %>% 
  strsplit2(., "\\s[FM]\\s", type = "before")

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

generos <- lapply(strsplit2(tmp, "^[MF]", type = "after"),
                  function(x){
                    trimws(x[1])
                  }) %>% unlist()

tmp <- lapply(strsplit2(tmp, "^[MF]", type = "after"),
              function(x){
                trimws(x[2])
              }) %>% unlist()

edades <- lapply(strsplit2(tmp, " ", type = "after"),
                 function(x){
                   trimws(x[1])
                 }) %>% unlist()

fechas <- str_extract(doc, pattern = "\\d{1,2}\\/\\d{1,2}\\/\\d{4}")

positivos_df <- data.frame(caso = 1:length(doc), estado = estados, genero = generos, edad = as.numeric(edades), fecha = fechas) %>% 
  na.omit() %>% 
  mutate(caso = 1:length(estado))

casos_positivos <- positivos_df

usethis::use_data(casos_positivos, overwrite = TRUE)
