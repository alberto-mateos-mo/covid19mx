require(pdftools)
require(tidyverse)
require(ggplot2)
require(unam.theme)

strsplit2 <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE,
                     ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

url_sospechosos <- "https://www.gob.mx/cms/uploads/attachment/file/544539/Tabla_casos_sospechosos_COVID-19_2020.03.31.pdf"

download.file(url_sospechosos, 'casos_sospechosos.pdf', mode="wb")

url_positivos <- "https://www.gob.mx/cms/uploads/attachment/file/544538/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.31.pdf"

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

positivos_df <- data.frame(caso = 1:length(doc), estado = estados, genero = generos, edad = edades, fecha = fechas) %>% 
  na.omit()

casos_fecha <- table(fechas) %>% 
  as.data.frame()

casos_fecha <- janitor::clean_names(casos_fecha)

casos_fecha <- casos_fecha %>% 
  mutate(fechas = lubridate::dmy(fechas))

casos_fecha <- arrange(casos_fecha, fechas)

casos_fecha <- casos_fecha %>% 
  mutate(cum_freq = cumsum(freq))

plotly::ggplotly(ggplot(casos_fecha)+
  geom_line(aes(fechas, cum_freq))+
  geom_point(aes(fechas, cum_freq))+
  scale_x_date(breaks = "day")+
  theme_unam()+
  theme(axis.text.x = element_text(angle = 90)))


plotly::ggplotly(ggplot(casos_fecha)+
  geom_line(aes(fechas, freq))+
  geom_point(aes(fechas, freq))+
  scale_x_date(breaks = "day")+
  theme_unam()+
  theme(axis.text.x = element_text(angle = 90)))

plotly::ggplotly(ggplot(casos_positivos)+
                   geom_bar(aes("genero", fill = genero), position = "fill")+
                   coord_flip()+
                   labs(x = "", y = "")+
                   ggtitle("Casos por género")+
                   theme_unam()+
                   scale_fill_discrete("Género")+
                   theme(axis.text = element_blank(), 
                         panel.grid = element_blank(), 
                         axis.ticks = element_blank(), 
                         axis.line = element_blank()))

plotly::ggplotly(ggplot(casos_positivos)+
  geom_bar(aes(as.numeric(edad)))+
  labs(x = "Edad", y = "Casos")+
  theme_unam())

ggplot(casos_positivos)+
  geom_density(aes(as.numeric(edad)))
