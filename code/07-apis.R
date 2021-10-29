library(httr)
# r <- GET("https://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/IPC206446?nult=5")
r <- GET("https://servicios.ine.es/wstempus/js/ES/DATOS_SERIE/IPC206446?date=20210101:20210801")
r

status_code(r)
r$status_code
http_status(r)

headers(r)

# Getting the data ---------------------------------------------------
str(content(r))

datos <- content(r)$Data
class(datos)
length(datos)

library(tidyverse)
df_datos <- map_dfr(datos, function(x){
  tibble(
    year = x$Anyo, 
    month = x$FK_Periodo, 
    value = x$Valor
  )
})

df_datos %>% 
  mutate(date = paste0(year, "-", month, "-01"), 
         date = as.Date(date)) %>% 
  ggplot() + 
  geom_line(aes(x = date, y = value)) + 
  labs(title = "IPC - Año 2021", x = "", y = "IPC") + 
  theme(panel.background = element_blank())