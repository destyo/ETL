
library(readxl)
path_to_file <- "../data/ejemplos_lectura.xlsx"
df_chungo <- read_xlsx(path_to_file, sheet = "Holi")
df_chungo

# Dates as numbers

df_chungo %>% 
  select(chungo_01) %>% 
  mutate(fecha = as.Date(chungo_01, origin = "1899-12-30"))

# Dates as expressions.

df_chungo %>% 
  select(chungo_02) %>% 
  mutate(fecha = as.Date(chungo_02, format = "Hoy es %d de %B de %Y"))

# Expressions in other languages

# Primero le digo a R que voy a trabajar en inglés para el tema de fechas
Sys.setlocale("LC_TIME", "English")

df_chungo %>% 
  select(chungo_03) %>% 
  mutate(fecha = as.Date(chungo_03, format = "%A, %B %d %Y"))

# Ahora pongo de nuevo la config original de idioma
Sys.setlocale("LC_TIME", "Spanish")

# Dates as something very strange. 
# La cuarta columna chunga es un número no entero leído como 
# carácter. Primero tendremos que pasar el carácter a número
# y luego convertimos a fecha. 

library(readr)
df_chungo %>% 
  select(chungo_04) %>% 
  mutate(chungo_04 = parse_number(chungo_04), 
         chungo_04 = as.Date(chungo_04, origin = "1899-12-30"))

# Dates as something even more weird.
# as.Date(as.character(as.Date(43839, origin = "1899-12-30")), format = "%Y-%d-%m")

df_chungo %>% 
  select(chungo_05) %>% 
  mutate(
    fecha = if_else(str_detect(chungo_05, "/"), 
                    as.Date(chungo_05, format = "%m/%d/%y"), 
                    as.Date(as.numeric(chungo_05), origin = "1899-12-30")), 
    fecha = if_else(str_detect(fecha, "09"), 
                    as.Date(as.character(fecha), format = "%Y-%d-%m"), 
                    fecha)
  )

#' Ejemplo. Calculamos el lunes de la semana a la que pertenece cada
#' fecha de la columna `fecha_01`.

df_fechas %>% 
  select(fecha_01) %>% 
  mutate(lunes = floor_date(fecha_01, "week", week_start = 1))

#' Si nos molestan las horas, podemos cambiar el tipo de la columna a
#' tipo fecha. 

df_fechas %>% 
  select(starts_with("fecha")) %>% 
  mutate(across(starts_with("fecha"), as.Date))

df_fechas %>% 
  select(fecha_01) %>% 
  mutate(fecha_01_b = date2ISOweek(fecha_01))
