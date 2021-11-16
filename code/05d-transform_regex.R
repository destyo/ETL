library(stringr)
x <- c("apple", "banana", "pear")

# numeric output (nº de elemento q contiene la e)
str_which(x, "e")

# character output (si contine la e devuelve el elemento)
str_subset(x, "e")

# logical output (true: contiene la e)
str_detect(x, "e")

# Atención a las mayúsculas
apples <- c("apple", "Apple", "APPLE")
str_detect(apples, "apple")

str_detect(tolower(apples), "apple")
str_detect(apples, regex("apple", ignore_case = TRUE))

library(dplyr)

#clase
#Decir si el nombre tiene w
fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(contiene_w = str_detect(lead_actor_actress, 'w'))

#Extraer el nombre de pila
fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(nombre_pila = str_extract(lead_actor_actress, '[A-z]+'))
#Extraer nombre con guión
fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(nombre_pila = str_extract(lead_actor_actress, '[A-z//-]+'))

#Extraer a(pellido
fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(apellido = str_extract(lead_actor_actress, '[A-z//-]+$'))

#Nombre transformado para gurdar como variable
fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(apellido = tolower(str_replace_all(
    lead_actor_actress,
    "\\s|-",
    "_"
    )))


fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(surname = str_extract(lead_actor_actress, "[A-z]+$"))

fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(name_altered = str_to_lower(lead_actor_actress), 
         name_altered = str_replace_all(name_altered, " ", "_"))


fivethirtyeight::biopics %>% 
  select(lead_actor_actress) %>% 
  mutate(name_altered = str_to_lower(lead_actor_actress), 
         name_altered = str_replace_all(name_altered, " |-", "_"))
