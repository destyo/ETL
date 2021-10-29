library(stringr)
x <- c("apple", "banana", "pear")

# numeric output
str_which(x, "e")

# character output
str_subset(x, "e")

# logical output
str_detect(x, "e")

# Atención a las mayúsculas
apples <- c("apple", "Apple", "APPLE")
str_detect(apples, "apple")

str_detect(apples, regex("apple", ignore_case = TRUE))

library(dplyr)
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
