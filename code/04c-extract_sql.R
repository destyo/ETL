#install.packages("dbplyr")
library(dplyr)

# Creamos una base de datos en memoria.
conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# Con dplyr podemos subir una tabla a la base de datos. 
# En este caso, nuestra tabla en un dataframe predefinido en R.
copy_to(conn, mtcars)
DBI::dbListTables(conn)

# Evaluación perezosa == Lazy Query
#tbl() dada una conexión acceder a una tabla, solo te descarga las
#10 primeras filas para evitar cargar tablas demasiado grandes

tbl(conn, "mtcars") %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg)) 
#Podemos acceder a métricas de la base de datos sin cargarla
#Las operaciones q se pueden hacer en una base de datos las podemos hacer
#con dplyr sin necesidad de saber SQL

summary <- tbl(conn, "mtcars") %>% 
  group_by(cyl) %>% 
  summarise(mpg = mean(mpg, na.rm = TRUE)) %>% 
  arrange(desc(mpg))

collect(summary)
#collect
#Para joins dplyr es mu ineficiente
#analysis de datos en SQL datacamp

# summary

summary %>% show_query()

# execute query and retrieve results
summary %>% collect()

own_query <- tbl(conn, sql("SELECT * FROM mtcars LIMIT 10"))
own_query
