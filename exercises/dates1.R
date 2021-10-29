#library(DBI) <---> DBI:: (para cargar solo una función)

con <- DBI::dbConnect(RSQLite::SQLite(), "~/data/pets.sqlite")

DBI::dbListTables(conn)