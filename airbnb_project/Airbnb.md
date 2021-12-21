CUNEF ETL - Assessment
================
Antonio Tello Gómez

2 de Diciembre de 2021

  - [Introducción](#introducción)
  - [Extract](#extract)
      - [1. Extracción (listings).](#1-extracción-listings)
      - [2. Extracción (reviews).](#2-extracción-reviews)
  - [Transform](#transform)
      - [3. Transformación (listings).](#3-transformación-listings)
      - [4. Transformación (listings).](#4-transformación-listings)
      - [5. Transformación (listings).](#5-transformación-listings)
      - [6. Transformación (reviews).](#6-transformación-reviews)
      - [7. Transformación (reviews).](#7-transformación-reviews)
  - [Load](#load)
      - [8. Carga](#8-carga)

# Introducción

Trabajo final de *Extracción, Transformación y Carga*. MUCD 2021/2022. A
partir de una base de datos con información sobre alojamientos de Airbnb
en Madrid. La tarea consiste en extraer parte de esos datos, construir
nuevas tablas transformándolos con ciertas directrices y subirlas de
nuevo a la base de datos. **Objetivo**: Construir dos tablas:  
**Tabla 1.** Evolución mensual del número de críticas por distrito, con
predicción para el mes siguiente.  
**Tabla 2.** Distribución del tipo de alojamiento por distrito.
Incluyendo nota media ponderada precio mediano.

``` r
#Librerías
library(tidyverse)
library(stringr)
```

# Extract

``` r
#Conexión a la base de datos
conn <- DBI::dbConnect(RSQLite::SQLite(), "airbnb.sqlite")

#Tablas en la base de datos
DBI::dbListTables(conn)
```

    ## [1] "Hoods"    "Listings" "Reviews"

## 1\. Extracción (listings).

``` r
#Query de SQL para hacer la extracción
first_query <- "SELECT L.price, L.number_of_reviews, L.room_type, 
L.review_scores_rating, H.neighbourhood_group AS district
FROM Listings AS L
INNER JOIN Hoods AS H
ON L.neighbourhood_cleansed=H.neighbourhood"
#DataFrame con la información extraída
listings <- collect(tbl(conn, sql(first_query)))
glimpse(listings)
```

    ## Rows: 19,612
    ## Columns: 5
    ## $ price                <chr> "$60.00", "$31.00", "$50.00", "$92.00", "$26.00",~
    ## $ number_of_reviews    <dbl> 79, 33, NA, 12, 149, 3, 170, 4, 8, NA, 118, 48, 5~
    ## $ room_type            <chr> "Private room", "Private room", "Entire home/apt"~
    ## $ review_scores_rating <dbl> 4.89, 4.58, NA, 4.92, 4.68, 4.00, 4.64, 5.00, 4.3~
    ## $ district             <chr> "Chamartín", "Latina", "Arganzuela", "Centro", "A~

## 2\. Extracción (reviews).

``` r
#Query de SQL para hacer la extracción
second_query <- "SELECT COUNT(Reviews.id) AS reviews_number,
Hoods.neighbourhood_group AS district, strftime('%Y-%m', Reviews.date) AS mes
FROM Reviews
INNER JOIN Listings
ON Listings.id=Reviews.listing_id
INNER JOIN Hoods
ON Listings.neighbourhood_cleansed=Hoods.neighbourhood
WHERE strftime('%Y', Reviews.date) NOT LIKE '2010'
GROUP BY district, mes"
#DataFrame con la información extraída
reviews <- collect(tbl(conn, sql(second_query)))
glimpse(reviews)
```

    ## Rows: 2,170
    ## Columns: 3
    ## $ reviews_number <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 4, 3, 7, 5~
    ## $ district       <chr> "Arganzuela", "Arganzuela", "Arganzuela", "Arganzuela",~
    ## $ mes            <chr> "2011-01", "2011-03", "2011-04", "2011-05", "2011-06", ~

# Transform

## 3\. Transformación (listings).

``` r
#Transformación del precio a númerico eliminando
#el $ y la coma
listings <- listings %>% 
  mutate(price = as.numeric(str_remove_all(price, "[,//$]")))
typeof(listings$price)
```

    ## [1] "double"

## 4\. Transformación (listings).

**Opción B**

``` r
#Para Number of Reviews

#Iteramos por las filas del df
for (i in 1:nrow(listings)) {
  # Condición para quedarnos con aquellas que contienen NA
  if (is.na(listings$number_of_reviews[[i]])) {
    # room_type para cada elemento NA
    rt <- listings$room_type[[i]]
    # Imputar valor aleatorio correspondiente a cada NA 
    #evitando que sea a su vez NA
    listings$number_of_reviews[[i]] <- 
      sample(na.omit(listings$number_of_reviews[listings$room_type==rt]),size=1)
    }
}
#Comprobamos que no quedan NAs
sum(is.na(listings$number_of_reviews))
```

    ## [1] 0

``` r
#Review_scores_rating
for (i in 1:nrow(listings)) {
  if (is.na(listings$review_scores_rating[[i]])) {
    rt <- listings$room_type[[i]]
    listings$review_scores_rating[[i]] <- 
      sample(na.omit(listings$review_scores_rating[listings$room_type==rt]),size=1)
  }
}
sum(is.na(listings$review_scores_rating))
```

    ## [1] 0

## 5\. Transformación (listings).

``` r
#Tabla con nota media ponderada y precio mediano
#agrupada por distrito  tipo de alojamiento
tabla1 <- listings %>% 
  group_by(district,room_type) %>% 
  summarise(wh_avg_review = weighted.mean(review_scores_rating,number_of_reviews),
            median_price = median(price))
```

    ## `summarise()` has grouped output by 'district'. You can override using the `.groups` argument.

``` r
tabla1
```

    ## # A tibble: 75 x 4
    ## # Groups:   district [21]
    ##    district    room_type       wh_avg_review median_price
    ##    <chr>       <chr>                   <dbl>        <dbl>
    ##  1 Arganzuela  Entire home/apt          4.69           70
    ##  2 Arganzuela  Hotel room               4.45           19
    ##  3 Arganzuela  Private room             4.65           31
    ##  4 Arganzuela  Shared room              4.66           44
    ##  5 Barajas     Entire home/apt          4.73           86
    ##  6 Barajas     Private room             4.76           35
    ##  7 Barajas     Shared room              4.90           37
    ##  8 Carabanchel Entire home/apt          4.54           70
    ##  9 Carabanchel Private room             4.70           26
    ## 10 Carabanchel Shared room              4.63           21
    ## # ... with 65 more rows

## 6\. Transformación (reviews).

``` r
#Predicción para agosto con valores de julio
reviews_prediction <- reviews %>% 
  select(everything()) %>% 
  filter(mes == '2021-07') %>% 
  mutate(mes = '2021-08')

#Combinamos predicciones con la tabla orginal
reviews <- reviews %>% 
  bind_rows(reviews_prediction) %>% 
  arrange(district,mes)
tail(reviews)
```

    ## # A tibble: 6 x 3
    ##   reviews_number district   mes    
    ##            <int> <chr>      <chr>  
    ## 1             18 Villaverde 2021-03
    ## 2             15 Villaverde 2021-04
    ## 3             18 Villaverde 2021-05
    ## 4             30 Villaverde 2021-06
    ## 5              3 Villaverde 2021-07
    ## 6              3 Villaverde 2021-08

## 7\. Transformación (reviews).

``` r
#df con todas las posibles combinaciones de fechas con distritos
df <- tibble(expand.grid(district = unique(reviews$district), 
                 mes = substring(seq(as.Date("2011-01-01"),
                                     as.Date("2021-08-01"),
                                     by="months"), 1, 7)))

#join de ambos df para añadir combinaciones que faltaban
reviews_wpred <- reviews %>% 
  full_join(df) %>% 
  arrange(district,mes) %>% 
  #convertimos los NAs que acabamos de introducir en 0
  mutate(reviews_number = ifelse(is.na(reviews_number), 0, reviews_number))
```

    ## Joining, by = c("district", "mes")

``` r
reviews_wpred
```

    ## # A tibble: 2,688 x 3
    ##    reviews_number district   mes    
    ##             <dbl> <chr>      <chr>  
    ##  1              1 Arganzuela 2011-01
    ##  2              0 Arganzuela 2011-02
    ##  3              1 Arganzuela 2011-03
    ##  4              1 Arganzuela 2011-04
    ##  5              1 Arganzuela 2011-05
    ##  6              1 Arganzuela 2011-06
    ##  7              1 Arganzuela 2011-07
    ##  8              1 Arganzuela 2011-08
    ##  9              1 Arganzuela 2011-09
    ## 10              1 Arganzuela 2011-10
    ## # ... with 2,678 more rows

# Load

## 8\. Carga

``` r
#Subimos tabla de Reviews con predicciones a la base de datos
RSQLite::dbWriteTable(conn, "Reviews_wpred", reviews_wpred)
collect(tbl(conn, sql("SELECT * FROM Reviews_wpred LIMIT 10")))
```

    ## # A tibble: 10 x 3
    ##    reviews_number district   mes    
    ##             <dbl> <chr>      <chr>  
    ##  1              1 Arganzuela 2011-01
    ##  2              0 Arganzuela 2011-02
    ##  3              1 Arganzuela 2011-03
    ##  4              1 Arganzuela 2011-04
    ##  5              1 Arganzuela 2011-05
    ##  6              1 Arganzuela 2011-06
    ##  7              1 Arganzuela 2011-07
    ##  8              1 Arganzuela 2011-08
    ##  9              1 Arganzuela 2011-09
    ## 10              1 Arganzuela 2011-10

``` r
#Subimos tabla de resumen de Listings a la base de datos
RSQLite::dbWriteTable(conn, "Listings_summary", tabla1)
collect(tbl(conn, sql( "SELECT * FROM Listings_summary LIMIT 10")))
```

    ## # A tibble: 10 x 4
    ##    district    room_type       wh_avg_review median_price
    ##    <chr>       <chr>                   <dbl>        <dbl>
    ##  1 Arganzuela  Entire home/apt          4.69           70
    ##  2 Arganzuela  Hotel room               4.45           19
    ##  3 Arganzuela  Private room             4.65           31
    ##  4 Arganzuela  Shared room              4.66           44
    ##  5 Barajas     Entire home/apt          4.73           86
    ##  6 Barajas     Private room             4.76           35
    ##  7 Barajas     Shared room              4.90           37
    ##  8 Carabanchel Entire home/apt          4.54           70
    ##  9 Carabanchel Private room             4.70           26
    ## 10 Carabanchel Shared room              4.63           21

``` r
#Desconexión de la base de datos
RSQLite::dbDisconnect(conn)
```
