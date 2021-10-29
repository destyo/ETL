
library(ggplot2)
ggplot(iris) + 
  geom_boxplot(aes(x = Species, y = Sepal.Length, group = Species))


# Outliers ----------------------------------------------------------------
library(dplyr)
iris %>% 
  filter(!(Species == "virginica" & Sepal.Length < 5)) %>% 
  nrow()


boxplot_object <- boxplot(iris$Sepal.Length[iris$Species == "virginica"])
boxplot_object$stats

iris %>% 
  filter(!(Species == "virginica" & Sepal.Length < boxplot_object$stats[1, 1])) %>% 
  nrow()


# Extreme -----------------------------------------------------------------

df_flights <- nycflights13::flights
boxplot(df_flights$arr_delay)

number_rows_to_be_removed <- floor(0.05 * nrow(df_flights))
rows_to_be_kept <- number_rows_to_be_removed:(nrow(df_flights) - number_rows_to_be_removed)

df_flights_reduced <- df_flights %>%
  arrange(arr_delay) %>% 
  slice(rows_to_be_kept)

nrow(df_flights_reduced)

boxplot(df_flights_reduced$arr_delay)