library(dplyr)


# Simulación de datos -----------------------------------------------------
# Simulamos datos con distribuciones conocidas
size_pop <- 100
set.seed(12345)

df_simulated <- tibble(
  index = seq_len(size_pop),
  V1 = runif(size_pop),
  V2 = rnorm(size_pop),
  V3 = rnorm(size_pop, 100, 10),
  V4 = rpois(size_pop, lambda = 10)
)

library(purrr)
ups_downs <- runif(size_pop, -0.015, 0.015)
ups_downs[1] <- 20
df_simulated <- df_simulated %>% 
  mutate(V5 = accumulate(ups_downs, ~.x + .x * .y))

head(df_simulated)

# Inventamos datos missing
rows_with_na <- map(1:5, ~sample(1:size_pop, 15))
for(i in seq_along(rows_with_na)){
  df_simulated[[i + 1]][rows_with_na[[i]]] <- NA
}

# Check
map_lgl(df_simulated, ~any(is.na(.)))

check_na_per_column <- function(one_data_frame){
  map_lgl(one_data_frame, ~any(is.na(.)))
}


# Valor constante ---------------------------------------------------------
replace_with_mean <- function(x){
  # x is a column (or vector)
  if(is.integer(x)){
    if_else(is.na(x), as.integer(mean(x, na.rm = TRUE)), x)  # floor
  } else {
    if_else(is.na(x), mean(x, na.rm = TRUE), x)  
  }
} 

df_imputed <- df_simulated %>% 
  mutate(across(starts_with("V"), replace_with_mean))

head(df_imputed)

check_na_per_column(df_imputed)
# Muestra aleatoria -------------------------------------------------------

# - `V1`: uniform distribution between 0 and 1.
# - `V2`: normal distribution with mean 0 and variance 1.
# - `V3`: normal distribution with mean 100 and variance 10.
# - `V4`: Poisson distribution with $\lambda = 10$.
# - `V5`: something random.


# First I create a copy of the data frame for being able to keep using the 
# original one
df_imputed <- df_simulated

how_many_na <- sum(is.na(df_imputed$V1))

set.seed(789)
df_imputed$V1[is.na(df_imputed$V1)] <- runif(how_many_na)

check_na_per_column(df_imputed)

# Dato previo -------------------------------------------------------------
library(ggplot2)
ggplot(df_simulated) + 
  geom_line(aes(x = index, y = V5))

df_imputed <- df_simulated %>% 
  mutate(V5_replaced = coalesce(V5, mean(V5, na.rm = TRUE))) 

df_imputed %>% 
  ggplot() + 
  geom_line(aes(x = index, y = V5)) + 
  geom_line(aes(x = index, y = V5_replaced), color = "red", linetype = 2)

df_imputed <- df_simulated %>% 
  select(index, V5) %>% 
  mutate(V5_lag = lag(V5, n = 1), # 1 period
         V5_fixed = if_else(is.na(V5), V5_lag, V5))

df_imputed

df_imputed %>% 
  ggplot() + 
  geom_line(aes(x = index, y = V5)) + 
  geom_line(aes(x = index, y = V5_fixed), color = "red", linetype = 2)

# Media móvil -------------------------------------------------------------

library(purrr)

df_imputed <- df_simulated

df_imputed$V5 <- imap_dbl(df_simulated$V5, function(.x, .y){
  # .x represents the value in the iteration
  # .y represents the index of the interation
  
  if(!is.na(.x)){
    return(.x) 
  } else {
    prev_value <- df_simulated$V5[.y - 1] 
    next_value <- df_simulated$V5[.y + 1] 
    
    return(mean(c(prev_value, next_value)))  
  }
})

