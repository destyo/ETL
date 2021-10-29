# Escalas diferentes
iris %>% 
  summarise(across(is.numeric, mean))

iris_scaled <- iris %>% 
  mutate(across(where(is.numeric), ~ (. - mean(.) / sd(.))))

iris_scaled %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd)))