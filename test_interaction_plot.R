# Sample problem: given a matrix of interaction values and a vector of importance values can you create a pixelated ggplot of pixels with two different legends

# Clear workspace and load in packages
rm(list = ls())
library(tidyverse)
library(MASS)
library(ggnewscale)
  
# Create some fake data
set.seed(100)
n_vars = 5
var_names = paste0('Var',1:n_vars)
var_imp = runif(n_vars)
var_int = abs(cor(mvrnorm(1000, 
                      mu = rep(0, n_vars), 
                      Sigma = rWishart(1, 20, diag(n_vars))[,,1])))
colnames(var_int) = rownames(var_int) = var_names
diag(var_int) = var_imp

# Now create a plot -------------------------------------------------------

var_int2 = var_int %>% as_tibble %>% 
  mutate(var_num1 = 1:n_vars) %>% 
  pivot_longer(cols = 1:n_vars,
               names_to = 'var', 
               values_to = 'Interaction\nStrength') %>% 
  mutate(var_num2 = rep(1:n_vars, n_vars),
         alpha_imp = as.integer(var_num1 == var_num2),
         alpha_int = 1 - alpha_imp,
         `Variable\nImportance` = alpha_imp*`Interaction\nStrength`,
         `Interaction\nStrength` = alpha_int*`Interaction\nStrength`)

ggplot(data = var_int2, 
       mapping = aes(x = var_num1, y = var_num2)) + 
  scale_x_continuous(breaks = 1:n_vars, labels = var_names) + 
  scale_y_continuous(breaks = 1:n_vars, labels = var_names) +
  geom_raster(aes(fill = `Interaction\nStrength`),
              alpha = var_int2$alpha_int) + 
  new_scale_fill() +
  geom_raster(aes(fill = `Variable\nImportance`),
              alpha = var_int2$alpha_imp) + 
  scale_fill_gradient(low = "red",high = "white") +
  xlab('') + 
  ylab('') + 
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
