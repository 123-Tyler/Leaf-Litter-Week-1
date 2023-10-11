
# To do -------------------------------------------------------------------

# Calculate alpha diversity
# GLM for alpha diversity V Section
# Boxplot for diversity per section
# Histogram for orders found (stacked per section?)

# Install packages --------------------------------------------------------

#install.packages('tidyverse')
library(tidyverse)
#install.packages('patchwork')
library(patchwork)
#install.packages('vegan')
library(vegan)

?cor.test

# Install data ------------------------------------------------------------

insect_data <- read.csv('group1_leaf_litter_data.csv') %>% 
  mutate(wood_name = as.factor(wood_name)) 
            
View(insect_data)

str(insect_data)

alpha_data <-
  insect_data %>% 
  select(transect_point, cockroaches.woodlice:earwig) %>% 
  pivot_longer(cols = cockroaches.woodlice:earwig,
               names_to = "species",
               values_to = "amount") %>% 
  pivot_wider(names_from = transect_point,
              values_from = amount)

View(alpha_data)

# Alpha diversity ---------------------------------------------------------

simpson.unb(x = insect_data[3], "simpson")

# GLM ---------------------------------------------------------------------
cor.test()

# Boxplot -----------------------------------------------------------------


# Histogram ---------------------------------------------------------------



