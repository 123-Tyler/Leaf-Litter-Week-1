
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

# Install data ------------------------------------------------------------

insect_data <- read.csv('group1_leaf_litter_data_new.csv') %>%
  select(1:26) %>% 
  mutate(wood_name = as.factor(wood_name)) %>%
  mutate(transect_point = as.factor(transect_point)) %>%
  select(transect_point, isopods:dermaptera) %>%
  pivot_longer(cols = isopods:dermaptera,
               names_to = "species",
               values_to = "amount") %>%
  pivot_wider(names_from = transect_point,
              values_from = amount)

View(insect_data)

str(insect_data)

# the rows have been added in excel to remove error message (easier than R):
species_data <-
  read.csv('group1_leaf_litter_species_frequency_data.csv', header = TRUE)

View(species_data)

alpha_data <- 
  read.csv('group1_leaf_litter_alpha_diversity_data.csv', header = TRUE)

View(alpha_data)

# Alpha diversity ---------------------------------------------------------

# Grassland Vs Ecotone
cor.test(alpha_data$grassland, alpha_data$ecotone, method = "spearman")

# Woodland Vs Ecotone
cor.test(alpha_data$woodland, alpha_data$ecotone, method = "spearman")

# Woodland Vs Grassland
cor.test(alpha_data$woodland, alpha_data$grassland, method = "spearman")

# GLM ---------------------------------------------------------------------

combination <-
  c("Grassland + Ecotone",
    "Woodland + Ecotone",
    "Grassland + Woodland")
rho <- c(0.07971706, 0.764959, 0.2179379)

glm_matrix <- data.frame(combination, rho)

View(glm_matrix)

# Rho plot ----------------------------------------------------------------

glm_matrix %>%
  ggplot(aes(
    x = combination,
    y = rho,
    label = rho,
    fill = combination
  )) +
  geom_col() +
  geom_text(vjust = -0.5) +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey"),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  )

ggsave("alpha_diversity_hist.jpeg")

# Histogram ---------------------------------------------------------------


