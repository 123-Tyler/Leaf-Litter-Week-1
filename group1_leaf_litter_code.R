
# To do -------------------------------------------------------------------

# Calculate alpha diversity
# GLM for alpha diversity V Section
# Boxplot for diversity per section
# Histogram for orders found (stacked per section?)

# Install packages --------------------------------------------------------

#install.packages('tidyverse')
library(tidyverse)
#install.packages('vegan')
library(vegan)
#install.packages('patchwork')
library(patchwork)

# Install data ------------------------------------------------------------
whole_data <- 
  read.csv('group1_leaf_litter_data_new.csv')

View(whole_data)

insect_data <- 
  read.csv('group1_leaf_litter_data_new.csv') %>%
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

cor.test(alpha_data$woodland, method = "shannon") # look at this


# Woodland Vs Grassland
cor.test(alpha_data$woodland, alpha_data$grassland, method = "spearman")

# GLM ---------------------------------------------------------------------

combination <-
  c("Grassland + Ecotone",
    "Woodland + Ecotone",
    "Grassland + Woodland")
rho <- c(0.07971706, 0.764959, 0.2179379)
p <- c("p = 0.75", "p < 0.01", "p = 0.39")

alpha_matrix <- data.frame(combination, rho, p)

View(alpha_matrix)

# Rho plot ----------------------------------------------------------------

alpha_matrix %>%
  ggplot(aes(
    x = combination,
    y = rho,
    label = p,
    fill = combination
  )) +
  geom_col() +
  ggtitle("Alpha Diversity Using Spearman's Rank") +
  ylab("Alpha Diversity (rho)") +
  geom_text(vjust = -0.5, 
            size = 5) +
  scale_fill_manual(values = c("blue2", "yellow2", "grey")) +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey"),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15,
                              face = "bold"),
    plot.title = element_text(size = 17,
                              face = "bold")
  )

ggsave("alpha_diversity_hist.jpeg",
       width = 16,
       height = 9,
       units = "in")

# Invertebrate abundance ---------------------------------------------------------------

p1 <- species_data %>%
  pivot_longer(cols = Grassland:Woodland,
               names_to = "Habitat",
               values_to = "Frequency") %>%
  ggplot(aes(
    x = reorder(str_to_title(species), -Frequency),
    y = Frequency,
    fill = Habitat
  )) +
  geom_col() +
  coord_flip() +
  ylab("Total Frequency") +
  xlab("Order") +
  ggtitle("Invertabrate Abundance Across the Transects") +
  scale_fill_manual(values = c("darkorange2", "#00AEDA", "#00B159")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey"),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 15,
                              face = "bold"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15, 
                                face = "bold"),
    plot.title = element_text(size = 17,
                              face = "bold")
  )

# Total Species -----------------------------------------------------------

Habitat <- c("Ecotone", "Grassland", "Woodland")
Count <- c(12, 10, 14)
total_inverts <- c(75, 60, 112)

total_species <- data.frame(Habitat, Count, total_inverts)

View(total_species)

p2 <- total_species %>% 
  ggplot(aes(
    x = Habitat,
    y = Count,
  )) +
  geom_col(fill = c("darkorange2", "#00AEDA", "#00B159")) +
  ylab("Total Orders Sampled") +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 13),
    axis.title.y = element_text(size = 15,
                              face = "bold"),
    axis.title.x = element_blank(),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15, 
                                face = "bold"),
    plot.title = element_text(size = 17,
                              face = "bold")
  )

# Combining plots ---------------------------------------------------------

p1 + inset_element(p2, 0.6, 0.6, 1, 1)

ggsave("insect_abundance_plot.jpeg",
       width = 16,
       height = 9,
       units = "in")

t.test(total_species$total_inverts)
