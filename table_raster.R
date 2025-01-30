# Turn a table into a raster with neat values, labels, and colors.

# Get packages.

library(tidyverse)
library(magrittr)
library(ggpubr)

# Set up levels for factors.

vec_rows = 
  c("Reform Fisheries Management",
    "Facilitate Fisheries Access",
    "Modernize Seafood Industry",
    "Advance Inclusive Science",
    "Upgrade and Diversify Infrastructure",
    "Diversify Industries, Workforce",
    "Enable Community Planning",
    "Shift Sociocultural Paradigms",
    "Reform Food Systems",
    "Decarbonize",
    "Develop Nature-Based Solutions")

vec_cols_1 = 
  c("Local",
    "State",
    "Regional",
    "National")

vec_cols_2 = 
  c("National",
    "Values",
    "Policy",
    "Localization",
    "Investment",
    "Innovation",
    "Information",
    "Coordination and Collaboration")

vec_cols = c(vec_cols_1, vec_cols_2)

# Set up palettes.

# Get the result.

"data.csv" %>% 
  read_csv %>% 
  pivot_longer(cols = !Row,
               names_to = "Col",
               values_to = "Value") %>% 
  mutate(Which = ifelse(Col %in% vec_cols_1, "Scale", "Flavor")) %>%
  # Add code for totals, cell colors, and text shades here.
  ggplot() +
  geom_raster(aes(x = Col,
                  y = Row,
                  fill = Value)) +
  coord_fixed() +
  scale_x_discrete(expand = c(0, 0),
                   position = "top") +
  scale_y_discrete(expand = c(0, 0)) +
  theme_pubr() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")


