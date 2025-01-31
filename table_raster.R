# Turn a table into a raster with neat values, labels, and colors.

# Get packages.

library(tidyverse)
library(magrittr)
library(ggpubr)
library(patchwork)

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
  c("Values",
    "Policy",
    "Localization",
    "Investment",
    "Innovation",
    "Information",
    "Coordination and \nCollaboration")

vec_cols = c(vec_cols_1, vec_cols_2)

# Set up colors.

offblue = "#3b5d91"
offorange = "#bc4700"

# Get the result.

vis_left = 
"data.csv" %>% 
  read_csv %>% 
  pivot_longer(cols = !Row,
               names_to = "Col",
               values_to = "Value") %>%
  mutate(Fill = ifelse(Row == "Total", NA, Value),
         Shade = ifelse(!is.na(Fill) & Value > 5, "Light", "Dark"),
         Col = ifelse(Col %>% str_sub(1, 3) == "Coo", "Coordination and \nCollaboration", Col),
         Col = Col %>% factor %>% fct_relevel(vec_cols),
         Row = Row %>% factor %>% fct_relevel(vec_rows) %>% fct_rev,
         Which = ifelse(Col %in% vec_cols_1, "Scale", "Flavor")) %>%
  filter(Col %in% vec_cols_1) %>% # Pick columns to keep in this plot.
  ggplot() +
  geom_tile(aes(x = Col,
                y = Row,
                fill = Fill),
            color = "gray25") +
  geom_text(aes(x = Col,
                y = Row,
                label = Value,
                color = Shade)) +
  coord_fixed() +
  scale_x_discrete(expand = c(0, 0),
                   position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = offblue, na.value = NA) +
  scale_color_manual(values = c("black", "white")) +
  theme_pubr() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

vis_right = 
  "data.csv" %>% 
  read_csv %>% 
  pivot_longer(cols = !Row,
               names_to = "Col",
               values_to = "Value") %>%
  mutate(Fill = ifelse(Row == "Total", NA, Value),
         Shade = ifelse(!is.na(Fill) & Value > 5, "Light", "Dark"),
         Col = ifelse(Col %>% str_sub(1, 3) == "Coo", "Coordination and \nCollaboration", Col),
         Col = Col %>% factor %>% fct_relevel(vec_cols),
         Row = Row %>% factor %>% fct_relevel(vec_rows) %>% fct_rev,
         Which = ifelse(Col %in% vec_cols_1, "Scale", "Flavor")) %>%
  filter(Col %in% vec_cols_2) %>% # Pick columns to keep in this plot.
  ggplot() +
  geom_tile(aes(x = Col,
                y = Row,
                fill = Fill),
            color = "gray25") +
  geom_text(aes(x = Col,
                y = Row,
                label = Value,
                color = Shade)) +
  coord_fixed() +
  scale_x_discrete(expand = c(0, 0),
                   position = "top",
                   guide = guide_axis(angle = 90)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = "white", high = offorange, na.value = NA) +
  scale_color_manual(values = c("black", "white")) +
  theme_pubr() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

vis = vis_left + vis_right

ggsave("vis.png",
       vis,
       dpi = 300,
       width = 6.5,
       height = 5)
