# This is a script is meant to create a map of the Dominican Republic that plots
# The google Mobility data by province
rm(list = ls())
pacman::p_load(tidyverse, 
               readxl, 
               lubridate)

# Explore and Clean Data ------------------------------------------------------------------
global <- read_csv("Global_Mobility_Report.csv")

dr <-  global %>%
  # I want to focus only on Dominican Republic for now
  filter(country_region_code == "DO") %>% 
  # There are several variables that, while valuable, are redundant
  select(-sub_region_2, -country_region_code, -country_region) %>% 
  # I wanna make this shit tidy
  gather(-sub_region_1:-date, 
         key = type_zone, value = percent_change) %>%
  # I believe that the NAs in sub_region_1 are meant to represent the country
  # as a whole. Must check this later to make sure this is true
  mutate(sub_region_1 = ifelse(is.na(sub_region_1), 
                               "Country", sub_region_1)) %>% 
  mutate(type_zone = str_remove(type_zone, "_percent_change_from_baseline"))

dr$type_zone <- as_factor(dr$type_zone)

levels(dr$type_zone) <- c("Comercio minorista/Recreación",
                          "Supermercados/Farmacias",
                          "Parques",
                          "Estaciones de Tránsito",
                          "Laborales",
                          "Residenciales"
                          )



# Create a time series plot of country wide mobilization-----------------------
subtitle <- "Relativo a la mediana de un día semanal equivalente entre Enero 3 y Febrero 6"

dr %>% 
  filter(sub_region_1 == "Country") %>% 
  ggplot(aes(x = date, y = percent_change, colour = type_zone)) +
  geom_line(show.legend = F) +
  facet_wrap(~ type_zone) +
  theme_bw() +
  labs(title = "Mobilización Física Dominicana en 2020",
       subtitle = subtitle,
       x = NULL,
       y = "Cambio porcentual",
       caption = "Por: Pablo Guzmán Lizardo,
       Usando datos del Google COVID-19 Community Mobility Report"
       ) +
  scale_y_continuous(breaks = seq(-100, 60, 20))


ggsave("mobilizacion_RD.jpg", scale = 1.2)