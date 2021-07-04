rm(list = ls())

if(!require(pacman)){install.packages("pacman")}
require(pacman)
p_load(tidyverse, here, extrafont, scales, radiant.data)

# Archivero

files <- list(clean_data = here("output/clean-data.rds"))

# Estilo para gráficas

loadfonts(quiet = T)

mi_tema <- theme_minimal() +
  theme(plot.title = element_text(size = 22, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 18, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 12, family = "Barlow Condensed"),
        axis.title = element_text(size = 14, family = "Barlow Condensed"),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        legend.text = element_text(size = 14, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"))

# Abrimos base a graficar

clean_data <- readRDS(files$clean_data)

# Graficamos

tempo <- clean_data 

ggplot(tempo, aes(y = price_gbp)) +
  geom_boxplot(width = 5) +
  labs(title = "Distribución de precios en books.toscrape",
       y = "Precios (Libras Esterlinas)") +
  mi_tema +
  theme(axis.text.x = element_blank())

ggsave(here("output/price-box.png"), width = 8, height = 12)

tempo <- clean_data %>% 
  mutate(total = n()) %>% 
  group_by(stars, total) %>% 
  summarise(total_grupo = n()) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(total_grupo / total, digits = 2))

ggplot(tempo, aes(x = stars, y = porcentaje)) +
  geom_col(fill = "lightblue") +
  geom_text(aes(label = paste0(porcentaje * 100, "%")), vjust = -0.5, 
            family = "Barlow Condensed", size = 7) +
  labs(title = "Porcentaje de libros con 1 a 5 estrellas", x = "Estrellas obtenidas",
       y = "Porcentaje") +
  scale_y_continuous(labels = percent_format()) +
  mi_tema

ggsave(here("output/star-bars.png"), width = 8, height = 12)

tempo <- clean_data %>% 
  group_by(genre) %>% 
  summarise(promedio = round(mean(stars), digits = 2),
            total_genre = n(),
            sd = radiant.data::sdpop(stars)) %>% 
  ungroup() %>% 
  arrange(-promedio) %>% 
  filter(total_genre > 1)

ggplot(tempo %>%  slice(1:10), aes(x = reorder(genre, -promedio), y = promedio, ymin = promedio-sd,
                  ymax = promedio+sd)) +
  geom_errorbar(width = 0.15, color = "#4392F1", size = 1.1) +
  geom_point(size = 3.5, fill = "#4392F1", shape = 21) +
  geom_label(aes(label = paste("Promedio:", promedio)), family = "Roboto Slab", size = 5, alpha = 0.1, vjust = -1) +
  labs(title = "Promedio y Variación de Estrellas por Género de Libro", y = "Estrellas (1-5)", 
       x = "Género") +
  mi_tema 

ggsave(here("output/promedios-sd.jpg"), width = 12, height = 12)
