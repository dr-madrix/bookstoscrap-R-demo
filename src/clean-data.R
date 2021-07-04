rm(list = ls())

if(!require(pacman)){install.packages("pacman")}
require(pacman)
p_load(tidyverse, here)

# Archivero

files <- list(scrap_data = here("output/scrap-data.rds"),
              clean_data = here("output/clean-data.rds"))

# Abrimos

scrap_data <- readRDS(files$scrap_data)

# Limpiamos
cleaned <- scrap_data %>% 
  select(-link) %>% 
  rename(price_gbp = price, stars = star) %>% 
  mutate(price_gbp = as.numeric(str_remove(price_gbp, "£")),
         stars = as.numeric(stars), 
         description = replace_na(description, "No description."),
         genre = case_when(
           genre %in% c("Default", "Add a comment") ~ "Undefined",
           T ~ genre))

saveRDS(cleaned, files$clean_data)
