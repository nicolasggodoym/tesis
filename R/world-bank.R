
# World Bank --------------------------------------------------------------


# Adjusted net national income per cápita ----------------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, stringr)

# Cargar datos ------------------------------------------------------------
wb <- read.csv("input/data/world_bank.csv", na.strings = "..")

# Procesar ----------------------------------------------------------------

wb <- wb %>% 
  select(4, 7:15) %>% 
  pivot_longer(cols = -1,
               names_to = "year",
               values_to = "nni_pc") %>% 
  mutate(year = as.numeric(str_sub(.$year, start = 10, end = 13))) %>% 
  rename(iso3c = 1)




