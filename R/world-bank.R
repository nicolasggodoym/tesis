
# World Bank --------------------------------------------------------------


# Adjusted net national income per cápita ----------------------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, stringr)

# Cargar datos ------------------------------------------------------------
wb <- read.csv("input/data/world_bank.csv", na.strings = "..")

# Procesar ----------------------------------------------------------------

wb <- wb %>% 
  filter(time %in% c(2013:2018) & 
           classif1 == "AGE_AGGREGATE_TOTAL" &
           classif2 == "EDU_AGGREGATE_TOTAL") %>% 
  select(iso3c = 1, 5, 6, year = 7, 8) %>% 
  group_by(iso3c, year) %>% 
  summarise(obs_value = first(obs_value)) %>% 
  ungroup() %>% 
  rename(pib_pc = obs_value)


