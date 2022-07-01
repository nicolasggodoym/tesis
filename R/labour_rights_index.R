
# Labour Rights Index (Kucera & Sari) -------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               readxl,
               car,
               sjmisc,
               stringr)

# Cargar datos ------------------------------------------------------------
lri <- readxl::read_xlsx("input/data/Scores_2000-2017.xlsx",
                         sheet = 2,
                         range = "A3:J190",
                         na = "N/A")

# Procesamiento -----------------------------------------------------------
lri <- lri %>% 
  select(country = 1,
         iso3c = 3,
         yr_2000 = 4,
         yr_2005 = 5,
         yr_2009 = 6,
         yr_2012 = 7, 
         yr_2015 = 7, 
         yr_2016 = 8,
         yr_2017 = 9)

lri <- lri %>% 
  pivot_longer(cols = -c(1,2),
               names_to = "year",
               values_to = "lri") %>% 
  mutate(year = str_sub(.$year, start = 4, -1))

