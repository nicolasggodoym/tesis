
# Database of Political Institutions (DPI) --------------------------------


# Cargar librerías --------------------------------------------------------
pacman::p_load(tidyverse,
               haven,
               car,
               sjmisc)

# Cargar datos ------------------------------------------------------------
dpi <- read_dta("input/data/DPI2020.dta")

# Procesamiento -----------------------------------------------------------
x <- dpi %>% 
  select(countryname,
         year,
         ex_pos = execrlc,
         mayoria = allhouse)

# Etiquetado --------------------------------------------------------------


# Exportar datos ----------------------------------------------------------


