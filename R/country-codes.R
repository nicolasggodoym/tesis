
# Country codes iso2c iso3c -----------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               rvest)

# Cargar datos ------------------------------------------------------------

url <- "https://www.iban.com/country-codes"
web <- read_html(url) %>% 
  html_elements("table.table-bordered.downloads.tablesorter") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  rename(country = 1,
         iso2c = 2,
         iso3c = 3,
         numeric = 4)

# Exportar ----------------------------------------------------------------

saveRDS(web, "output/data/country-codes.rds")
