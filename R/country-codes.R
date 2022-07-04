
# Country codes iso2c iso3c -----------------------------------------------


# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               rvest,
               dplyr)

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


# Español -----------------------------------------------------------------

url <- read_html("https://es.wikipedia.org/wiki/ISO_3166-1_alfa-3")
iso3c <- url %>% html_nodes("tt") %>% html_text()  
iso3c <- iso3c[1:249]
country <- url %>% html_nodes("li a") %>% html_text()
country1 <- country[15:243]
country2 <- country[245:264]
country <- c(country1, country2)
table <- data.frame(iso3c = iso3c, country = country)



# Exportar ----------------------------------------------------------------

saveRDS(web, "output/data/country-codes.rds")
saveRDS(table, "output/data/country-codes_esp.rds")
