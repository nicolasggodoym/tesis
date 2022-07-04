
# Procesamiento general ---------------------------------------------------

rm(list = ls())
# Variables micro ---------------------------------------------------------
source("R/proc_2015.R")

# Variables macro ---------------------------------------------------------
source("R/proc_ctry_lvl.R")

# Unificar datos ----------------------------------------------------------
final <- merge(data, ctry_lvl, by = "iso3c", all.x = T)

final <- filter(final, !is.na(id) & iso3c != "PHL")

#final_dpi <- merge(data, ctry_lvl_dpi, by = "iso2c") %>% 
#  select(-c(country.x, country.y, year.x, year.y))

rm(ctry_lvl, 
   #ctry_lvl_dpi, 
   data)


# Pegar country code en español -------------------------------------------

esp <- readRDS("output/data/country-codes_esp.rds")

final <- merge(final, esp, by = "iso3c", all.x = T)

# Etiquetado de variables -------------------------------------------------


# Exportar ----------------------------------------------------------------

saveRDS(final, "output/data/data.rds")
#saveRDS(final_dpi, "output/data/data_dpi.rds")
#save(final, file = "output/data/data.RData")
