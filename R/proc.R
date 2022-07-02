
# Procesamiento general ---------------------------------------------------


# Variables micro ---------------------------------------------------------
source("R/proc_2015.R")

# Variables macro ---------------------------------------------------------
source("R/proc_ctry_lvl.R")

# Unificar datos ----------------------------------------------------------
final <- merge(data, ctry_lvl, by = "iso2c") %>% 
  select(-c(country.x, country.y, year.x, year.y))
final_dpi <- merge(data, ctry_lvl_dpi, by = "iso2c") %>% 
  select(-c(country.x, country.y, year.x, year.y))

rm(ctry_lvl, ctry_lvl_dpi, data)

# Etiquetado de variables -------------------------------------------------


# Exportar ----------------------------------------------------------------

saveRDS(final, "output/data/data.rds")
saveRDS(final_dpi, "output/data/data_dpi.rds")
save(final, final_dpi, file = "output/data/data.RData")
