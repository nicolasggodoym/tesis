
# Procesamiento general ---------------------------------------------------

rm(list = ls())
pacman::p_load(sjlabelled)
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

rm(esp)

# Etiquetado de variables -------------------------------------------------
final$SEX = set_label(final$SEX, "Sexo")
final$AGE = set_label(final$AGE, "Edad")
final$sector = set_label(final$sector, "Sector económico")
final$clase = set_label(final$clase, "Posición de clase")
final$pm_index = set_label(final$pm_index, "Índice de actitud solidaria hacia el trabajo")
final$job_money = set_label(final$job_money, "Actitud mercantilizada hacia el trabajo")
final$apoyo_nacional = set_label(final$apoyo_nacional, "Legitimidad nacional de actores sindicales")
final$densidad = set_label(final$densidad, "Densidad sindical")
final$lri = set_label(final$lri, "Índice de derechos laborales")
final$plp = set_label(final$plp, "Poder potencial de los trabajadores")
final$nni_pc = set_label(final$nni_pc, "INN per capita")
final$country = set_label(final$country, "País")
final$have_index = set_label(final$have_index, "Índice de condiciones laborales")

# Exportar ----------------------------------------------------------------

saveRDS(final, "output/data/data.rds")
#saveRDS(final_dpi, "output/data/data_dpi.rds")
#save(final, file = "output/data/data.RData")
