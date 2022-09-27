
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

final <- na.omit(final)

rm(esp)

# Etiquetado de variables -------------------------------------------------
final$SEX = set_label(final$SEX, "Sexo")
final$AGE = set_label(final$AGE, "Edad")
final$sector = set_label(final$sector, "Sector economico")
final$clase = set_label(final$clase, "Posicion de clase")
final$pm_index = set_label(final$pm_index, "Indice de actitud solidaria hacia el trabajo")
final$job_money = set_label(final$job_money, "Actitud mercantilizada hacia el trabajo")
final$UNION = set_label(final$UNION, "Afiliación sindical")
#final$apoyo_nacional = set_label(final$apoyo_nacional, "Legitimidad nacional de actores sindicales")
final$densidad = set_label(final$densidad, "Densidad sindical")
final$lri = set_label(final$lri, "Indice de derechos laborales")
final$plp = set_label(final$plp, "Poder potencial de los trabajadores")
final$den_std = set_label(final$den_std, "Densidad sindical")
final$lri_std = set_label(final$lri_std, "Indice de derechos laborales")
final$plp_std = set_label(final$plp_std, "Poder potencial de los trabajadores")
final$ipo = set_label(final$ipo, "Índice de poder obrero")
final$pib_pc = set_label(final$pib_pc, "PIB per capita")
final$country = set_label(final$country, "Pais")
final$have_index = set_label(final$have_index, "Indice de condiciones laborales")
final$job_money = set_label(final$job_money, "Actitud mercantilizada hacia el trabajo")
final$pm_suma = set_label(final$pm_suma, "Indice de actitud solidaria hacia el trabajo")


# Exportar ----------------------------------------------------------------

saveRDS(final, "output/data/data.rds")
#saveRDS(final_dpi, "output/data/data_dpi.rds")
#save(final, file = "output/data/data.RData")
