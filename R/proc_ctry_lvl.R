# Macro nivel ---------------------------------------

## Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse,
               sjmisc,
               stringr,
               haven,
               dplyr,
               car)

## Cargar datos ------------------------------------------------------------

### Country codes -----------------------------------------------------------

country_codes <- readRDS("output/data/country-codes.rds")

# Poder económico y asociativo --------------------------------------------

source("R/ilostat.R")

### Poder institucional: DPI ------------------------------------------------

#source("R/dpi.R")

### Poder asociativo e institucional: LRI -----------------------------------

source("R/labour_rights_index.R")


### Adjusted net national income per capita ---------------------------------

source("R/world-bank.R")

## Seleccionar y filtrar ---------------------------------------------------

### Filtrar países de interés -----------------------------------------------

country_codes <- country_codes %>% 
  filter(iso2c %in% c("AT", "AU", "BE", "CH", "CL", "CN", "CZ", "DE", "DK", "EE",
                      "ES", "FI", "FR", "GB", "GE", "HR", "HU", "IL", "IN",
                      "IS", "JP", "LT", "LV", "MX", "NO", "NZ", "PH", "PL", "RU",
                      "SE", "SI", "SK", "SR", "TW", "US", "VE", "ZA")) %>% 
  select(-numeric, - country)

#### Crear vectores para filtrado
v_iso2c = country_codes$iso2c
v_iso3c = country_codes$iso3c
v_country = country_codes$country


# ILO-STAT ----------------------------------------------------------------

tud <- tud %>% 
  filter(iso3c %in% c(v_iso3c) & (year > 2010 & year < 2018))

plp <- plp %>% 
  filter(iso3c %in% c(v_iso3c) & (year > 2010 & year < 2018))

# DPI ---------------------------------------------------------------------

#dpi <- dpi %>% 
#  filter(country %in% c(v_country) & (year > 2010 & year < 2018)) 

# LRI ---------------------------------------------------------------------

lri <- lri %>% 
  filter(iso3c %in% c(v_iso3c) & (year > 2010 & year < 2018))


# World Bank --------------------------------------------------------------

wb <- wb %>% 
  filter(iso3c %in% c(v_iso3c) & (year > 2010 & year < 2018))


imp <- data.frame(iso3c = v_iso3c, year = 2015, plp = NA, densidad = NA, lri = NA)

imp_tud = imp %>% filter(iso3c %in% c("IND", "ISR", "POL", "TWN", "VEN")) %>% 
  select(iso3c, year, densidad)

imp_plp = imp %>% filter(iso3c %in% c("IND", "PHL", "SUR", "POL", "VEN")) %>% 
  select(iso3c, year, plp)

# Unificar  ---------------------------------------------------------------

# Pegar country_codes a cada df

#tud = merge(country_codes, tud, by = "iso3c")
#lri = merge(country_codes, lri, by = "iso3c")
#plp = merge(country_codes, plp, by = "iso3c")

# Pegar valores nulos y ordenar df

ctry_lvl <- list(lri, plp, tud, wb) %>% 
  Reduce(function(x,y) merge(x,y, by = c("iso3c", "year"), all = T), .)

ctry_lvl <- merge(ctry_lvl, imp_tud, by = c("iso3c", "year", "densidad"), all = T)

#ctry_lvl <- merge(ctry_lvl, imp_plp, by = c("iso3c", "year", "plp"), all = T)


# Imputar valores NA 2015 -------------------------------------------------

ctry_lvl = ctry_lvl %>% 
  mutate_at(vars(plp, densidad, lri, nni_pc), 
            ~(ifelse(is.na(.) & lag(iso3c) == iso3c, na.locf(.), 
                ifelse(is.na(.) & lead(iso3c) == iso3c,
                       na.locf(., fromLast = T),
                       .)))) %>% 
  filter(year == 2015 & iso3c != "PHL") %>% 
  select(-year)

#dpi <- merge(country_codes, dpi, by = "country") %>% select(-country)

#ctry_lvl_dpi = merge(ctry_lvl, dpi)

rm(country_codes, lri, plp, tud, v_country, v_iso2c, v_iso3c, imp_plp, imp_tud, imp, wb)
