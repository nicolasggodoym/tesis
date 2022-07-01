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

source("R/dpi.R")

### Poder asociativo e institucional: LRI -----------------------------------

source("R/labour_rights_index.R")

## Seleccionar y filtrar ---------------------------------------------------

### Filtrar países de interés -----------------------------------------------

country_codes <- country_codes %>% 
  filter(iso2c %in% c("AT", "AU", "BE", "CH", "CL", "CN", "CZ", "DE", "DK", "EE",
                      "ES", "FI", "FR", "GB", "GE", "HR", "HU", "IL", "IN",
                      "IS", "JP", "LT", "LV", "MX", "NO", "NZ", "PH", "PL", "RU",
                      "SE", "SI", "SK", "SR", "TW", "US", "VE", "ZA")) %>% 
  select(-numeric)

#### Crear vectores para filtrado
v_iso2c = country_codes$iso2c
v_iso3c = country_codes$iso3c
v_country = country_codes$country


# ILO-STAT ----------------------------------------------------------------

tud <- tud %>% 
  filter(iso3c %in% c(v_iso3c) & year == 2015)

plp <- plp %>% 
  filter(iso3c %in% c(v_iso3c) & year == 2015)

# DPI ---------------------------------------------------------------------

dpi <- dpi %>% 
  filter(country %in% c(v_country) & year == 2015) 

# LRI ---------------------------------------------------------------------

lri <- lri %>% 
  filter(iso3c %in% c(v_iso3c) & year == 2015)


# Unificar  ---------------------------------------------------------------

ctry_lvl <- list(country_codes, lri, plp, tud) %>% 
  Reduce(function(x,y) merge(x,y, by = "iso3c"), .)

dpi <- merge(country_codes, dpi, by = "country") %>% select(-country)

ctry_lvl_dpi = merge(ctry_lvl, dpi)

rm(country_codes, lri, plp, tud, dpi, v_country, v_iso2c, v_iso3c)
