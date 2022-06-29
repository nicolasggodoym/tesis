# Macro nivel ---------------------------------------
rm(list = ls())

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

### FDL ---------------------------------------------------------------------

load(url("https://github.com/fabrica-datos-laborales/fdl-data/raw/main/output/data/fdl.RData"))  
rm(list_fdl)


### EPL Index ---------------------------------------------------------------

epl <- read.csv("input/data/epl_index.csv")

## Explorar datos ----------------------------------------------------------

find_var(fdl, "density") #ud_ilo-stat


## Seleccionar y filtrar ---------------------------------------------------


### Filtrar países de interés -----------------------------------------------

country_codes <- country_codes %>% 
  filter(iso2c %in% c("AT", "AU", "BE", "CH", "CL", "CN", "CZ", "DE", "DK", "EE",
                      "ES", "FI", "FR", "GB", "GE", "HR", "HU", "IL", "IN",
                      "IS", "JP", "LT", "LV", "MX", "NO", "NZ", "PH", "PL", "RU",
                      "SE", "SI", "SK", "SR", "TW", "US", "VE", "ZA"))

#### Crear vectores para filtrado
v_iso2c = country_codes$iso2c
v_iso3c = country_codes$iso3c

fdl <- fdl %>% 
  filter(iso3c %in% v_iso3c & year > 2010) %>% 
  select(iso3c, year,
         tud = `ud_ilo-stat`)

epl <- epl %>% 
  select(iso3c = 1,
         year = 5,
         epl_index = 13) %>% 
  filter(iso3c %in% v_iso3c & year == 2015)


## Recodificar y transformar -----------------------------------------------



