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
  filter(iso2c %in% c(c("AT", "AU", "BE", "CH", "CL", "CZ", 
                        "DE", "DK", "EE",
                        "ES", "FI", "FR", "GB", "HU", "IS", 
                        "LT", "LV", "MX", "NO", "PL", "RU",
                        "SE", "SI", "SK", "US", "ZA"))) %>% 
  select(-numeric, - country)

#### Crear vectores para filtrado
v_iso2c = country_codes$iso2c
v_iso3c = country_codes$iso3c
v_country = country_codes$country


# ILO-STAT ----------------------------------------------------------------

tud <- tud %>% 
  filter(iso3c %in% c(v_iso3c))

plp <- plp %>% 
  filter(iso3c %in% c(v_iso3c))

# DPI ---------------------------------------------------------------------

#dpi <- dpi %>% 
#  filter(country %in% c(v_country) & (year > 2010 & year < 2018)) 

# LRI ---------------------------------------------------------------------

lri <- lri %>% 
  filter(iso3c %in% c(v_iso3c) & (year > 2012 & year < 2018))


# World Bank --------------------------------------------------------------

wb <- wb %>% 
  filter(iso3c %in% c(v_iso3c) & (year > 2012 & year < 2018))


# Imputar -----------------------------------------------------------------

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

ctry_lvl <- merge(ctry_lvl, imp_plp, by = c("iso3c", "year", "plp"), all = T)

# Imputar valores NA 2015 -------------------------------------------------

ctry_lvl = ctry_lvl %>% 
  mutate_at(vars(plp, densidad, lri, pib_pc), 
            ~(ifelse(is.na(.) & lead(iso3c) == iso3c, na.locf(., fromLast = T), 
                ifelse(is.na(.) & lag(iso3c) == iso3c,
                       na.locf(.),
                       .)))) %>% 
  filter(year == 2015) %>% 
  select(-year) %>% 
  mutate(lri = 10-lri,
         mean_d = mean(densidad, na.rm =T),
         sd_d = sd(densidad, na.rm =T),
         mean_lri = mean(lri, na.rm =T),
         sd_lri = sd(lri, na.rm =T),
         mean_plp = mean(plp, na.rm =T),
         sd_plp = sd(plp, na.rm =T),
         mean_pibpc = mean(pib_pc, na.rm =T),
         sd_pibpc = sd(pib_pc, na.rm =T)) %>% 
  mutate(lri_std = (lri-mean_lri)/sd_lri,
         plp_std = (plp-mean_plp)/sd_plp,
         den_std = (densidad-mean_d)/sd_d,
         pib_std = (pib_pc-mean_pibpc)/sd_pibpc) %>% 
  # mutate(lri = (lri)/max(lri)*5,
  #        plp = (plp)/max(plp)*5,
  #        densidad = (densidad)/max(densidad)*5) %>%
  # mutate(lri = ifelse(lri == 0, 5, 
  #                     ifelse(lri == 5, 0,
  #                            5-lri))) %>% 
  rowwise() %>% 
  mutate(ipo = sum(lri_std, plp_std, den_std)/3) %>% 
  ungroup() %>% 
  select(-c(starts_with("mean"), starts_with("sd"))) %>% 
  filter(!duplicated(iso3c))

#dpi <- merge(country_codes, dpi, by = "country") %>% select(-country)

#ctry_lvl_dpi = merge(ctry_lvl, dpi)

rm(country_codes, lri, plp, tud, v_country, v_iso2c, v_iso3c, imp_plp, imp_tud, imp, wb)
