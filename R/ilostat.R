
# ILOSTAT -----------------------------------------------------------------

# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse, zoo)

# Densidad sindical -------------------------------------------------------

## Cargar datos ------------------------------------------------------------
tud <- read.csv("input/data/ILR_TUMT_NOC_RT_A.csv")

## Procesar datos ----------------------------------------------------------
tud <- tud %>% 
  select(iso3c = 1,
         year = time, 
         densidad = 5)

# Fuerza de trabajo según sexo y educación (miles) -------------------------------------------------------

## Cargar datos ------------------------------------------------------------
ft_s_ed <- read.csv("input/data/EAP_TEAP_SEX_EDU_NB_A.csv")

## Procesar datos ----------------------------------------------------------
ft_s_ed <- ft_s_ed %>% 
  filter(sex == "SEX_T") %>% 
  select(iso3c = 1,
         year = time,
         ed = classif1,
         ft_tot = 7) %>% 
  pivot_wider(id_cols = c(1,2),
              names_from = ed,
              values_from = ft_tot) %>% 
  select(1:8) %>% 
  filter(year %in% c(2012:2018))

ft_s_ed <- ft_s_ed[
  with(ft_s_ed, order(iso3c, year)),
]

ft_s_ed = ft_s_ed %>%
  mutate_at(vars(EDU_AGGREGATE_TOTAL, EDU_AGGREGATE_LTB, EDU_AGGREGATE_BAS,
                 EDU_AGGREGATE_INT, EDU_AGGREGATE_ADV, EDU_AGGREGATE_X),
            ~(ifelse(is.na(.) & lead(iso3c) == iso3c, na.locf(., fromLast = T),
                     ifelse(is.na(.) & lag(iso3c) == iso3c,
                              na.locf(.),
                            .))))

# Tasa de desocupación según sexo, edad y educación (%) | Anual -------------------------------------------------------

## Cargar datos ------------------------------------------------------------
unemp <- read.csv("input/data/UNE_DEAP_SEX_AGE_EDU_RT_A.csv")

## Procesar datos ----------------------------------------------------------
unemp <- unemp %>% 
  filter(sex == "SEX_T",
         classif1 == "AGE_AGGREGATE_TOTAL") %>% 
  select(iso3c = 2,
         year = time,
         ed = classif2,
         unemp_tot = 9) %>% 
  pivot_wider(id_cols = c(1,2),
              names_from = ed,
              values_from = unemp_tot) %>% 
  select(1:8) %>% 
  filter(year %in% c(2012:2018))

unemp <- unemp[
  with(unemp, order(iso3c, year)),
]

unemp <- unemp %>% 
  #Transformar en tasa de ocupación
  mutate_at(vars(EDU_AGGREGATE_LTB, EDU_AGGREGATE_BAS,
                 EDU_AGGREGATE_INT, EDU_AGGREGATE_ADV, EDU_AGGREGATE_X),
            ~((100 - .)/100)) %>% 
  mutate_at(vars(EDU_AGGREGATE_TOTAL, EDU_AGGREGATE_LTB, EDU_AGGREGATE_BAS,
                 EDU_AGGREGATE_INT, EDU_AGGREGATE_ADV, EDU_AGGREGATE_X),
            ~(ifelse(is.na(.) & lead(iso3c) == iso3c, na.locf(., fromLast = T),
                     ifelse(is.na(.) & lag(iso3c) == iso3c,
                            na.locf(.),
                            .))))


# Unificar y crear PLP (Rudra) --------------------------------------------

plp <- merge(ft_s_ed, unemp, by = c("iso3c", "year"))

rm(ft_s_ed, unemp)

plp = plp %>% 
  rowwise() %>% 
  mutate(ocupados_ltb = EDU_AGGREGATE_LTB.x * EDU_AGGREGATE_LTB.y,
         ocupados_bas = EDU_AGGREGATE_BAS.x * EDU_AGGREGATE_BAS.y,
         ocupados_int = EDU_AGGREGATE_INT.x * EDU_AGGREGATE_INT.y,
         ocupados_adv = EDU_AGGREGATE_ADV.x * EDU_AGGREGATE_ADV.y,
         ocupados_x = EDU_AGGREGATE_X.x * EDU_AGGREGATE_X.y,
         unemp_per = EDU_AGGREGATE_TOTAL.y) %>% 
  ungroup() %>% 
  select(1,2, starts_with("ocupados"), unemp_per) %>% 
  rowwise() %>% 
  mutate(plp_razon = ocupados_adv / (ocupados_ltb + ocupados_bas + 
                                       ocupados_int + ocupados_x),
         inv_unemp = 1/unemp_per) %>% 
  mutate(plp = plp_razon * inv_unemp) %>% 
  ungroup() %>% 
  select(1,2, plp)
  
