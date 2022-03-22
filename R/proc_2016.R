
# Código de procesamiento ISSP 2016 -------------------------------------------------

rm(list = ls())
# 1. Cargar librerías  ----------------------------------------------------

pacman::p_load(tidyverse,
               sjmisc,
               haven,
               dplyr,
               car)

# 2. Cargar datos ---------------------------------------------------------

issp <- read_dta("input/data/issp_2016.dta")

# 3. Explorar -------------------------------------------------------------

# Variables independientes de interés


# Variables respuesta
find_var(issp, "Q5a") #v7 Q5a Government and economy: cuts in governments spending
find_var(issp, "Q5c") #v9 Q5c Government and economy: less government regulation of business
find_var(issp, "Q7g") #v27 Q7g Government responsibility: reduce income differences rich/ poor
find_var(issp, "Q16a") #v47 Q16a People like me have no say about what government does
find_var(issp, "Q17a") #v51 Q17a Taxes for high incomes
find_var(issp, "Q17b") #v52 Q17b Taxes for middle incomes
find_var(issp, "Q17c") #v53 Q17c Taxes for low incomes

# Variables predictoras (clase social y control)
find_var(issp, "EMP") #EMPREL Employment relationship
find_var(issp, "ISCO") #ISCO08
find_var(issp, "Supervise") #WRKSUP Supervise other employees
find_var(issp, "Number") #NSUP Number of other employees supervised
find_var(issp, "SEX") #SEX Sex of Respondent
find_var(issp, "Country") #c_alphan Country Prefix ISO 3166 Code - alphanumeric
find_var(issp, "ID") #CASEID ID Number of Respondent
find_var(issp, "WEIGHT") #WEIGHT Weighting factor
find_var(issp, "DATE") #DATEYR Year of interview: YYYY (four digits)
find_var(issp, "UNION") #UNION Trade union membership


frq(issp$v7) #Q5a Government and economy: cuts in governments spending
frq(issp$v9) #Q5c Government and economy: less government regulation of business
frq(issp$v27) #Q7g Government responsibility: reduce income differences rich/ poor
frq(issp$v47) #Q16a People like me have no say about what government does
frq(issp$v51) #Q17a Taxes for high incomes
frq(issp$v52) #Q17b Taxes for middle incomes
frq(issp$v53) #Q17c Taxes for low incomes

frq(issp$EMPREL)
frq(issp$WRKSUP)
frq(issp$NSUP)
frq(issp$SEX)

# 4. Seleccionar y procesar variables -------------------------------------


# a) Selección y filtrar ------------------------------------------------------------

data <- issp %>% 
  select(gov_cuts = v7,
         gov_less_reg = v9, 
         gov_red_diff = v27,
         not_say_gov = v47,
         tax_high = v51,
         tax_mid = v52,
         tax_low = v53,
         EMPREL,
         ISCO08,
         WRKSUP,
         NSUP,
         SEX,
         UNION,
         iso3c = c_alphan,
         id = CASEID,
         year = DATEYR,
         exp = WEIGHT) %>%
  filter(ISCO08!=110,ISCO08!=210, ISCO08!=310) %>% #Eliminar FFAA
  
  # b) Procesamiento
  
  mutate_if(is.labelled, as.numeric) %>% #Transformar en numeric 
  mutate_at(vars(WRKSUP, NSUP, EMPREL, ISCO08), ~(car::recode(.,
                                                              "0 = NA"))) %>% 
  mutate_at(vars(WRKSUP, EMPREL, starts_with("gov"), starts_with("tax"), not_say_gov), ~(car::recode(.,
                                                                             "c(8,9) = NA"))) %>% 
  mutate(SEX = car::recode(.$SEX,
                           recodes = c("1 = 'Hombre';
                                       2 = 'Mujer';
                                       -9 = NA")),
         UNION = car::recode(.$UNION,
                             c("c(1, 2) = 'Si';
                               3 = 'No';
                               c(7, 8, 9) = NA")),
         propiedad = car::recode(.$EMPREL,
                                 recodes = c("1 = 'No propietario';
                                             2 = 'Pequeña burguesia';
                                             3 = 'Pequeño empleador';
                                             4 = 'Capitalista';
                                             5 = 'Propietario de negocio familiar'")),
         ISCO08 = substr(.$ISCO08, start = 1, stop = 2)) %>% 
  mutate(habilidades = car::recode(.$ISCO08, 
                                   recodes = "10:26= 'Experto';
                                   c(30, 31, 32, 33, 34, 35, 51, 60, 61, 72)='Calificado';
                                   c(40, 41, 42, 43, 44, 50, 52, 53, 54, 62, 63, 70, 71, 73, 
                                   74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 
                                   89, 90, 91, 92, 93, 94, 95, 96)= 'No calificado';
                                   99=NA"),
         
         # c) Construcción de la variable clase social (clase) ---------------------
         clase = factor(case_when(propiedad == 'Capitalista' ~ 'Capitalista',
                                  propiedad == 'Pequeño empleador' ~ 'Pequeño empleador',
                                  propiedad == 'Pequeña burguesia' ~ 'Pequeña burguesia',
                                  propiedad == 'No propietario' & WRKSUP == 1 & habilidades == 'Experto' ~ 'Experto directivo/supervisor',
                                  propiedad == 'No propietario' & WRKSUP == 1 & habilidades == 'Calificado' ~ 'Directivo/supervisor semi-credencializado',
                                  propiedad == 'No propietario' & WRKSUP == 1 & habilidades == 'No calificado' ~ 'Directivo/supervisor no credencializado',
                                  propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'Experto' ~ 'Experto no directivo',
                                  propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'Calificado' ~ 'Obrero semi-credencializado',
                                  propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'No calificado' ~ 'Proletario',
                                  TRUE ~ NA_character_), 
                        levels = c('Proletario',
                                   'Obrero semi-credencializado', 
                                   'Experto no directivo',
                                   'Directivo/supervisor no credencializado',
                                   'Directivo/supervisor semi-credencializado', 
                                   'Experto directivo/supervisor',
                                   'Pequeña burguesia',
                                   'Pequeño empleador',
                                   'Capitalista'))) %>%
  select(-c(EMPREL, ISCO08, WRKSUP, NSUP, propiedad, habilidades))

# 5. Etiquetado -----------------------------------------------------------


# 6. Exportar datos -------------------------------------------------------


