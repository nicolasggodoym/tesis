
# Código de procesamiento ISSP 2015 -------------------------------------------------


# 1. Cargar librerías  ----------------------------------------------------

pacman::p_load(tidyverse,
               sjmisc,
               haven,
               dplyr,
               car)

# 2. Cargar datos ---------------------------------------------------------

issp <- read_dta("input/data/issp_2015.dta")

# 3. Explorar -------------------------------------------------------------

# Variables independientes de interés


# Realización en el trabajo
find_var(issp, "Q2c") #v5 Q2c Personally important: opportunities for advancement
find_var(issp, "Q2d") #v6 Q2d Personally important: an interesting job
find_var(issp, "Q2f") #v8 Q2f Personally important: help other people
find_var(issp, "Q2g") #v9 Q2g Personally important: a job useful to society
find_var(issp, "Q4") #v13 Q4 Remain in not satisfying job for benefit of family life
find_var(issp, "Q13b") #v31 Q13b How often applies: find work stressful
find_var(issp, "Q23") #v44 Q23 How satisfied are you in your (main) job
find_var(issp, "Q25b") #v49 Q25b I am proud of the type of work I do

# Fetichización del trabajo
find_var(issp, "Q1a") #v1 Q1a Job is a way of earning money - no more
find_var(issp, "Q2b") #v4 Q2b Personally important: high income
find_var(issp, "Q2c") #v5 Q2c Personally important: opportunities for advancement

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


frq(issp$v5)
frq(issp$v6)
frq(issp$v8)
frq(issp$v9)
frq(issp$v13)
frq(issp$v31)
frq(issp$v44)
frq(issp$v49)
frq(issp$v1)
frq(issp$v4)

frq(issp$EMPREL)
frq(issp$WRKSUP)
frq(issp$NSUP)
frq(issp$SEX)



# 4. Seleccionar y procesar variables -------------------------------------


# a) Selección y filtrar ------------------------------------------------------------

data <- issp %>% 
  select(pi_opp_adv = v5,
         pi_inter = v6, 
         pi_help = v8,
         pi_useful = v9,
         work_stress = v31,
         satisf_job = v44,
         proud_job = v49,
         earn_job = v1,
         pi_high_inc = v4,
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
  mutate_at(vars(WRKSUP, EMPREL), ~(car::recode(.,
                                                "c(8,9) = NA"))) %>% 
  mutate(SEX = car::recode(.$SEX,
                           recodes = c("1 = 'Hombre';
                                       2 = 'Mujer';
                                       -9 = NA")),
         UNION = car::recode(.$UNION,
                             c("c(1, 2) = 'Si';
                               3 = 'No';
                               c(-9, -7, -4) = NA")),
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
                                   'Capitalista'))) #%>% 
  select(-c(EMPREL, ISCO08, WRKSUP, NSUP, propiedad, habilidades))





# 5. Etiquetado -----------------------------------------------------------


# 6. Exportar datos -------------------------------------------------------


