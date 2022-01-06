
# Código de procesamiento -------------------------------------------------


# 1. Cargar librerías  ----------------------------------------------------

pacman::p_load(tidyverse,
              sjmisc,
              haven,
              dplyr,
              car)

# 2. Cargar datos ---------------------------------------------------------

issp <- read_sav("input/data/issp_2019.sav")

# 3. Explorar -------------------------------------------------------------

find_var(issp, "Conflict") 

#v36 Q12a Conflicts in [COUNTRY]: Between poor people and rich people?
#v37 Q12b Conflicts: Between the working class and the middle class?
#V38 Q12c Conflicts: Between management and workers?

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
# find_var(issp, "")
# find_var(issp, "")

frq(issp$v36)
frq(issp$v37)
frq(issp$v38)
frq(issp$EMPREL)
frq(issp$WRKSUP)
frq(issp$NSUP)
frq(issp$SEX)

#(data$)


# 4. Seleccionar y procesar variables -------------------------------------


# a) Selección y filtrar ------------------------------------------------------------

data <- issp %>% 
  select(confl_rp = v36, #Rich and poor
         confl_wm = v37, #Working class and middle class
         confl_mw = v38, #Managers and workers
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
  mutate_at(vars(WRKSUP, NSUP, EMPREL), ~(car::recode(.,
                                                      "c(-9, -4) = NA"))) %>% 
  mutate_at(vars(starts_with("confl")), ~(car::recode(.,
                                                recodes = c("1 = 'El conflicto es muy fuerte';
                                                            2 = 'El conflicto es fuerte';
                                                            3 = 'El conflicto no es muy fuerte';
                                                            4 = 'No hay conflicto'"),
                                                as.factor = T, 
                                                levels = c('No hay conflicto',
                                                           'El conflicto no es muy fuerte',
                                                           'El conflicto es fuerte',
                                                           'El conflicto es muy fuerte')))) %>% 
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
         ISCO08 = substr(.$ISCO08, start = 1, stop = 2),
         habilidades = car::recode(.$ISCO08, 
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
                           propiedad == 'No propietario' & WRKSUP == 1 & NSUP > 100 & habilidades == 'Experto' ~ 'Experto directivo',
                           propiedad == 'No propietario' & WRKSUP == 1 & NSUP > 100 & habilidades == 'Calificado' ~ 'Directivo semi-credencializado',
                           propiedad == 'No propietario' & WRKSUP == 1 & NSUP > 100 & habilidades == 'No calificado' ~ 'Directivo no credencializado',
                           propiedad == 'No propietario' & WRKSUP == 1 & NSUP <= 100 & habilidades == 'Experto' ~ 'Experto supervisor',
                           propiedad == 'No propietario' & WRKSUP == 1 & NSUP <= 100 & habilidades == 'Calificado' ~ 'Supervisor semi-credencializado',
                           propiedad == 'No propietario' & WRKSUP == 1 & NSUP <= 100 & habilidades == 'No calificado' ~ 'Supervisor no credencializado',
                           propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'Experto' ~ 'Experto no directivo',
                           propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'Calificado' ~ 'Obrero semi-credencializado',
                           propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'No calificado' ~ 'Proletario'), 
                        levels = c('Proletario',
                                   'Obrero semi-credencializado', 
                                   'Experto no directivo',
                                   'Supervisor no credencializado',
                                   'Supervisor semi-credencializado', 
                                   'Experto supervisor',
                                   'Directivo no credencializado',
                                   'Directivo semi-credencializado', 
                                   'Experto directivo',
                                   'Pequeña burguesia',
                                   'Pequeño empleador',
                                   'Capitalista'))) 


  
  

# 5. Etiquetado -----------------------------------------------------------


# 6. Exportar datos -------------------------------------------------------


