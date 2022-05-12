
# Código de procesamiento ISSP 2015 -------------------------------------------------

rm(list = ls())
# 1. Cargar librerías  ----------------------------------------------------

pacman::p_load(tidyverse,
               sjmisc,
               haven,
               dplyr,
               car, 
               remotes)

#Para alfa ordinal
remotes::install_github("jogrue/jogRu", force = T)

# 2. Cargar datos ---------------------------------------------------------

issp <- read_dta("input/data/issp_2015.dta")

# 3. Explorar -------------------------------------------------------------

# Variables de interés

# Have index
find_var(issp, "Q12a") #v22 Q12a Apply to R's job: my job is secure
find_var(issp, "Q12b") #v23 Q12b Apply to R's job: my income is high
find_var(issp, "Q12c") #v24 Q12c Apply to R's job: opportunities for advancement are high
find_var(issp, "Q12d") #v25 Q12d Apply to R's job: my job is interesting
find_var(issp, "Q12e") #v26 Q12e Apply to R's job: can work independently
find_var(issp, "Q12f") #v27 Q12f Apply to R's job: can help other people
find_var(issp, "Q12g") #v28 Q12g Apply to R's job: job is useful to society

# Lack index
find_var(issp, "Q2a") #v3 Q2a Personally important: job security
find_var(issp, "Q2b") #v4 Q2b Personally important: high income
find_var(issp, "Q2c") #v5 Q2c Personally important: opportunities for advancement
find_var(issp, "Q2d") #v6 Q2d Personally important: an interesting job
find_var(issp, "Q2e") #v7 Q2e Personally important: work independently
find_var(issp, "Q2f") #v8 Q2f Personally important: help other people
find_var(issp, "Q2g") #v9 Q2g Personally important: a job useful to society

# Valoración no-mercantil del trabajo remunerado
find_var(issp, "Q1a") #v1 Q1a Job is a way of earning money - no more
find_var(issp, "Q1b") #v2 Q1b Enjoy a paid job even if I did not need money

# Índice de sobreexigencia laboral
find_var(issp, "Q13a") #v30 Q13a How often applies: Do hard physical work
find_var(issp, "Q13b") #v31 Q13b How often applies: find work stressful
find_var(issp, "Q14a") #v32 Q14a And how often applies: work at home during working hours
find_var(issp, "Q14b") #v33 Q14b And how often applies: involve working on weekends

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

## Nivel micro -------------------------------------------------------------

### a) Selección y filtrar ------------------------------------------------------------

data <- issp %>% 
  filter((ISCO08!=110 | ISCO08!=210 | ISCO08!=310) & v20 == 1) %>% #Eliminar FFAA
  select(id = CASEID,
         iso3c = c_alphan,
         year = DATEYR,
         exp = WEIGHT,
         SEX,
         AGE,
         job_money = v1,
         job_enjoy = v2,
         pi_security = v3, 
         pi_income = v4, 
         pi_advance = v5, 
         pi_interest = v6, 
         pi_indep = v7, 
         pi_helpful = v8, 
         pi_useful = v9,
         have_security = v22,
         have_income = v23,
         have_advance = v24,
         have_interest = v25,
         have_indep = v26,
         have_helpful = v27,
         have_useful = v28,
         often_phys = v30,
         often_stress = v31,
         often_home = v32,
         often_weekend = v33,
         EMPREL,
         ISCO08,
         WRKSUP,
         NSUP,
         UNION) %>%
  
### b) Procesamiento -------------------------------------------------------------------------

  mutate_if(is.labelled, as.numeric) %>% #Transformar en numeric 
  mutate_at(vars(WRKSUP, NSUP, EMPREL, ISCO08, starts_with("often")), ~(car::recode(.,
                                                      "0 = NA"))) %>% 
  mutate_at(vars(WRKSUP, EMPREL, starts_with("pi"), starts_with("have"), starts_with("job"), starts_with("often"), SEX), ~(car::recode(.,
                                                "c(8,9) = NA"))) %>% 
  mutate_at(vars(starts_with("pi"), starts_with("have"), starts_with("often"), job_enjoy), ~(car::recode(.,
                                          c("1 = 4;
                                            2 = 3;
                                            3 = 2;
                                            4 = 1;
                                            5 = 0")))) %>% 
  mutate(job_money = car::recode(.$job_money, 
                                 recodes = c("1 = 0;
                                             2 = 1;
                                             3 = 2;
                                             4 = 3;
                                             5 = 4")),
         SEX = car::recode(.$SEX,
                           recodes = c("1 = 'Hombre';
                                       2 = 'Mujer';
                                       -9 = NA")),
         UNION = car::recode(.$UNION,
                             c("c(1, 2) = 'Si';
                               3 = 'No';
                               c(0, 7, 9) = NA")),
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
         
### c) Construcción de la variable clase social (clase) ---------------------
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
 rowwise() %>%
 mutate(commitment_suma = sum(job_money, job_enjoy, na.rm = T),
       have_suma = sum(have_security, have_income, have_advance, have_interest, have_indep, have_helpful, have_useful, na.rm = T),
       lack_security = have_security - pi_security,
       lack_income = have_income - pi_income,
       lack_advance = have_advance - pi_advance,
       lack_interest = have_interest - pi_interest,
       lack_indep = have_indep - pi_indep,
       lack_helpful = have_helpful - pi_helpful,
       lack_useful = have_useful - pi_useful,
       lack_suma = sum(lack_security, lack_income, lack_advance, lack_interest, lack_indep, lack_helpful, lack_useful, na.rm = T),
       exigencia_suma = sum(often_phys, often_stress, often_home, often_weekend, na.rm = T)) %>%
 mutate(commitment_index = (commitment_suma/max(.$commitment_suma) * 100),
       have_index = (have_suma/max(.$have_suma) * 100),
       lack_index = (lack_suma/max(.$lack_suma) * 100),
       exigencia_index = (exigencia_suma/max(.$exigencia_suma) * 100)) %>%
 ungroup() %>%
 mutate_at(vars(ends_with("index")), ~(car::recode(., "0 = NA"))) %>% 
 select(-c(EMPREL, ISCO08, WRKSUP, NSUP, propiedad, habilidades, starts_with("pi"), 
           starts_with("job"), starts_with("have"), -have_index, starts_with("often"), ends_with("suma"),
           starts_with("lack"), -lack_index))

# Variables indices
## Employment commitment
### job_money
### job_enjoy

## Have index

### have_security
### have_income
### have_advance
### have_interest
### have_indep
### have_helpful
### have_useful

## Lack index

### pi_security
### pi_income
### pi_advance
### pi_interest
### pi_indep
### pi_helpful
### have_useful

## Sobreexigencia

### often_phys
### often_stress
### often_home
### often_weekend

jogRu::ordinal_alpha(data %>% select(starts_with("pi_")))

jogRu::ordinal_alpha(data %>% select(16:22))

jogRu::ordinal_alpha(data %>% select(starts_with("often_")))


## Macro nivel ---------------------------------------

### Cargar datos ------------------------------------------------------------

load(url("https://github.com/fabrica-datos-laborales/fdl-data/raw/main/output/data/fdl.RData"))  

### Seleccionar y filtrar ---------------------------------------------------

context <- fdl %>% filter(year == 2019) %>% select(iso3c, 'nstrikes_isic31_total_ilo-stat', 'cbc_ilo-stat')
rm(list_fdl)


### Recodificar y transformar -----------------------------------------------



# Unificar con merge() -----------------------------------------------------------------

m <- merge(data, context,
           by = "iso3c", all.x = T)


# 5. Etiquetado -----------------------------------------------------------


# 6. Exportar datos -------------------------------------------------------


