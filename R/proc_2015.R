
# Código de procesamiento ISSP 2015 -------------------------------------------------

# 1. Cargar librerías  ----------------------------------------------------

pacman::p_load(tidyverse,
               sjmisc,
               stringr,
               haven,
               dplyr,
               car)

#Para alfa ordinal
#remotes::install_github("jogrue/jogRu", force = T)

# 2. Cargar datos ---------------------------------------------------------

issp <- read_dta("input/data/issp_2015.dta")

country_codes <- readRDS("output/data/country-codes.rds")

country_codes <- country_codes %>% 
  filter(iso2c %in% c("AT", "AU", "BE", "CH", "CL", "CZ", 
                      "DE", "DK", "EE",
                      "ES", "FI", "FR", "GB", "HU", "IS", 
                      "LT", "LV", "MX", "NO", "PL", "RU",
                      "SE", "SI", "SK", "US", "ZA")) %>% 
  select(-c(numeric, country))

# 3. Explorar -------------------------------------------------------------

# Variables de interés

# Have index
# find_var(issp, "Q12a") #v22 Q12a Apply to R's job: my job is secure
# find_var(issp, "Q12b") #v23 Q12b Apply to R's job: my income is high
# find_var(issp, "Q12c") #v24 Q12c Apply to R's job: opportunities for advancement are high
# find_var(issp, "Q12d") #v25 Q12d Apply to R's job: my job is interesting
 # find_var(issp, "Q12e") #v26 Q12e Apply to R's job: can work independently
# find_var(issp, "Q12f") #v27 Q12f Apply to R's job: can help other people
# find_var(issp, "Q12g") #v28 Q12g Apply to R's job: job is useful to society

# Valoración no-mercantil del trabajo remunerado
# find_var(issp, "Q1a") #v1 Q1a Job is a way of earning money - no more
# find_var(issp, "Q1b") #v2 Q1b Enjoy a paid job even if I did not need money

# Variables predictoras (clase social y control)
# find_var(issp, "EMP") #EMPREL Employment relationship
# find_var(issp, "ISCO") #ISCO08
# find_var(issp, "Supervise") #WRKSUP Supervise other employees
# find_var(issp, "Number") #NSUP Number of other employees supervised
# find_var(issp, "SEX") #SEX Sex of Respondent
# find_var(issp, "Country") #c_alphan Country Prefix ISO 3166 Code - alphanumeric
# find_var(issp, "ID") #CASEID ID Number of Respondent
# find_var(issp, "WEIGHT") #WEIGHT Weighting factor
# find_var(issp, "DATE") #DATEYR Year of interview: YYYY (four digits)
# find_var(issp, "UNION") #UNION Trade union membership


# frq(issp$v5)
# frq(issp$v6)
# frq(issp$v8)
# frq(issp$v9)
# frq(issp$v13)
# frq(issp$v31)
# frq(issp$v44)
# frq(issp$v49)
# frq(issp$v1)
# frq(issp$v4)

# frq(issp$EMPREL)
# frq(issp$WRKSUP)
# frq(issp$NSUP)
# frq(issp$SEX)

# 4. Seleccionar y procesar variables -------------------------------------

## Nivel micro -------------------------------------------------------------

### a) Selección y filtrar ------------------------------------------------------------

data <- issp %>% 
  filter((ISCO08!=110 | ISCO08!=210 | ISCO08!=310) & v20 == 1) %>% #Eliminar FFAA
  select(id = CASEID,
         country,
         iso2c = c_alphan,
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
         pi_decide = v10,
         pi_contact = v11,
         union_need = v17,
         union_bad = v18,
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
         satisfied = v44,
         proud = v49,
         EMPREL,
         NEMPLOY,
         ISCO08,
         WRKSUP,
         NSUP,
         UNION,
         sector = TYPORG2) %>%
  
### b) Procesamiento -------------------------------------------------------------------------

  mutate(country = as_factor(.$country)) %>% 
  mutate_if(is.labelled, as.numeric) %>% #Transformar en numeric 
  mutate_at(vars(WRKSUP, NSUP, EMPREL, ISCO08, starts_with("often")), ~(car::recode(.,
                                                      "0 = NA"))) %>% 
  mutate_at(vars(WRKSUP, EMPREL, starts_with("pi"), starts_with("have"), 
                 starts_with("job"), starts_with("often"), SEX, proud, 
                 starts_with("union"), satisfied), ~(car::recode(.,
                                                "c(8,9) = NA"))) %>% 
  mutate_at(vars(starts_with("pi"), starts_with("have"), starts_with("often"), 
                 job_enjoy, proud, union_need), ~(car::recode(.,
                                          c("1 = 4;
                                            2 = 3;
                                            3 = 2;
                                            4 = 1;
                                            5 = 0")))) %>% 
  mutate(country = str_sub(.$country, start = 4, -1), 
         job_money = car::recode(.$job_money, 
                                 recodes = c("1 = 4;
                                             2 = 3;
                                             3 = 2;
                                             4 = 1;
                                             5 = 0")),
         union_need = factor(ifelse(union_need %in% c(0:2), "Desacuerdo",
                                    ifelse(union_need %in% c(3,4), "De acuerdo", NA)),
                             levels = c("Desacuerdo", "De acuerdo")),
         union_bad = car::recode(.$union_bad, 
                                 recodes = c("1 = 0;
                                             2 = 1;
                                             3 = 2;
                                             4 = 3;
                                             5 = 4")), 
         NEMPLOY = case_when(NEMPLOY < 3 ~ "1 o 2 empleados",
                             NEMPLOY > 2 & NEMPLOY < 50 ~ "3 a 49 empleados",
                             NEMPLOY > 49 & NEMPLOY < 9998 ~ "Más de 50 empleados",
                             TRUE ~ NA_character_), 
         sector = car::recode(.$sector,
                              recodes = c("1 = 'Publico';
                                          2 = 'Privado';
                                          c(0, 8, 9) = NA")),
         satisfied = car::recode(.$satisfied,
                                 recodes = c("1 = 7;
                                             2 = 6;
                                             3 = 5;
                                             4 = 4;
                                             5 = 3;
                                             6 = 2;
                                             7 = 1")),
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
                                             c(2,3,4) = 'Propietario'")),
         ISCO08 = substr(.$ISCO08, start = 1, stop = 2)) %>% 
         mutate(habilidades = car::recode(.$ISCO08, 
                                   recodes = "10:26= 'Experto';
                                   c(30, 31, 32, 33, 34, 35, 51, 60, 61, 72)='Calificado';
                                   c(40, 41, 42, 43, 44, 50, 52, 53, 54, 62, 63, 70, 71, 73, 
                                   74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 
                                   89, 90, 91, 92, 93, 94, 95, 96)= 'No calificado';
                                   99=NA")) %>% 
         
### c) Construcción de la variable clase social (clase) ---------------------
         mutate(clase = factor(case_when(propiedad == 'Propietario' & 
                                    NEMPLOY %in% c("3 a 49 empleados", "Más de 50 empleados") ~ 'Capitalista',
                                  propiedad == 'Propietario' & 
                                    NEMPLOY == "1 o 2 empleados" & 
                                    habilidades == 'Experto' ~ 'Pequeña burguesia profesional',
                                  propiedad == 'Propietario' & 
                                    NEMPLOY == "1 o 2 empleados" & 
                                    habilidades != 'Experto' ~ 'Pequeña burguesia no profesional',
                                  propiedad == 'No propietario' & WRKSUP == 1 & habilidades == 'Experto' ~ 'Gerente',
                                  propiedad == 'No propietario' & WRKSUP == 1 & habilidades == 'Calificado' ~ 'Directivo/supervisor calificado',
                                  propiedad == 'No propietario' & WRKSUP == 1 & habilidades == 'No calificado' ~ 'Directivo/supervisor no calificado',
                                  propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'Experto' ~ 'Experto no directivo',
                                  propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'Calificado' ~ 'Obrero calificado',
                                  propiedad == 'No propietario' & WRKSUP == 2 & habilidades == 'No calificado' ~ 'Proletario',
                                  TRUE ~ NA_character_), 
                        levels = c('Proletario',
                                   'Obrero calificado', 
                                   'Experto no directivo',
                                   'Directivo/supervisor no calificado',
                                   'Directivo/supervisor calificado', 
                                   'Gerente',
                                   'Pequeña burguesia no profesional',
                                   'Pequeña burguesia profesional',
                                   'Capitalista'))) 

data <- data %>%
 rowwise() %>%
 mutate(have_suma = sum(have_security, have_income, have_advance, have_interest, have_indep, have_helpful, have_useful, na.rm = T),
       pm_suma = sum(pi_useful, pi_helpful, na.rm = T)) %>%
  mutate(have_index = (have_suma/max(.$have_suma) * 5)) %>%
 ungroup() %>%
 select(country, iso2c, 
        job_money, pm_suma, 
        clase, have_index, 
        UNION, SEX)
 

# Estimar apoyo a nivel nacional ------------------------------------------

# apoyo = data %>% 
#   mutate(union_mean = mean(union_need, na.rm = T),
#          union_sd = sd(union_need, na.rm = T)) %>% 
#   group_by(iso2c) %>% 
#   mutate(mean_nacional = mean(union_need, na.rm = T)) %>% 
#   ungroup() %>% 
#   rowwise() %>% 
#   mutate(apoyo_nacional = (mean_nacional - union_mean)/union_sd) %>%
#   ungroup() %>% 
#   select(2, 12) %>% 
#   distinct()


# Pegar apoyo nacional a datos micro --------------------------------------

# data = merge(data, apoyo, 
#              by = "iso2c",
#              all = T)

data = merge(data, country_codes, by = "iso2c")

data = data %>% select(-c(iso2c, country))

rm(issp, #apoyo, 
   country_codes) 


# Alfa ordinal

# jogRu::ordinal_alpha(data %>% select(starts_with("pi_")))
# 
# jogRu::ordinal_alpha(data %>% select(pi_interest, pi_indep, pi_decide, pi_contact))
# 
# jogRu::ordinal_alpha(data %>% select(pi_helpful, pi_useful))

# Análisis factorial

# x <- data %>% select(starts_with("pi"), starts_with("job"))
# 
# library(RcmdrMisc)
# 
# rcorr.adjust(x, use="pairwise.complete.obs")
# 
# cor(x, use="pairwise.complete.obs")
# 
# library(psych)
# 
# KMO(x)
# 
# x <- x %>%  #select(-c(pi_income, job_money)) %>%
#   na.omit(.)
# 
# KMO(x)
# 
# cortest.bartlett(x)
# 
# ev <- eigen(cor(x, use="pairwise.complete.obs")) # get eigenvalues
# ev$values
# 
# scree(x, pc=FALSE)
# 
# fa.parallel(x, fa="fa")
# 
# fit <- factanal(x, 4, rotation="equamax")
# 
# print(fit, digits=2, cutoff=0.3, sort=TRUE)
# 
# loads <- fit$loadings
# 
# fa.diagram(loads)

# Resultado: sólo nos quedamos con un factor (pm)


