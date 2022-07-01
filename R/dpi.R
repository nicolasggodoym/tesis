
# Database of Political Institutions (DPI) --------------------------------


# Cargar librerías --------------------------------------------------------
pacman::p_load(tidyverse,
               haven,
               car,
               sjmisc)

# Cargar datos ------------------------------------------------------------
dpi <- read_dta("input/data/DPI2020.dta")

# Procesamiento -----------------------------------------------------------
dpi <- dpi %>% 
  select(country = countryname,
         year,
         ex_pos = execrlc,
         mayoria = allhouse,
         gov_votes = numvote,
         gov_seats = numgov,
         totalseats) %>% 
  mutate_at(vars(ex_pos, mayoria), ~(car::recode(.,
                                                 "-999 = NA"))) %>% 
  mutate(ex_pos = as.numeric(.$ex_pos),
         gov_seats = car::recode(.$gov_seats, "NaN = NA"),
         totalseats = car::recode(.$totalseats, "NaN = NA")) %>% 
  mutate(ex_pos = car::recode(.$ex_pos,
                              c("0 = NA;
                                1 = 'Derecha';
                                2 = 'Centro';
                                3 = 'Izquierda'"), as.factor = T),
         izq_seats = ifelse(ex_pos == 'Izquierda',
                            (gov_seats/totalseats * 100),
                            100 - (gov_seats/totalseats * 100)),
         izq_mayoria = ifelse(izq_seats < 50, "< 50%",
                              ifelse(izq_seats >= 50 & izq_seats <= 66.7, "50% - 66.7%",
                                     ifelse(izq_seats > 66.7, "> 66.7%", 
                                            ifelse(totalseats == 0, "No hay parlamento",
                                                   NA))))) %>% 
  select(country, year, ex_pos, gov_votes, izq_mayoria)
