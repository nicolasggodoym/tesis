
# Análisis descriptivo ----------------------------------------------------

rm(list = ls())
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               ggplot2, 
               sjmisc,
               sjPlot,
               kableExtra)

# Cargar datos ------------------------------------------------------------

data <- readRDS("output/data/data.rds")


# Análisis ----------------------------------------------------------------

# Presentar países --------------------------------------------------------
data %>% 
  frq(country,
      show.na = F,
      out = "viewer",
      encoding = "UTF-8") %>%  
  as.data.frame() %>% #Transformamos en dataframe para manipular con dplyr
  select(val, frq, raw.prc) %>% #Seleccionamos columnas de interés
  mutate(raw.prc = paste(.$raw.prc, "%")) %>% #Incorporamos porcentaje con paste()
  kable(caption = "Distribución según país de residencia",
        format = "html",
        col.names = c("País", "Frecuencia absoluta", "Frecuencia relativa")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :")


# Distribución dependiente ------------------------------------------------


# Distribución clase social ------------------------------------------------------
data %>% 
  frq(clase,
      show.na = F,
      out = "viewer",
      encoding = "UTF-8") %>%  
  as.data.frame() %>% #Transformamos en dataframe para manipular con dplyr
  select(val, frq, raw.prc) %>% #Seleccionamos columnas de interés
  mutate(raw.prc = paste(.$raw.prc, "%")) %>% #Incorporamos porcentaje con paste()
  kable(caption = "Distribución de la variable clase social",
        format = "html",
        col.names = c("País", "Frecuencia absoluta", "Frecuencia relativa")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :")

# Distribución variables macro --------------------------------------------
data %>% 
  group_by(country) %>%
  summarise(plp = round(mean(plp),3),
            tud = round(mean(densidad), 3),
            lri = round(mean(lri), 3),
            apoyo_nacional = round(mean(apoyo_nacional), 3)) %>% 
  ungroup() %>% #Transformamos en dataframe para manipular con dplyr
  kable(caption = "Distribución de variables macro según país",
        format = "html",
        col.names = c("País", "P.P.T.", 
                      "Densidad Sindical",
                      "Índice de derechos Laborales", 
                      "Legitimidad de actores sindicales")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :")

# Dependiente x clase social ----------------------------------------------
data %>% 
  group_by(clase) %>%
  summarise(pm = round(mean(pm_index, na.rm = T),3)) %>% 
  ungroup() %>% #Transformamos en dataframe para manipular con dplyr
  filter(!is.na(clase)) %>% 
  kable(caption = "Media de Índice de actitud solidaria hacia el trabajo según clase social",
        format = "html",
        col.names = c("Clase social", "Media de Índice de actitud solidaria hacia el trabajo")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :")

# Distribución variables control ------------------------------------------


