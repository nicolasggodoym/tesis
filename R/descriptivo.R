
# Análisis descriptivo ----------------------------------------------------

rm(list = ls())
# Cargar paquetes ---------------------------------------------------------
pacman::p_load(tidyverse,
               ggplot2, 
               sjmisc,
               sjPlot,
               kableExtra,
               webshot)

# Cargar datos ------------------------------------------------------------

data <- readRDS("output/data/data.rds")

# Análisis ----------------------------------------------------------------

# Presentar países --------------------------------------------------------
data %>% 
  frq(country,
      show.na = F,
      out = "viewer",
      encoding = "LATIN1") %>%  
  as.data.frame() %>% #Transformamos en dataframe para manipular con dplyr
  select(val, frq, raw.prc) %>% #Seleccionamos columnas de interés
  mutate(raw.prc = paste(.$raw.prc, "%")) %>% #Incorporamos porcentaje con paste()
  kable(caption = "Distribución según país de residencia",
        format = "html",
        col.names = c("País", "Frecuencia absoluta", "Frecuencia relativa")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :") %>%
  cat(., file = "output/fig/clase_distr.html")
webshot("output/fig/country_distr.html", "output/fig/country_distr.png")


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
        col.names = c("Posición de clase", "Frecuencia absoluta", "Frecuencia relativa")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :") 
webshot("output/fig/clase_frq.html", "output/fig/clase_frq.png")


# Distribución afiliación sindical ----------------------------------------
data %>% 
  frq(UNION,
      show.na = F,
      out = "viewer",
      encoding = "UTF-8") %>%  
  as.data.frame() %>% #Transformamos en dataframe para manipular con dplyr
  select(val, frq, raw.prc) %>% #Seleccionamos columnas de interés
  mutate(raw.prc = paste(.$raw.prc, "%")) %>% #Incorporamos porcentaje con paste()
  kable(caption = "Distribución de la variable afiliación sindical",
        format = "html",
        col.names = c("Afiliación sindical", "Frecuencia absoluta", "Frecuencia relativa")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :") 
webshot("output/fig/union.html", "output/fig/union.png")



# Distribución variables macro --------------------------------------------
data %>% 
  group_by(country) %>%
  summarise(plp = round(mean(plp),3),
            lri = round(mean(lri), 3),
            densidad = round(mean(densidad), 3)) %>% 
  ungroup() %>% #Transformamos en dataframe para manipular con dplyr
  kable(caption = "Distribución de variables macro según país",
        format = "html",
        col.names = c("País", "P.P.T.", "Densidad sindical", 
                      "Índice de derechos Laborales")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :") 
webshot("output/fig/country_summary.html", "output/fig/country_summary.png")

#### ESTANDARIZADO
data %>% 
  group_by(country) %>%
  summarise(plp_std = round(mean(plp_std),3),
            den_std = round(mean(den_std), 3),
            lri_std = round(mean(lri_std), 3),
            ipo = round(mean(ipo), 3)) %>% 
  ungroup() %>% #Transformamos en dataframe para manipular con dplyr
  kable(caption = "Distribución de variables macro según país",
        format = "html",
        col.names = c("País", "P.P.T.", "Densidad sindical", 
                      "Índice de derechos Laborales",
                      "Índice de Poder Obrero")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :") 
webshot("output/fig/country_summary_std.html", "output/fig/country_summary_std.png")


# Correlación variables macro ---------------------------------------------

sjPlot::tab_corr(data %>% select(plp_std, den_std, lri_std),
                 title = "Correlación de Pearson para variables macro",
                 corr.method = "pearson",
                 triangle = "lower",
                 file = "output/fig/country_corr.html") 
webshot("output/fig/country_corr.html", "output/fig/country_corr.png")
# Dependiente x clase social ----------------------------------------------

#CI
data %>% 
  group_by(clase) %>%
  summarise(ci = round(mean(job_money, na.rm = T),3)) %>% 
  ungroup() %>% 
  kable(caption = "Media de Índice de actitud mercantilizada hacia el trabajo según clase social",
        format = "html",
        col.names = c("Posición de clase", 
                      "Media de actitud mercantilizada hacia el trabajo")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :") 
webshot("output/fig/clase_ci.html", "output/fig/clase_ci.png")
