rm(list = ls())
pacman::p_load(tidyverse, lme4, sjmisc, 
               sjPlot, tidyverse, 
               lmerTest, kableExtra,  
               webshot, ggrepel)
data <- readRDS("output/data/data.rds")

# Modelos CI ----------------------------------------------------------

data$country = as_factor(data$country)

# Visualización -----------------------------------------------------------

lm_sim_ci <- lm(job_money ~ clase, data = data) 
lm_un_ci <- lm(job_money ~ clase + UNION, data = data)
lm_sx_ci <- lm(job_money ~ clase + SEX, data = data) 
lm_clase_ci <- lm(job_money ~ clase + UNION + SEX, data = data) 

# ml_tot_ci <- lmer(job_money ~ clase + 
#                     (plp + lri + densidad|country) +
#                     UNION + SEX, data)
# 
# ml_den_ci <- lmer(job_money ~ clase + UNION + SEX + (densidad|country), data)
# ml_plp_ci <- lmer(job_money ~ clase + UNION + SEX + (plp|country), data)
# 
# ml_lri_ci <- lmer(job_money ~ clase + UNION + SEX + (lri|country), data)

ml_tot_std <- lmer(job_money ~ clase + 
                    (plp_std+lri_std+den_std|country) +
                    UNION + SEX, data)

ml_plplri_std <- lmer(job_money ~ clase + 
                        (plp_std+lri_std|country) +
                        UNION + SEX, data)

ml_plpden_std <- lmer(job_money ~ clase + 
          (plp_std+den_std|country) +
          UNION + SEX, data)

ml_lriden_std <- lmer(job_money ~ clase + 
                        (lri_std+den_std|country) +
                        UNION + SEX, data)

ml_den_std <- lmer(job_money ~ clase + UNION + SEX + (den_std|country), data)
ml_plp_std <- lmer(job_money ~ clase + UNION + SEX + (plp_std|country), data)

ml_lri_std <- lmer(job_money ~ clase + UNION + SEX + (lri_std|country), data)


final_fix = lmer(job_money ~ clase + UNION + SEX +ipo+(1|country), data)
final_rand = lmer(job_money ~ clase + UNION + SEX +(ipo|country), data)
final_norand = lmer(job_money ~ clase + UNION + SEX +(1|country), data)

# Un nivel -----------------------------------------------

tab_model(list(lm_sim_ci, lm_un_ci, lm_sx_ci, lm_clase_ci),
          title = "Regresion lineal sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T, 
          dv.labels = c("Modelo 1",
                        "Modelo 2",
                        "Modelo 3",
                        "Modelo 4"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          encoding = "UTF-8",
          file = "output/fig/lm_ci.html")
webshot("output/fig/lm_ci.html", "output/fig/lm_ci.png")

# Multinivel --------------------------------------------------------------
# tab_model(list(ml_plp_ci, ml_lri_ci, ml_den_ci, ml_tot_ci),
#           title = "Regresion lineal multinivel sobre actitud mercantilizada hacia el trabajo",  
#           auto.label = T,
#           dv.labels = c("Modelo 5",
#                         "Modelo 6",
#                         "Modelo 7",
#                         "Modelo 8"),
#           collapse.se = T,
#           show.ci = F,
#           string.pred = "Predictores",
#           string.est = "Coef.",
#           string.p = "P-valor",
#           string.intercept = "Intercepto",
#           encoding = "UTF-8",
#           show.r2 = F,
#           file = "output/fig/ml_ci.html")
# 
# webshot("output/fig/ml_ci.html", "output/fig/ml_ci.png")

tab_model(list(ml_plp_std, ml_den_std, ml_lri_std, ml_tot_std),
          title = "Regresion lineal multinivel sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T,
          dv.labels = c("Modelo 5",
                        "Modelo 6",
                        "Modelo 7",
                        "Modelo 8"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          encoding = "UTF-8",
          show.dev = F,
          show.aic = F,
          show.r2 = F,
          file = "output/fig/ml_std.html")
webshot("output/fig/ml_std.html", "output/fig/ml_std.png")

tab_model(list(ml_tot_std, ml_plplri_std, ml_plpden_std, ml_lriden_std),
          title = "Regresion lineal multinivel sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T,
          dv.labels = c("Modelo 8",
                        "Modelo 9",
                        "Modelo 10",
                        "Modelo 11"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          encoding = "UTF-8",
          show.dev = F,
          show.aic = F,
          show.r2 = F,
          file = "output/fig/ml2_std.html")
webshot("output/fig/ml2_std.html", "output/fig/ml2_std.png")

tab_model(list(ml_tot_std, final_fix, final_rand, final_norand),
          title = "Regresion lineal multinivel sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T,
          dv.labels = c("Modelo 8",
                        "Modelo 12",
                        "Modelo 13",
                        "Modelo 14"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          encoding = "UTF-8",
          show.dev = F,
          show.aic = F,
          show.r2 = F,
          file = "output/fig/ml3_std.html")
webshot("output/fig/ml3_std.html", "output/fig/ml3_std.png")


# Ajuste ------------------------------------------------------------------

Modelos = c("Modelo 1",
            "Modelo 2",
            "Modelo 3",
            "Modelo 4",
            "Modelo 5", 
            "Modelo 6",
            "Modelo 7",
            "Modelo 8",
            "Modelo 9",
            "Modelo 10",
            "Modelo 11",
            "Modelo 12",
            "Modelo 13",
            "Modelo 14")

AIC = c(AIC(lm_sim_ci),
        AIC(lm_un_ci),
        AIC(lm_sx_ci),
        AIC(lm_clase_ci),
        AIC(ml_plp_std),
        AIC(ml_den_std),
        AIC(ml_lri_std),
        AIC(ml_tot_std),
        AIC(ml_plplri_std),
        AIC(ml_plpden_std),
        AIC(ml_lriden_std),
        AIC(final_fix),
        AIC(final_rand),
        AIC(final_norand))


BIC = c(BIC(lm_sim_ci),
        BIC(lm_un_ci),
        BIC(lm_sx_ci),
        BIC(lm_clase_ci),
        BIC(ml_plp_std),
        BIC(ml_den_std),
        BIC(ml_lri_std),
        BIC(ml_tot_std),
        BIC(ml_plplri_std),
        BIC(ml_plpden_std),
        BIC(ml_lriden_std),
        BIC(final_fix),
        BIC(final_rand),
        BIC(final_norand))

Devianza = c(deviance(lm_sim_ci),
             deviance(lm_un_ci),
             deviance(lm_sx_ci),
             deviance(lm_clase_ci),
             deviance(ml_plp_std),
             deviance(ml_den_std),
             deviance(ml_lri_std),
             deviance(ml_tot_std),
             deviance(ml_plplri_std),
             deviance(ml_plpden_std),
             deviance(ml_lriden_std),
             deviance(final_fix),
             deviance(final_rand),
             deviance(final_norand))

data.frame(Modelos, AIC, BIC, Devianza) %>% 
  kable(caption = "Comparación de ajuste de modelos",
        format = "html") %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :")
webshot("output/fig/ajuste_modelos.html", "output/fig/ajuste_modelos.png")



# Comparativa pendientes e interceptos ------------------------------------

x <- data.frame(coef(final_fix)$country)

pais = sort(unique(data$country))
ipo = data %>% 
  select(pais = country, ipo) %>% 
  distinct(pais, ipo)
x <- data.frame(pais, b0 = coef(final_fix)$country)
x = merge(x, ipo, by = "pais")
row.names(x) = NULL
x %>% 
  select(1, 2, ipo) %>% 
  mutate_at(vars(2, 3), ~(round(., 3))) %>% 
  rowwise() %>% 
  mutate(pred = round(b0..Intercept. + (ipo*-.53), 3)) %>% 
  ungroup() %>% 
  .[order(.$pred, decreasing = T),] %>%  
  kable(caption = "Interceptos aleatorios y valores predichos estimados en el 
Modelo 12",
        format = "html",
        col.names = c("País",
                      "Intercepto aleatorio",
                      "IPO", "Valor predicho")) %>% 
  kable_classic(full_width = F,
                html_font = "Times New Roman") %>% 
  footnote("Elaboración propia",
           general_title = "Fuente :")
webshot("output/fig/interceptos_mod12.html", "output/fig/interceptos_mod12.png")

x %>% 
  select(1, 2, ipo) %>% 
  mutate_at(vars(2, 3), ~(round(., 3))) %>% 
  rowwise() %>% 
  mutate(pred = round(b0..Intercept. + (ipo*-.53), 3)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ipo, y = pred)) +
  geom_point() + 
  geom_text_repel(aes(label=pais), max.overlaps = 25) +
  geom_smooth(method = "lm", colour = "black") + 
  labs(title = "Gráfico 2",
  subtitle="Relación entre el IPO y los valores predichos para un obrero hombre no sindicalizado",
       x ="IPO", y = "Valores predichos",
       caption = "Elaboración propia") +
  theme_minimal() 
ggsave("output/fig/cor_predipo.jpg", fig = last_plot(), width = 19, height = 14)

x %>% 
  select(1, 2, ipo) %>% 
  mutate_at(vars(2, 3), ~(round(., 3))) %>% 
  rowwise() %>% 
  mutate(pred = round(b0..Intercept. + (ipo*-.53), 3)) %>% 
  ungroup() %>% 
  ggplot(aes(x = ipo, y = pred)) +
  geom_point() + 
  geom_text_repel(aes(label=pais), max.overlaps = 25) +
  geom_smooth(method = "lm", colour = "black") + 
  labs(title = "Relación entre el IPO y los valores predichos para un obrero hombre no sindicalizado",
       subtitle="",
       x ="IPO", y = "Valores predichos",
       caption = "Elaboración propia") +
  theme_minimal() 

ggsave("output/fig/cor_predipo_notitulo.jpg", fig = last_plot())


# Valores predichos por clase para Chile ----------------------------------

p = data.frame(clases = factor(c("Proletario",
           "Obrero calificado",
           "Experto no directivo",
           "Directivo/supervisor no calificado",
           "Directivo/supervisor calificado",
           "Gerente",
           "Pequeña burguesia no profesional",
           "Pequeña burguesia profesional",
           "Capitalista"), levels = c("Proletario",
                                      "Obrero calificado",
                                      "Experto no directivo",
                                      "Directivo/supervisor no calificado",
                                      "Directivo/supervisor calificado",
                                      "Gerente",
                                      "Pequeña burguesia no profesional",
                                      "Pequeña burguesia profesional",
                                      "Capitalista")),
           pais = c("Chile", "Chile", "Chile", "Chile", "Chile", "Chile", "Chile", 
                    "Chile", "Chile",
                    "Finlandia", "Finlandia", "Finlandia" ,"Finlandia" ,"Finlandia",
                    "Finlandia", "Finlandia", "Finlandia", "Finlandia"),
           pred = c(round(1.95 + (-.71*-.53)),
                    round(1.95 + (-.71*-.53) -.22, 3),
                    round(1.95 + (-.71*-.53) -.61, 3),
                    round(1.95 + (-.71*-.53) -.22, 3),
                    round(1.95 + (-.71*-.53) -.48, 3),
                    round(1.95 + (-.71*-.53) -.69, 3),
                    round(1.95 + (-.71*-.53) -.34, 3),
                    round(1.95 + (-.71*-.53) -.78, 3),
                    round(1.95 + (-.71*-.53) -.76, 3),
                    round(2.6 + -.53),
                    round(2.6 + -.53 -.22, 3),
                    round(2.6 + -.53 -.61, 3),
                    round(2.6 + -.53 -.22, 3),
                    round(2.6 + -.53 -.48, 3),
                    round(2.6 + -.53 -.69, 3),
                    round(2.6 + -.53 -.34, 3),
                    round(2.6 + -.53 -.78, 3),
                    round(2.6 + -.53 -.76, 3)))
           
p = p %>%
  rowwise() %>% 
  mutate(inf = pred - ((var(.$pred)/sqrt(10))*1.96),
         sup = pred + ((var(.$pred)/sqrt(10))*1.96)) %>%
  ungroup()
  

p %>% 
  ggplot(aes(x = clases, y = pred), 
             label = pred) +
  geom_point() + 
  geom_errorbar(aes(ymin = inf, ymax = sup, width = 0.2)) +
  geom_text(aes(label = pred), hjust =1.3) +
  labs(title = "Gráfico 1",
       subtitle = "Valores predichos para Chile y Finlandia según clase social",
       y = "", x ="",
       caption = "Elaboración propia") +
  guides(color = "none") +
  scale_color_discrete(name = "Clases sociales") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() +
  facet_grid(cols = vars(pais))

save_plot("output/fig/predclase.jpg", fig = last_plot(), width = 35, height = 25)

p %>% 
  ggplot(aes(x = clases, y = pred), 
         label = pred) +
  geom_point() + 
  geom_errorbar(aes(ymin = inf, ymax = sup, width = 0.2)) +
  geom_text(aes(label = pred), hjust =1.3) +
  labs(title = "Valores predichos para Chile y Finlandia según clase social",
       subtitle = "",
       y = "", x ="",
       caption = "Elaboración propia") +
  guides(color = "none") +
  scale_color_discrete(name = "Clases sociales") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme_minimal() +
  facet_grid(cols = vars(pais))

save_plot("output/fig/predclase_notitulo.jpg", fig = last_plot(), width = 33, height = 15)
