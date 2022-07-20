rm(list = ls())
pacman::p_load(tidyverse, lme4, sjmisc, 
               sjPlot, tidyverse, 
               lmerTest, ordinal, MCMCglmm)
data <- readRDS("output/data/data.rds")

data$pm_suma = as_factor(data$pm_suma)
data$country = as_factor(data$country)

# data$pm_ordinal <- car::recode(data$pm_suma, 
#                                c("c(0,1) = 0;
#                                  c(2,3) = 1;
#                                  c(4,5) = 2;
#                                  c(6,7) = 3;
#                                  8 = 4"))
# 
# data$pm_ordinal <- set_label(data$pm_ordinal, "Índice de actitud solidaria hacia el trabajo")
# data <- data %>% 
#   group_by(country) %>% 
#   mutate(pm_dummy = ifelse(pm_index > mean(pm_index, na.rm = T), 1, 
#                     ifelse(pm_index <= mean(pm_index, na.rm = T), 0, NA))) %>% 
#   ungroup()
# data$pm_dummy <- set_label(data$pm_dummy, "Índice de actitud solidaria hacia el trabajo")


# Modelos PM ----------------------------------------------------------


# Regresión lineal múltiple -----------------------------------------------

# lm_sim_pm <- lm(pm_index ~ clase, data)
# 
# lm_sc_pm <- lm(pm_index ~ clase + have_index, data)
# 
# lm_clase_pm <- lm(pm_index ~ clase + have_index + SEX +sector, data)

# Regresión logística binaria -----------------------------------------------

# glm_sim_pm <- glm(pm_dummy ~ clase, family = binomial(link = "logit"), data)
# 
# glm_sc_pm <- glm(pm_dummy ~ clase + have_index, family = binomial(link = "logit"), data)
# 
# glm_clase_pm <- glm(pm_dummy ~ clase + have_index + SEX +sector, family = binomial(link = "logit"), data)

# Multinivel --------------------------------------------------------------

#Lineal
# ml_tot_pm <- lmer(pm_index ~ clase + 
#                  (plp + densidad + lri + apoyo_nacional|country) + 
#                  have_index + SEX, data)
# 
# ml_plp_pm <- lmer(pm_index ~ clase + have_index + plp + (1|country), data)
# 
# ml_densidad_pm <- lmer(pm_index ~ clase + have_index + densidad + (1|country), data)
# 
# ml_lri_pm <- lmer(pm_index ~ clase + have_index + lri + (1|country), data)
# 
# ml_apoyo_pm <- lmer(pm_index ~ clase + have_index + apoyo_nacional + (1|country), data)

#Binaria
# gml_tot_pm <- glmer(pm_dummy ~ clase + 
#                  plp + densidad + lri + apoyo_nacional + (1|country) + 
#                  have_index + SEX, family = binomial, data)
# 
# gml_plp_pm <- glmer(pm_dummy ~ clase + have_index + plp + (1|country), family = binomial, data)
# 
# gml_densidad_pm <- glmer(pm_dummy ~ clase + have_index + densidad + (1|country), family = binomial, data)
# 
# gml_lri_pm <- glmer(pm_dummy ~ clase + have_index + lri + (1|country), family = binomial, data)
# 
# gml_apoyo_pm <- glmer(pm_dummy ~ clase + have_index + apoyo_nacional + (1|country), family = binomial, data)



# Ordinal -----------------------------------------------------------------

## Un nivel ----------------------------------------------------------------

clm_sim_pm <- clm(pm_suma ~ clase, data = data) 
clm_sc_pm <- clm(pm_suma ~ clase + have_index, data = data) 
clm_clase_pm <- clm(pm_suma ~ clase + have_index + SEX, data = data) 

## Multinivel --------------------------------------------------------------

clmm_tot_pm <- clmm(pm_suma ~ clase + have_index + SEX + 
                      plp + densidad + lri + apoyo_nacional + (1|country), 
                    data = data,
                    Hessian = T) 

clmm_plp_pm <- clmm(pm_suma ~ clase + have_index + SEX + 
                      plp + (1|country), 
                    data = data,
                    Hessian = T) 

clmm_densidad_pm <- clmm(pm_suma ~ clase + have_index + SEX + 
                           densidad + (1|country), 
                         data = data,
                         Hessian = T) 

clmm_lri_pm <- clmm(pm_suma ~ clase + have_index + SEX + 
                      lri + (1|country), 
                    data = data,
                    Hessian = T) 

clmm_apoyo_pm <- clmm(pm_suma ~ clase + have_index + SEX + 
                        apoyo_nacional + (1|country), 
                      data = data,
                      Hessian = T) 

save(clm_clase_pm, clm_sc_pm, clm_sim_pm, clmm_apoyo_pm, clmm_densidad_pm, clmm_lri_pm, clmm_plp_pm, clmm_tot_pm, file = "output/data/clm_pm.RData")

# Visualización -----------------------------------------------------------


# Regresión lineal múltiple -----------------------------------------------
# tab_model(list(lm_sim_pm, lm_sc_pm, lm_clase_pm),
#           show.ci = F,
#           string.pred = "Predictores",
#           string.est = "β",
#           string.p = "P-valor",
#           string.intercept = "Intercepto")


# Regresión logística binaria ---------------------------------------------

# tab_model(list(glm_sim_pm, glm_sc_pm, glm_clase_pm),
#           show.ci = F,
#           string.pred = "Predictores",
#           string.est = "β",
#           string.p = "P-valor",
#           string.intercept = "Intercepto", 
#           ci_method = "wald")

# Multinivel lineal --------------------------------------------------------------
# tab_model(list(ml_plp_pm, ml_densidad_pm, ml_lri_pm, ml_apoyo_pm, ml_tot_pm),
#           show.ci = F,
#           string.pred = "Predictores",
#           string.est = "β",
#           string.p = "P-valor",
#           string.intercept = "Intercepto")

# Regresión logística binaria multinivel ---------------------------------------------

# tab_model(list(gml_plp_pm, gml_densidad_pm, gml_lri_pm, gml_apoyo_pm, gml_tot_pm),
#           show.ci = F,
#           string.pred = "Predictores",
#           string.est = "β",
#           string.p = "P-valor",
#           string.intercept = "Intercepto", 
#           ci_method = "wald")



## Ordinal -----------------------------------------------------------------

tab_model(list(clm_sim_pm, clm_sc_pm, clm_clase_pm),
          title = "Regresion ordinal multinivel sobre actitud solidaria hacia el trabajo",  
          transform = NULL,
          auto.label = T, 
          dv.labels = c("Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo"),
          collapse.se = T,
          show.ci = F,
          show.aic = T,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          file = "output/fig/clm_pm.html")
webshot("output/fig/clm_pm.html", "output/fig/clm_pm.png")

## Multinivel --------------------------------------------------------------
tab_model(list(clmm_plp_pm, clmm_densidad_pm, clmm_lri_pm, clmm_apoyo_pm, clmm_tot_pm),
          title = "Regresion ordinal multinivel sobre actitud solidaria hacia el trabajo",  
          transform = NULL,
          auto.label = T,
          dv.labels = c("Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo"),
          collapse.se = T,
          show.ci = F,
          show.aic = T,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          file = "output/fig/clmm_pm.html")
webshot("output/fig/clmm_pm.html", "output/fig/clmm_pm.png")


# Lmer --------------------------------------------------------------------

lm_sim_pm <- lm(pm_suma ~ clase, data = data) 
lm_sc_pm <- lm(pm_suma ~ clase + have_index, data = data) 
lm_clase_pm <- lm(pm_suma ~ clase + have_index + SEX, data = data) 

ml_tot_pm <- lmer(pm_suma ~ clase +
                 (plp + densidad + lri + apoyo_nacional|country) +
                 have_index + SEX, data)

ml_plp_pm <- lmer(pm_suma ~ clase + have_index +(plp|country), data)

ml_densidad_pm <- lmer(pm_suma ~ clase + have_index + (densidad|country), data)

ml_lri_pm <- lmer(pm_suma ~ clase + have_index + (lri|country), data)

ml_apoyo_pm <- lmer(pm_suma ~ clase + have_index + (apoyo_nacional|country), data)

tab_model(list(lm_sim_pm, lm_sc_pm, lm_clase_pm),
          title = "Regresion ordinal sobre actitud solidaria hacia el trabajo",  
          auto.label = T, 
          dv.labels = c("Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          file = "output/fig/lm_pm.html")
webshot("output/fig/lm_pm.html", "output/fig/lm_pm.png")

tab_model(list(ml_plp_pm, ml_densidad_pm, ml_lri_pm, ml_apoyo_pm, ml_tot_pm),
          title = "Regresion ordinal multinivel sobre actitud solidaria hacia el trabajo",  
          auto.label = T,
          dv.labels = c("Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo",
                        "Actitud solidaria hacia el trabajo"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          file = "output/fig/ml_pm.html")
webshot("output/fig/ml_pm.html", "output/fig/ml_pm.png")
