rm(list = ls())
pacman::p_load(lme4, sjmisc, sjPlot, tidyverse, lmerTest, ordinal)
data <- readRDS("output/data/data.rds")
#load("output/data/data.RData")

#lme4::lmer(dep ~ indep + (1 | 2doNivel), data)

data$pm_ordinal <- car::recode(data$pm_suma, 
                               c("c(0,1) = 0;
                                 c(2,3) = 1;
                                 c(4,5) = 2;
                                 c(6,7) = 3;
                                 8 = 4"))

data$pm_ordinal <- set_label(data$pm_ordinal, "Índice de actitud solidaria hacia el trabajo")
data <- data %>% 
  group_by(country) %>% 
  mutate(pm_dummy = ifelse(pm_index > mean(pm_index, na.rm = T), 1, 
                    ifelse(pm_index <= mean(pm_index, na.rm = T), 0, NA))) %>% 
  ungroup()
data$pm_dummy <- set_label(data$pm_dummy, "Índice de actitud solidaria hacia el trabajo")


# Modelos PM ----------------------------------------------------------


# Regresión lineal múltiple -----------------------------------------------

lm_sim_pm <- lm(pm_index ~ clase, data)

lm_sc_pm <- lm(pm_index ~ clase + have_index, data)

lm_clase_pm <- lm(pm_index ~ clase + have_index + SEX +sector, data)

# Regresión logística binaria -----------------------------------------------

glm_sim_pm <- glm(pm_dummy ~ clase, family = binomial(link = "logit"), data)

glm_sc_pm <- glm(pm_dummy ~ clase + have_index, family = binomial(link = "logit"), data)

glm_clase_pm <- glm(pm_dummy ~ clase + have_index + SEX +sector, family = binomial(link = "logit"), data)

# Multinivel --------------------------------------------------------------

#Lineal
ml_tot_pm <- lmer(pm_index ~ clase + 
                 (plp + densidad + lri + apoyo_nacional|country) + 
                 have_index + SEX + sector, data)

ml_plp_pm <- lmer(pm_index ~ clase + have_index + plp + (1|country), data)

ml_densidad_pm <- lmer(pm_index ~ clase + have_index + densidad + (1|country), data)

ml_lri_pm <- lmer(pm_index ~ clase + have_index + lri + (1|country), data)

ml_apoyo_pm <- lmer(pm_index ~ clase + have_index + apoyo_nacional + (1|country), data)

#Binaria
gml_tot_pm <- glmer(pm_dummy ~ clase + 
                 plp + densidad + lri + apoyo_nacional + (1|country) + 
                 have_index + SEX + sector, family = binomial, data)

gml_plp_pm <- glmer(pm_dummy ~ clase + have_index + plp + (1|country), family = binomial, data)

gml_densidad_pm <- glmer(pm_dummy ~ clase + have_index + densidad + (1|country), family = binomial, data)

gml_lri_pm <- glmer(pm_dummy ~ clase + have_index + lri + (1|country), family = binomial, data)

gml_apoyo_pm <- glmer(pm_dummy ~ clase + have_index + apoyo_nacional + (1|country), family = binomial, data)



# Visualización -----------------------------------------------------------


# Regresión lineal múltiple -----------------------------------------------
tab_model(list(lm_sim_pm, lm_sc_pm, lm_clase_pm),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto")


# Regresión logística binaria ---------------------------------------------

tab_model(list(glm_sim_pm, glm_sc_pm, glm_clase_pm),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto", 
          ci_method = "wald")

# Multinivel lineal --------------------------------------------------------------
tab_model(list(ml_plp_pm, ml_densidad_pm, ml_lri_pm, ml_apoyo_pm, ml_tot_pm),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto")

# Regresión logística binaria multinivel ---------------------------------------------

tab_model(list(gml_plp_pm, gml_densidad_pm, gml_lri_pm, gml_apoyo_pm, gml_tot_pm),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto", 
          ci_method = "wald")
