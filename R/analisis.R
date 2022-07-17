rm(list = ls())
pacman::p_load(lme4, sjmisc, sjPlot, tidyverse, lmerTest, ordinal)
data <- readRDS("output/data/data.rds")
#load("output/data/data.RData")

#lme4::lmer(dep ~ indep + (1 | 2doNivel), data)

data$pm_ordinal <- case_when(data$pm_index <= 33.3 ~ "Bajo",
                           data$pm_index > 33.3 & data$pm_index <= 66.6 ~ "Medio",
                           data$pm_index > 66.6 ~ "Alto",
                           TRUE ~ NA_character_)

data$pm_ordinal <- set_label(data$pm_ordinal, "Índice de actitud solidaria hacia el trabajo")
data <- data %>% 
  group_by(country) %>% 
  mutate(pm_dummy = ifelse(pm_index > mean(pm_index, na.rm = T), 1, 
                    ifelse(pm_index <= mean(pm_index, na.rm = T), 0, NA))) %>% 
  ungroup()
data$pm_dummy <- set_label(data$pm_dummy, "Índice de actitud solidaria hacia el trabajo")


# Modelos PM ----------------------------------------------------------


# Regresión lineal múltiple -----------------------------------------------

lm_sim <- lm(pm_index ~ clase, data)

lm_sc <- lm(pm_index ~ clase + have_index, data)

lm_clase <- lm(pm_index ~ clase + have_index + SEX +sector, data)

# Regresión logística binaria -----------------------------------------------

glm_sim <- glm(pm_dummy ~ clase, family = binomial(link = "logit"), data)

glm_sc <- glm(pm_dummy ~ clase + have_index, family = binomial(link = "logit"), data)

glm_clase <- glm(pm_dummy ~ clase + have_index + SEX +sector, family = binomial(link = "logit"), data)

# Multinivel --------------------------------------------------------------

#Lineal
ml_tot <- lmer(pm_index ~ clase + 
                 (plp + densidad + lri + apoyo_nacional|country) + 
                 have_index + SEX + sector, data)

ml_plp <- lmer(pm_index ~ clase + have_index + plp + (1|country), data)

ml_densidad <- lmer(pm_index ~ clase + have_index + densidad + (1|country), data)

ml_lri <- lmer(pm_index ~ clase + have_index + lri + (1|country), data)

ml_apoyo <- lmer(pm_index ~ clase + have_index + apoyo_nacional + (1|country), data)

#Binaria
gml_tot <- glmer(pm_dummy ~ clase + 
                 plp + densidad + lri + apoyo_nacional + (1|country) + 
                 have_index + SEX + sector, family = binomial, data)

gml_plp <- glmer(pm_dummy ~ clase + have_index + plp + (1|country), family = binomial, data)

gml_densidad <- glmer(pm_dummy ~ clase + have_index + densidad + (1|country), family = binomial, data)

gml_lri <- glmer(pm_dummy ~ clase + have_index + lri + (1|country), family = binomial, data)

gml_apoyo <- glmer(pm_dummy ~ clase + have_index + apoyo_nacional + (1|country), family = binomial, data)



# Visualización -----------------------------------------------------------


# Regresión lineal múltiple -----------------------------------------------
tab_model(list(lm_sim, lm_sc, lm_clase),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto")


# Regresión logística binaria ---------------------------------------------

tab_model(list(glm_sim, glm_sc, glm_clase),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto", 
          ci_method = "wald")

# Multinivel lineal --------------------------------------------------------------
tab_model(list(ml_plp, ml_densidad, ml_lri, ml_apoyo, ml_tot),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto")

# Regresión logística binaria multinivel ---------------------------------------------

tab_model(list(gml_plp, gml_densidad, gml_lri, gml_apoyo, gml_tot),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto", 
          ci_method = "wald")
