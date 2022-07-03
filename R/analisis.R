rm(list = ls())
pacman::p_load(lme4, sjmisc, sjPlot, tidyverse, lmerTest)
final <- readRDS("output/data/data.rds")
#load("output/data/data.RData")

#lme4::lmer(dep ~ indep + (1 | 2doNivel), data)


# Cognitivo-instrumental --------------------------------------------------

str <- lmer(strategic_index ~ clase + have_index + plp + densidad + lri +
              apoyo_nacional + (1|iso2c), final)

summary(str)

str_plp <- lmer(strategic_index ~ clase + have_index + plp + (1|iso2c), final)
summary(str_plp)

str_densidad <- lmer(strategic_index ~ clase + have_index + densidad + (1|iso2c), final)
summary(str_densidad)

str_lri <- lmer(strategic_index ~ clase + have_index + lri + (1|iso2c), final)
summary(str_lri)

str_apoyo <- lmer(strategic_index ~ clase + have_index + apoyo_nacional + (1|iso2c), final)
summary(str_apoyo)

# Práctico-moral ----------------------------------------------------------

pm <- lmer(pm_index ~ clase + have_index + SEX + sector + (plp + densidad + lri +
                                        apoyo_nacional|iso3c), final)

pm_plp <- lmer(pm_index ~ clase + have_index + plp + (1|iso2c), final)
summary(pm_plp)

pm_densidad <- lmer(pm_index ~ clase + have_index + densidad + (1|iso2c), final)
summary(pm_densidad)

pm_lri <- lmer(pm_index ~ clase + have_index + lri + (1|iso2c), final)
summary(pm_lri)

pm_apoyo <- lmer(pm_index ~ clase + have_index + apoyo_nacional + (1|iso2c), final)
summary(pm_apoyo)


# Estético-expresiva ------------------------------------------------------

expr <- lmer(expr_index ~ clase + have_index + plp + densidad + lri +
                    apoyo_nacional + (1|iso2c), final)
summary(expr)

expr_plp <- lmer(expr_index ~ clase + have_index + plp + (1|iso2c), final)
summary(pm_plp)

expr_densidad <- lmer(expr_index ~ clase + have_index + densidad + (1|iso2c), final)
summary(pm_densidad)

expr_lri <- lmer(expr_index ~ clase + have_index + lri + (1|iso2c), final)
summary(pm_lri)

expr_apoyo <- lmer(expr_index ~ clase + have_index + apoyo_nacional + (1|iso2c), final)
summary(pm_apoyo)
