rm(list = ls())
pacman::p_load(tidyverse, lme4, sjmisc, 
               sjPlot, tidyverse, 
               lmerTest, #multileveltools,  
               webshot)
data <- readRDS("output/data/data.rds")

# Modelos CI ----------------------------------------------------------

data$country = as_factor(data$country)

# Visualización -----------------------------------------------------------

lm_sim_ci <- lm(job_money ~ clase, data = data) 
lm_sc_ci <- lm(job_money ~ clase + UNION, data = data) 
lm_clase_ci <- lm(job_money ~ clase + UNION + SEX, data = data) 

ml_tot_ci <- lmer(job_money ~ clase + 
                    (plp + lri|country) +
                    UNION + SEX, data)

ml_plp_ci <- lmer(job_money ~ clase + UNION + SEX + (plp|country), data)

ml_lri_ci <- lmer(job_money ~ clase + UNION + SEX + (lri|country), data)

# Un nivel -----------------------------------------------

tab_model(list(lm_sim_ci, lm_sc_ci, lm_clase_ci),
          title = "Regresion lineal sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T, 
          dv.labels = c("Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          file = "output/fig/lm_ci.html")
webshot("output/fig/lm_ci.html", "output/fig/lm_ci.png")

# Multinivel --------------------------------------------------------------
tab_model(list(ml_plp_ci, ml_lri_ci,  ml_tot_ci),
          title = "Regresion lineal multinivel sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T,
          dv.labels = c("Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo"),
          collapse.se = T,
          show.ci = F,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto",
          file = "output/fig/ml_ci.html")
webshot("output/fig/ml_ci.html", "output/fig/ml_ci.png")


