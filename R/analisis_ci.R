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
lm_un_ci <- lm(job_money ~ clase + UNION, data = data)
lm_sx_ci <- lm(job_money ~ clase + SEX, data = data) 
lm_clase_ci <- lm(job_money ~ clase + UNION + SEX, data = data) 

ml_tot_ci <- lmer(job_money ~ clase + 
                    (plp + lri + densidad|country) +
                    UNION + SEX, data)

ml_den_ci <- lmer(job_money ~ clase + UNION + SEX + (densidad|country), data)
ml_plp_ci <- lmer(job_money ~ clase + UNION + SEX + (plp|country), data)

ml_lri_ci <- lmer(job_money ~ clase + UNION + SEX + (lri|country), data)

ml_tot_std <- lmer(job_money ~ clase + 
                    (plp_std + lri_std + den_std|country) +
                    UNION + SEX, data)

ml_den_std <- lmer(job_money ~ clase + UNION + SEX + (den_std|country), data)
ml_plp_std <- lmer(job_money ~ clase + UNION + SEX + (plp_std|country), data)

ml_lri_std <- lmer(job_money ~ clase + UNION + SEX + (lri_std|country), data)

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
tab_model(list(ml_plp_ci, ml_lri_ci, ml_den_ci, ml_tot_ci),
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
          file = "output/fig/ml_ci.html")
webshot("output/fig/ml_ci.html", "output/fig/ml_ci.png")

tab_model(list(ml_plp_std, ml_lri_std, ml_den_std, ml_tot_std),
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
          encoding = "UTF-8")
webshot("output/fig/ml_std.html", "output/fig/ml_std.png")
