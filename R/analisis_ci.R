rm(list = ls())
pacman::p_load(tidyverse, lme4, sjmisc, 
               sjPlot, tidyverse, 
               lmerTest, ordinal, 
               webshot, MCMCglmm)
data <- readRDS("output/data/data.rds")

# Modelos CI ----------------------------------------------------------

data$job_money = as_factor(data$job_money)
data$country = as_factor(data$country)
# Un nivel ----------------------------------------------------------------

clm_sim_ci <- clm(job_money ~ clase, data = data) 
clm_sc_ci <- clm(job_money ~ clase + have_index, data = data) 
clm_clase_ci <- clm(job_money ~ clase + have_index + SEX, data = data) 

# Multinivel --------------------------------------------------------------

clmm_tot_ci <- clmm(job_money ~ clase + have_index + SEX + 
                       plp + densidad + lri + apoyo_nacional + (1|country), 
                       data = data,
                     Hessian = T) 

clmm_plp_ci <- clmm(job_money ~ clase + have_index + SEX + 
                      plp + (1|country), 
                    data = data,
                    Hessian = T) 

clmm_densidad_ci <- clmm(job_money ~ clase + have_index + SEX + 
                         densidad + (1|country), 
                         data = data,
                         Hessian = T) 

clmm_lri_ci <- clmm(job_money ~ clase + have_index + SEX + 
                    lri + (1|country), 
                    data = data,
                    Hessian = T) 

clmm_apoyo_ci <- clmm(job_money ~ clase + have_index + SEX + 
                      apoyo_nacional + (1|country), 
                      data = data,
                      Hessian = T) 

save(clm_clase_ci, clm_sc_ci, clm_sim_ci, clmm_apoyo_ci, clmm_densidad_ci, clmm_lri_ci, clmm_plp_ci, clmm_tot_ci, file = "output/data/clm_ci.RData")

# Visualización -----------------------------------------------------------


# Un nivel -----------------------------------------------
tab_model(list(clm_sim_ci, clm_sc_ci, clm_clase_ci),
          title = "Regresion ordinal sobre actitud mercantilizada hacia el trabajo", 
          transform = NULL,
          auto.label = T,
          dv.labels = c("Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo"), 
          collapse.se = T,
          show.ci = F,
          show.aic = T,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto", 
          file = "output/fig/clm_ci.html")
webshot("output/fig/clm_ci.html", "output/fig/clm_ci.png")

# Multinivel --------------------------------------------------------------
tab_model(list(clmm_plp_ci, clmm_densidad_ci, clmm_lri_ci, clmm_apoyo_ci, clmm_tot_ci),
          title = "Regresion ordinal multinivel sobre actitud mercantilizada hacia el trabajo",  
          transform = NULL,
          auto.label = T,
          dv.labels = c("Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo"),
          encoding = "UTF-8",
          collapse.se = T,
          show.ci = F,
          show.aic = T,
          string.pred = "Predictores",
          string.est = "Coef.",
          string.p = "P-valor",
          string.intercept = "Intercepto", 
          file = "output/fig/clmm_ci.html")
webshot("output/fig/clmm_ci.html", "output/fig/clmm_ci.png")


lm_sim_ci <- lm(job_money ~ clase, data = data) 
lm_sc_ci <- lm(job_money ~ clase + have_index, data = data) 
lm_clase_ci <- lm(job_money ~ clase + have_index + SEX, data = data) 

ml_tot_ci <- lmer(job_money ~ clase +
                    (plp + densidad + lri + apoyo_nacional|country) +
                    have_index + SEX, data)

ml_plp_ci <- lmer(job_money ~ clase + have_index +(plp|country), data)

ml_densidad_ci <- lmer(job_money ~ clase + have_index + (densidad|country), data)

ml_lri_ci <- lmer(job_money ~ clase + have_index + (lri|country), data)

ml_apoyo_ci <- lmer(job_money ~ clase + have_index + (apoyo_nacional|country), data)

tab_model(list(lm_sim_ci, lm_sc_ci, lm_clase_ci),
          title = "Regresion ordinal sobre actitud mercantilizada hacia el trabajo",  
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

tab_model(list(ml_plp_ci, ml_densidad_ci, ml_lri_ci, ml_apoyo_ci, ml_tot_ci),
          title = "Regresion ordinal multinivel sobre actitud mercantilizada hacia el trabajo",  
          auto.label = T,
          dv.labels = c("Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
                        "Actitud mercantilizada hacia el trabajo",
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


