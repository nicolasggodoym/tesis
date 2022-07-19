rm(list = ls())
pacman::p_load(lme4, sjmisc, sjPlot, tidyverse, lmerTest, ordinal)
data <- readRDS("output/data/data.rds")
#load("output/data/data.RData")

#lme4::lmer(dep ~ indep + (1 | 2doNivel), data)

# Modelos CI ----------------------------------------------------------

  
# Un nivel ----------------------------------------------------------------

clm_sim_ci <- clm2(job_money ~ clase, data = data) 
clm_sc_ci <- clm2(job_money ~ clase + have_index, data = data) 
clm_clase_ci <- clm2(job_money ~ clase + have_index + SEX + sector, data = data) 

# Multinivel --------------------------------------------------------------

clmm_tot_ci <- glmer(job_money ~ clase + 
                      plp + densidad + lri + apoyo_nacional + (1|country) + 
                      have_index + SEX + sector, family = binomial, data)

clmm_plp_ci <- glmer(job_money ~ clase + have_index + plp + (1|country), family = binomial, data)

clmm_densidad_ci <- glmer(job_money ~ clase + have_index + densidad + (1|country), family = binomial, data)

clmm_lri_ci <- glmer(job_money ~ clase + have_index + lri + (1|country), family = binomial, data)

clmm_apoyo_ci <- glmer(job_money ~ clase + have_index + apoyo_nacional + (1|country), family = binomial, data)


# Visualización -----------------------------------------------------------


# Un nivel -----------------------------------------------
tab_model(list(clm_sim_ci, clm_sc_ci, clm_clase_ci),
          show.ci = F,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto")


# Multinive --------------------------------------------------------------
tab_model(list(clmm_plp_ci, clmm_densidad_ci, clmm_lri_ci, clmm_apoyo_ci, clmm_tot_ci),
          show.ci = F,
          show.aic = T,
          string.pred = "Predictores",
          string.est = "β",
          string.p = "P-valor",
          string.intercept = "Intercepto")
