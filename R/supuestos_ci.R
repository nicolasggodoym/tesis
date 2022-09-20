
# Análisis de supuestos modelos lineales ----------------------------------

rm(list = ls())
# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse, lme4, car, lmtest, MASS)

# Cargar datos ------------------------------------------------------------

data <- readRDS("output/data/data.rds")

# Crear modelos -----------------------------------------------------------

plp <- lmer(job_money ~ clase + UNION + SEX + (plp|country), data)

#densidad <- lmer(job_money ~ clase + UNION + SEX + (densidad|country), data)

lri <- lmer(job_money ~ clase + UNION + SEX + (lri|country), data)

# Análisis de supuestos ---------------------------------------------------


## Casos influyentes -------------------------------------------------------

### PLP ---------------------------------------------------------------------
n<- nobs(plp) #n de observaciones
kplp<- length(coef(plp)) # n de parametros
dcplp<- 4/(n-kplp-1) #punt de corte

plp2 <- broom::augment_columns(plp,data)
plp2$id <- as.numeric(row.names(plp2))
# identify obs with Cook's D above cutoff
ggplot(plp2, aes(id, .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=dcplp)+
  geom_text(aes(label=ifelse((.cooksd>dcplp),id,"")),
            vjust=-0.2, hjust=0.5)

identplp <- plp2 %>% filter(.cooksd>dcplp)
data_plp <- plp2 %>% filter(!(id %in% identplp$id))

mlplp2<- lmer(job_money ~ clase + UNION + SEX +(plp|country), data_plp)

tab_model(list(plp, mlplp2))

### Densidad ----------------------------------------------------------------
n<- nobs(densidad) #n de observaciones
kdensidad<- length(coef(densidad)) # n de parametros
dcdensidad<- 4/(n-kdensidad-1) #punt de corte

densidad2 <- broom::augment_columns(densidad,data)
densidad2$id <- as.numeric(row.names(densidad2))
# identify obs with Cook's D above cutoff
ggplot(densidad2, aes(id, .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=dcdensidad)+
  geom_text(aes(label=ifelse((.cooksd>dcdensidad),id,"")),
            vjust=-0.2, hjust=0.5)

identdensidad <- densidad2 %>% filter(.cooksd>dcdensidad)
data_densidad <- densidad2 %>% filter(!(id %in% identdensidad$id))

mldensidad2<- lmer(job_money ~ clase + UNION + SEX +(plp|country), data_densidad)

tab_model(list(plp, mldensidad2))

### LRI ---------------------------------------------------------------------
n<- nobs(lri) #n de observaciones
klri<- length(coef(lri)) # n de parametros
dclri<- 4/(n-klri-1) #punt de corte

lri2 <- broom::augment_columns(lri,data)
lri2$id <- as.numeric(row.names(lri2))
# identify obs with Cook's D above cutoff
ggplot(lri2, aes(id, .cooksd))+
  geom_bar(stat="identity", position="identity")+
  xlab("Obs. Number")+ylab("Cook's distance")+
  geom_hline(yintercept=dclri)+
  geom_text(aes(label=ifelse((.cooksd>dclri),id,"")),
            vjust=-0.2, hjust=0.5)

identlri <- lri2 %>% filter(.cooksd>dclri)
data_lri <- lri2 %>% filter(!(id %in% identlri$id))

mllri2<- lmer(job_money ~ clase + UNION + SEX +(plp|country), data_lri)

tab_model(list(plp, mllri2))

## Linealidad --------------------------------------------------------------

### PLP ---------------------------------------------------------------------
linplp <-plot(resid(plp),
              data$job_money) #resid() calls for the residuals of the model, Cigarettes was our initial outcome variables - we're plotting the residuals vs observered


claseplp<- mlplp2$model$clase
fitplp<- mlplp2$fitted.values
data01 <- as.data.frame(cbind(edad,fit))

ggplot(data01, aes(x = edad, y = fit)) +
  theme_bw() +
  geom_point()+
  geom_smooth()

elsoc02$lningreso <- log(elsoc02$ing_pcap)
elsoc02$lningreso <- set_label(elsoc02$lningreso,"log(ingreso per cap)")
fit06 <- lm(partpol~sexo+edad+edad2+lningreso+pospol,data=elsoc02)

plot_frq(elsoc02$ing_pcap,type = "hist",normal.curve = T, show.mean = T)
plot_frq(elsoc02$lningreso,type = "hist", normal.curve = T,show.mean = T)

labs03 <- c("Intercepto","Sexo (mujer=1)","Edad",
            "Quintil 2","Quintil 3","Quintil 4","Quintil 5","Quintil perdido",
            "Izquierda (ref. derecha)","Centro","Idep./Ninguno", "Edad²","Ingreso per cap (log)")

htmlreg(list(fit04, fit05, fit06), doctype = FALSE,
        custom.model.names = c("Modelo 4", "Modelo 5", "Modelo 6"), 
        custom.coef.names = labs03)

### Densidad ----------------------------------------------------------------


### LRI ---------------------------------------------------------------------




## Test homogeneidad de varianza -------------------------------------------
### PLP ---------------------------------------------------------------------
data$n <- nrow(data)
data$plp_res<- residuals(plp) #extracts the residuals and places them in a new column in our original data table
data$plp_absres <-abs(data$plp_res) #creates a new column with the absolute value of the residuals
data$plp_res2 <- data$plp_absres^2 #squares the absolute values of the residuals to provide the more robust estimate
plplevene <- lm(plp_res2 ~ n, data) #ANOVA of the squared residuals
anova(plplevene) #displays the results
plot(plplevene)

### Densidad ----------------------------------------------------------------


### LRI ---------------------------------------------------------------------


## Multicolinealidad -------------------------------------------------------
### PLP ---------------------------------------------------------------------


### Densidad ----------------------------------------------------------------


### LRI ---------------------------------------------------------------------

car::vif(fit04)
car::vif(fit05)

