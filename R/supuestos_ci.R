
# Análisis de supuestos modelos lineales ----------------------------------

rm(list = ls())
# Cargar paquetes ---------------------------------------------------------

pacman::p_load(tidyverse, lme4, car, lmtest, MASS,
               performance, lattice, sjPlot)

# Cargar datos ------------------------------------------------------------

data <- readRDS("output/data/data.rds")

# Crear modelos -----------------------------------------------------------

mod = lmer(job_money ~ clase + UNION + SEX +(ipo|iso3c), data)

# Análisis de supuestos ---------------------------------------------------

## Linealidad --------------------------------------------------------------
pred <- fitted(mod)
obs <- data$job_money
ipo = data$ipo
lineal = data.frame(obs, pred, ipo, residuos = residuals(mod))
plot(lineal$residuos, lineal$pred) 
#Existe patrón, no hay linealidad
#Relación (no) lineal entre residuos y predichos

ggplot(lineal, aes(residuos, pred)) +
  geom_point() + 
  labs(title = "Gráfico 1",
       subtitle = "Correlación entre residuos y valores predichos",
       caption = "Elaboración propia") +
  xlab("Residuos") + ylab("Valores predichos") +
  theme_classic()

sjPlot::save_plot("output/fig/linealidad.png", fig = last_plot())

## Test homogeneidad de varianza -------------------------------------------
check_heteroscedasticity(mod)
data$residuos <- residuals(mod)
data$abs_residuos <-abs(data$residuos)
data$residuos2 <- data$abs_residuos^2
modelo_levene <- lm(residuos2 ~ iso3c, data=data)
anova(modelo_levene) #Valor <0.5, no se cumple homoscedasticidad,
#varianza de residuos no es igual en todos los grupos
#https://ademos.people.uic.edu/Chapter18.html#62_assumption_2_homogeneity_of_variance
plot(mod)

## Normalidad de residuos --------------------------------------------------
check_normality(mod) #Test Saphiro-Wilk
qqnorm(lineal$residuos, pch = 1, frame = FALSE,
       main = "Gráfico 2. 
       Análisis de normalidad de residuos",
       xlab = "Cuantiles teóricos",
       ylab = "Cuantiles observados")
qqline(lineal$residuos, col = "steelblue", lwd = 2)

sjPlot::save_plot("output/fig/norm_residuos.png", fig = last_plot())

## Multicolinealidad -------------------------------------------------------
plot(check_collinearity(mod))

# Independencia de residuos -----------------------------------------------
check_autocorrelation(mod) #Durbin-Watson
durbinWatsonTest(lineal$residuos) #https://www.investopedia.com/terms/d/durbin-watson-statistic.asp
#https://godatadrive.com/blog/basic-guide-to-test-assumptions-of-linear-regression-in-r

## Casos influyentes -------------------------------------------------------
plot(check_outliers(mod))
check_outliers(mod)
