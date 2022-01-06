# Estructura de Clases de E. O Wright ---------------

# 1. Cargar librarias
pacman::p_load(haven, dplyr, car, tidyverse, ggplot2, ggsci)

# 2. Cargar base de datos
issp <- read_dta("G:/SociologÌa/Fondecyt N∞11190229 Determinantes institucionales y polÌticos de conflicto/ISSP/Work Orientations/ZA6770_v2-1-0.dta")

# 3.Seleccionar variables
# ¬øCu√°les son las variables relevantes para el esquema de E.O Wright? 
# 1. Propiedad; 2. Autoridad; 3. Calificaciones
issp <- select(issp,
               NEMPLOY, #
               WORK, # Employee/Unemployee
               EMPREL, # Asalariado/No Asalariado 
               WRKSUP, #Supervision
               ISCO08, #Skills
               SEX, # sexo
               c_alphan) # Country

# 4. Recodificar variables -------------

# 4.1 Propietarios -------------------
# Proxy: NEMPLOY (How many employees do/ did you have, not counting yourself?)

# A.1 Crear variable "capitalistas":
#1.PequeÒa burguesia: 0 a 1 empleados
#2. PequeÒos empleadores: de 2 a 9 empleados
#3. Capitalist: de 10 a m·s empleados
issp$self_emply <- as.numeric(issp$NEMPLOY)
summary(issp$self_emply)
issp$self_emply <- car::recode(issp$self_emply,recodes = "0:1='PequeÒa burguesia';2:9='PequeÒos empleadores';10:hi='Burguesia'; c(9998,9999)=NA", as.factor = T,
                               levels = c("Burguesia","PequeÒos empleadores","PequeÒa burguesia"))

# A.2 Verificar
issp %>% count(self_emply) %>% mutate(prop = prop.table(n))

# 4.2 Asalariados --------------------

# 4.2.1 Asalariados ------------------
# Proxy: (A) ¬øWORRK o EMPREL? (# https://zacat.gesis.org/webview/index.jsp?object=http://zacat.gesis.org/obj/fStudy/ZA6770)
# (A) WORK: Are you currently working for pay, did you work for pay in the past, or have you never been in paid work?
# (B) EMPREL: Are/ were you an employee, self-employed, or working for your own family's business?

# A.1 Filtrar inactivos
issp <- filter(issp, WORK != 3) #¬øPor qu√© no a desempleados?
# A.2 Crear variable asalariados
issp$EMPREL <- as.numeric(issp$EMPREL) 
issp$wage_earners <- car::recode(issp$EMPREL,recodes = "1='Asalariado';c(2,3,4,9)='No Asalariado'", as.factor = T,
                                 levels = c("Asalariado", "No Asalariado"))
# Verificar
issp %>% count(wage_earners) %>% mutate(prop = prop.table(n))

# 4.2.2 Supervisan --------------------
# Proxy: WRKSUP #Do/ did you supervise other employees? Yes/No
issp$WRKSUP <- as.numeric(issp$WRKSUP)
issp$control <- car::recode(issp$WRKSUP,recodes = "1='Control';c(2,8,9)='No control'",as.factor =T, 
                            levels = c("Control", "No control"))

# Verificar
issp %>% count(control) %>% mutate(prop = prop.table(n)) 

# 4.2.3 Skills
# Proxy: ISCO08 #What is/ was your occupation - i.e., what is/ was the name or title of your main job?
# In your main job, what kind of activities do/ did you do most of the time?
# (https://zacat.gesis.org/webview/index.jsp?object=http://zacat.gesis.org/obj/fStudy/ZA6770)

# A.1 Transformar a num√©rica
issp$ISCO08 <- as.numeric(issp$ISCO08)

#A.2 Eliminar FFAA
issp <- issp %>% filter(ISCO08!=110,ISCO08!=210, ISCO08!=310)
table(issp$ISCO08)

#A.3 Quedarme solo con los primeros dos digitos
issp$ISCO <- substr(issp$ISCO08, start = 1, stop = 2) 
table(issp$ISCO)

#A.4 Agrupar por "skills"

#Grupo 1: Profesionales y Managers (del 10 al 26)
#Grupo 2: T√©cnicos o Semi-credencializados.  Desde el c√≥digo 30 hasta el 35. Sumar 51, 60 61 y 72. 
#Grupo 3: No calificados. Son 40, 41, 42, 43, 44,50, 52, 53, 54, 62, 63, 71. Luego del 73 al 96.
#Grupo 4: NA
issp$skills <- car::recode(issp$ISCO, 
                               recodes = "10:26='Experto';c(30,31,32,33,34,35,51,60,61,72)='Calificado'
                               ;c(40,41,42,43, 44,50,52,53,54,62,63,70,71,73,74,75,76,76,77,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96)='No calificado';
                               c(99)=NA", as.factor = T, levels = c("Experto", "Calificado", "No calificado"))

# A.5 Verificar
issp %>% count(skills) %>% mutate(prop = prop.table(n))
#######3. FINAL CLASS VARIABLE#####

#3.1 Se genera variable class
issp$class<- NA #crear variable vac√≠a
issp$class <- with(issp, ifelse(wage_earners=="No Asalariado" & self_emply=="Burguesia", 1, class))
issp$class <- with(issp, ifelse(wage_earners=="No Asalariado"& self_emply=="PequeÒos empleadores", 2, class))
issp$class <- with(issp, ifelse(wage_earners=="No Asalariado"& self_emply=="PequeÒa burguesia", 3, class))
issp$class <- with(issp, ifelse(wage_earners=="Asalariado" & control=="Control" & skills=="Experto", 4, class))
issp$class <- with(issp, ifelse(wage_earners=="Asalariado" & control=="No control" & skills=="Experto", 5, class))
issp$class <- with(issp, ifelse(wage_earners=="Asalariado" & control=="Control" & skills=="Calificado", 6, class))
issp$class <- with(issp, ifelse(wage_earners=="Asalariado" & control=="No control" & skills=="Calificado", 7, class))
issp$class <- with(issp, ifelse(wage_earners=="Asalariado" & control=="Control" & skills=="No calificado", 8, class))
issp$class <- with(issp, ifelse(wage_earners=="Asalariado" & control=="No control" & skills=="No calificado", 9, class))


#3.2 Etiquetar class
issp$class <- factor(issp$class,levels = c(1:9),
                         labels = c("Burguesia","PequeÒos empleadores","PequeÒa burguesia",
                                    "Experto directivo/supervisor","Experto no directivo",
                                    "Directivo/supervisor calificado","Obrero calificado",
                                    "Directivo/supervisor no calificado","Proletarios"))

table(issp$class)

#3.3 Class scheme
class = issp %>% 
  filter(!is.na(class)) %>% 
  count(class) %>% #n de casos
  mutate(proporcion = prop.table(n))#proporci√≥n

## ¬ø Qu√© pasa por pais? ------------------------

## Esquema de clases en Chile
class_cl = issp %>%
  filter(!is.na(class), c_alphan == "CL") %>%  
  count(class) %>% 
  mutate(proporcion = prop.table(n))

## Esquema de clases en US
class_us = issp %>%
  filter(!is.na(class), c_alphan == "US") %>%  
  count(class) %>% 
  mutate(proporcion = prop.table(n))

## Esquema de clases en Chile-por genero
class_cl_gen = issp %>%
  filter(!is.na(class), c_alphan == "CL", SEX != 9 ) %>%
  group_by(class, SEX) %>% 
  summarise(n = n()) %>%
  mutate(proporcion = n/sum(n))


# Graficos ----------------------------------------------------------------
ggplot(class_cl, aes(x= str_wrap(class,15), y = round(proporcion,3)*100, fill = class)) + 
  geom_bar(stat = "identity", color = "black", width = 0.8, position = "dodge2") + 
  geom_text(aes(label = paste0(round(proporcion,3)*100,"%")),position=position_dodge(width=0.9),
            vjust=-0.25, color="black", size= 5) +
  labs(x = "Clase social", y = "%", 
       title = "Gr√°fico 1. Esquema de clases E.O Wright en Chile",fill = "",
       caption = "Fuente: ISSP (2015)")  + 
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_nejm() + theme_minimal()

ggplot(class_us, aes(x= str_wrap(class,15), y = round(proporcion,3)*100, fill = class)) + 
  geom_bar(stat = "identity", color = "black", width = 0.8, position = "dodge2") + 
  geom_text(aes(label = paste0(round(proporcion,3)*100,"%")),position=position_dodge(width=0.9),
            vjust=-0.25, color="black", size= 5) +
  labs(x = "Clase social", y = "%", 
       title = "Gr√°fico 2. Esquema de clases E.O Wright en Estados Unidos",fill = "",
       caption = "Fuente: ISSP (2015)")  + 
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_nejm() + theme_minimal()


ggplot(class_cl_gen, aes(x= str_wrap(class,15), y = round(proporcion,3)*100, fill = as_factor(SEX))) + 
  geom_bar(stat = "identity", color = "black", width = 0.8, position = "dodge2") + 
  geom_text(aes(label = paste0(round(proporcion,3)*100,"%")),position=position_dodge(width=0.9),
            vjust=-0.25, color="black", size= 5) +
  labs(x = "Clase social", y = "%", 
       title = "Gr√°fico 3. Esquema de clases E.O Wright en Chile por sexo",fill = "",
       caption = "Fuente: ISSP (2015)")  + 
  theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom") + scale_fill_nejm() + theme_minimal()


