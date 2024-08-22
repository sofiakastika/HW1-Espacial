#Importo shapefile
terrenos <- read.csv("https://raw.githubusercontent.com/sofiakastika/HW1-Espacial/main/Terrenos-en-venta-2019.csv")


#Remuevo toda la información
rm(list=ls()) 

#Descargo librerías
install.packages(stargazer)
library(sf) # trabaja con objetos espaciales
library(tidyverse) # limpiar datos 
library(lmtest)
require(gridExtra)
library(stargazer)

#Importo shapefile
terrenos <- read.csv("C:/Users/sofia/Desktop/Maestría/Optativas/Segundo trimestre/Econometría Espacial/TP Espacial/Terrenos-en-venta-2019.csv")

#Paso los nombres de las variables a minúscula 
terrenos1 <- terrenos %>% 
  rename_all(tolower)

#Creo variable logaritmo del precio en dolares 
terrenos1 <- terrenos1 %>%
  mutate(lnprecio = log(preciousd))

#Creo variable logaritmo de metros cuadrados 
terrenos1 <- terrenos1 %>%
  mutate(lnm2total = log(m2total))

#Elimino valores sin sentido en las variables
terrenos1$lnm2total[is.infinite(terrenos1$lnm2total)] <- NA
terrenos1$lnprecio[is.infinite(terrenos1$lnprecio)] <- NA

# Variable barrio como factor
terrenos1$barrio <- as.factor(terrenos1$barrio)

# Modelo con fixed effects por barrio
modelo1 <- lm(lnprecio ~ lnm2total + barrio, data = terrenos1)
summary(modelo1) #resumen del modelo
n_obs <- nobs(modelo1) #cantidad de observaciones en el modelo
print(n_obs)

# Modelo sin fixed effects por barrio
modelo2 <- lm(lnprecio ~ lnm2total, data = terrenos1)
summary(modelo2) #resumen del modelo
n_obs2 <- nobs(modelo2) #cantidad de observaciones en el modelo
print(n_obs2)

#Saco a latex tabla 
stargazer(modelo2, modelo1, 
          type = "latex",
          dep.var.labels = c("Logaritmo precio en dólares"),
          covariate.labels = c("Logaritmo metros cuadrados totales", "FE por barrio"),
          omit.stat = c("ser", "f", "adj.rsq"),
          align = TRUE,
          no.space = TRUE,
          title = "",
         column.labels = c("Sin FE", "Con FE"))

# Guardo los residuos del modelo
residuos <- modelo1$residuals

# Añadir los residuos al data frame
terrenos1$residuos <- residuos

#Descargo nueva base .csv
#write.csv(terrenos1, "C:/Users/judit/OneDrive - Económicas - UBA/Documentos/GitHub/HW1-Espacial/terrenos1_con_residuos.csv", row.names = FALSE)

#Descargo nueva base .csv
#write.csv(terrenos1,"C:/Users/sofia/Desktop/Maestría/Optativas/Segundo trimestre/Econometría Espacial/TP Espacial/dataset1.csv", row.names = FALSE)

###################### 
# Ejercicios 3,4,5,6 #
######################

# Importo la base creada en QGIS desde mi escritorio porque de github lo lee mal no se que
terrenos_2 <- read.csv("/Users/tomasmarotta/Documents/GitHub/HW1-Espacial/BASEFINAL.csv")

# Elimino distancia al obelisco incorrecta
terrenos_2 <- subset(terrenos_2, select = -Distancia_obelisco)

# Le cambio el nombre a algunas variables para mergear en terrenos1
terrenos_2 <- terrenos_2 %>% 
  rename(poly_id = POLY_ID, dist_subte = Distance, dist_tren = Distancia_tren, dist_obe = Distancia_obelisco.correcta. , count_delitos = NUMPOINTS )

# Seleccionar solo las columnas que te interesan de terrenos_2 para mergear en terrenos1
terrenos_2_subset <- terrenos_2 %>% select(poly_id, dist_subte, dist_tren, dist_obe, count_delitos)

# Hacer el merge usando left_join
terrenos_mergeado <- terrenos1 %>% 
  left_join(terrenos_2_subset, by = "poly_id")

## Ejercicio 4 ##

# Estimo el mismo modelo que en el punto (1) pero sumando las variables creadas en el punto (3)
modelo_ej4 <- lm(lnprecio ~ m2total + almagro + balvanera + barrac_e + barrac_o + belgrano + boedo + boca + caballito + chacarita +
               coghlan + colegial + constitu + fl_norte + fl_sur + floresta + liniers + mataderos + mt_castro +
               montserr + nva_pomp + nunez + palermo + p_avell + p_chacab + p_chas + p_patric + paternal + p_mader +
               recoleta + retiro + saavedra + san_cris + san_nico + san_telmo + v_sars + versalles + vcrespo +
               vdelparq + v_d_nor + v_d_sur + vgmitre + vlugano + vluro + vortuzar + vpuerr + vreal + vriachu +
               vsrita + vsoldati + vurquiza +
               dist_subte + dist_tren + dist_obe + count_delitos
               , data = terrenos_mergeado)

stargazer(modelo_ej4, type = "latex",
          title = "Resultados de la Regresión",
          label = "tab:resultados_regresion",
          dep.var.labels = c("Logaritmo del Precio en USD"),
          covariate.labels = c("Metros Cuadrados Totales", 
                               "Almagro", "Balvanera", "Barracas Este", "Barracas Oeste",
                               "Belgrano", "Boedo", "La Boca", "Caballito", 
                               "Chacarita", "Coghlan", "Colegiales", "Constitución", 
                               "Flores Norte", "Flores Sur", "Floresta", "Liniers", 
                               "Mataderos", "Monte Castro", "Monserrat", "Nueva Pompeya", 
                               "Núñez", "Palermo", "Parque Avellaneda", 
                               "Parque Chacabuco", "Parque Chas", "Parque Patricios", 
                               "Paternal", "Puerto Madero", "Recoleta", "Retiro", 
                               "Saavedra", "San Cristóbal", "San Nicolás", 
                               "San Telmo", "Villa Sarsfield", "Versalles", 
                               "Villa Crespo", "Villa del Parque", "Villa del Norte", 
                               "Villa del Sur", "Villa General Mitre", "Villa Lugano", 
                               "Villa Luro", "Villa Ortuzar", "Villa Pueyrredón", 
                               "Villa Real", "Villa Riachuelo", "Villa Santa Rita", 
                               "Villa Soldati", "Villa Urquiza"),
          out = "resultados_regresion_ej4.tex")
