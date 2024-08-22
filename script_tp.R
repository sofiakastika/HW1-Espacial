
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
terrenos <- read.csv("https://raw.githubusercontent.com/sofiakastika/HW1-Espacial/main/Terrenos-en-venta-2019.csv")

#Paso los nombres de las variables a minúscula 
terrenos1 <- terrenos %>% 
  rename_all(tolower)

#Creo variable logaritmo del precio en dolares 
terrenos1 <- terrenos1 %>%
  mutate(lnprecio = log(preciousd))

#Creo variable logaritmo de metros cuadrados 
terrenos1 <- terrenos1 %>%
  mutate(lnm2total = log(m2total))

#Elimino las observaciones con m2total=0
terrenos1 <- terrenos1[terrenos1$m2total != 0, ]

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

#Saco a latex tabla sin coeficientes de barrio
stargazer(modelo2, modelo1, 
          type = "latex",
          dep.var.labels = c("Logaritmo precio en dólares"),
          covariate.labels = c("Logaritmo metros cuadrados totales", "FE por barrio"),
          omit.stat = c("ser", "f", "adj.rsq"),
          omit = "barrio",  # Omite todos los coeficientes que comienzan con 'barrio'
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

#Elimino las observaciones con m2total=0
terrenos_2 <- terrenos_2[terrenos_2$M2TOTAL != 0, ]

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

# Modelo agregando vars ej 3
modelo3 <- lm(lnprecio ~ lnm2total + barrio +
                dist_subte + dist_tren + dist_obe + count_delitos, data = terrenos_mergeado)
summary(modelo3) #resumen del modelo
n_obs3 <- nobs(modelo3) #cantidad de observaciones en el modelo
print(n_obs3)

# Saco a latex tabla sin coeficientes de barrio
stargazer(modelo3, 
          type = "latex",
          dep.var.labels = c("Precio (log USD)"),
          covariate.labels = c("M2 totales (log)", "Distancia subte", "Dist. tren", "Dist. obelisco", "Delitos"),
          omit = "barrio",  # Omite todos los coeficientes que comienzan con 'barrio'
          omit.stat = c("ser", "f", "adj.rsq"),
          align = TRUE,
          no.space = TRUE,
          title = "")

# Guardo los residuos del modelo
Residuos_Ej_4 <- modelo3$residuals

# Añadir los residuos al data frame
terrenos_mergeado$Residuos_Ej_4 <- Residuos_Ej_4

#Descargo nueva base .csv
write.csv(terrenos_mergeado, 
          "/Users/tomasmarotta/Documents/GitHub/HW1-Espacial/terrenos_mergeado_resid.csv", 
          row.names = FALSE)

# Ejercicio 6
terrenos_3 <- read.csv("/Users/tomasmarotta/Documents/GitHub/HW1-Espacial/terrenos_final.csv")

# Modelo con outliers LH (no encontramos HL)
modelo4 <- lm(lnprecio ~ lnm2total + barrio +
                dist_subte + dist_tren + dist_obe + count_delitos+
                Outliers_LH, data = terrenos_3)
summary(modelo4) #resumen del modelo
n_obs4 <- nobs(modelo4) #cantidad de observaciones en el modelo
print(n_obs4)

# Saco a latex tabla sin coeficientes de barrio
stargazer(modelo4, 
          type = "latex",
          dep.var.labels = c("Precio (log USD)"),
          covariate.labels = c("M2 totales (log)", "Dist. subte", "Dist. tren", "Dist. obelisco", "Delitos"),
          omit = "barrio",  # Omite todos los coeficientes que comienzan con 'barrio'
          omit.stat = c("ser", "f", "adj.rsq"),
          align = TRUE,
          no.space = TRUE,
          title = "")

#creo dummies por barrio
table(terrenos_3$barrio) #veo cantidad de terrenos por barrio 
length(unique(terrenos_3$barrio)) #cuento cuántos barrio hay
terrenos_3 <- terrenos_3 %>%
  mutate(
    agronomia = ifelse(barrio == "AGRONOMIA", 1, 0),
    almagro = ifelse(barrio == "ALMAGRO", 1, 0),
    balvanera = ifelse(barrio == "BALVANERA", 1, 0),
    barrac_e = ifelse(barrio == "BARRACAS ESTE", 1, 0),
    barrac_o = ifelse(barrio == "BARRACAS OESTE", 1, 0),
    belgrano = ifelse(barrio == "BELGRANO", 1, 0),
    boedo = ifelse(barrio == "BOEDO", 1, 0),
    boca = ifelse(barrio == "BOCA", 1, 0),
    caballito = ifelse(barrio == "CABALLITO", 1, 0),
    chacarita = ifelse(barrio == "CHACARITA", 1, 0),
    coghlan = ifelse(barrio == "COGHLAN", 1, 0),
    colegial = ifelse(barrio == "COLEGIALES", 1, 0),
    constitu = ifelse(barrio == "CONSTITUCION", 1, 0),
    fl_norte = ifelse(barrio == "FLORES NORTE", 1, 0),
    fl_sur = ifelse(barrio == "FLORES SUR", 1, 0),
    floresta = ifelse(barrio == "FLORESTA", 1, 0),
    liniers = ifelse(barrio == "LINIERS", 1, 0),
    mataderos = ifelse(barrio == "MATADEROS", 1, 0),
    mt_castro = ifelse(barrio == "MONTE CASTRO", 1, 0),
    montserr = ifelse(barrio == "MONTSERRAT", 1, 0),
    nva_pomp = ifelse(barrio == "NUEVA POMPEYA", 1, 0),
    nunez = ifelse(barrio == "NUÑEZ", 1, 0),
    palermo = ifelse(barrio == "PALERMO", 1, 0),
    p_avell = ifelse(barrio == "PARQUE AVELLANEDA", 1, 0),
    p_chacab = ifelse(barrio == "PARQUE CHACABUCO", 1, 0),
    p_chas = ifelse(barrio == "PARQUE CHAS", 1, 0),
    p_patric = ifelse(barrio == "PARQUE PATRICIOS", 1, 0),
    paternal = ifelse(barrio == "PATERNAL", 1, 0),
    p_mader = ifelse(barrio == "PUERTO MADERO", 1, 0),
    recoleta = ifelse(barrio == "RECOLETA", 1, 0),
    retiro = ifelse(barrio == "RETIRO", 1, 0),
    saavedra = ifelse(barrio == "SAAVEDRA", 1, 0),
    san_cris = ifelse(barrio == "SAN CRISTOBAL", 1, 0),
    san_nico = ifelse(barrio == "SAN NICOLAS", 1, 0),
    san_telmo = ifelse(barrio == "SAN TELMO", 1, 0),
    vl_sars = ifelse(barrio == "VELEZ SARSFIELD", 1, 0),
    versalles = ifelse(barrio == "VERSALLES", 1, 0),
    vl_cresp = ifelse(barrio == "VILLA CRESPO", 1, 0),
    vl_d_park = ifelse(barrio == "VILLA DEL PARQUE", 1, 0),
    vl_d_nor = ifelse(barrio == "VILLA DEVOTO NORTE", 1, 0),
    vl_d_sur = ifelse(barrio == "VILLA DEVOTO SUR", 1, 0),
    vl_g_mtr = ifelse(barrio == "VILLA GRAL. MITRE", 1, 0),
    vl_lgn = ifelse(barrio == "VILLA LUGANO", 1, 0),
    vl_luro = ifelse(barrio == "VILLA LURO", 1, 0),
    vl_ortuzar = ifelse(barrio == "VILLA ORTUZAR", 1, 0),
    vl_puerr = ifelse(barrio == "VILLA PUEYRREDON", 1, 0),
    vl_real = ifelse(barrio == "VILLA REAL", 1, 0),
    vl_riachu = ifelse(barrio == "VILLA RIACHUELO", 1, 0),
    vl_st_rit = ifelse(barrio == "VILLA SANTA RITA", 1, 0),
    vl_soldat = ifelse(barrio == "VILLA SOLDATI", 1, 0),
    vl_urqza = ifelse(barrio == "VILLA URQUIZA", 1, 0)
  )

#Descargo nueva base .csv
write.csv(terrenos_3, 
          "/Users/tomasmarotta/Documents/GitHub/HW1-Espacial/terrnos_final_final.csv", 
          row.names = FALSE)
