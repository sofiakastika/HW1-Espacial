
#descargo paquetes
rm(list=ls()) # remueve toda informacion
library(sf) # trabaja con objetos espaciales
library(tidyverse) # limpiar datos 
require(gridExtra)
library(Hmisc)
library(stargazer)

#Importo shapefile
terrenos <- read.csv("https://raw.githubusercontent.com/sofiakastika/HW1-Espacial/main/Terrenos-en-venta-2019.csv")


#Paso los nombres de las variables a minúscula 
terrenos1 <- terrenos %>% 
  rename_all(tolower)
print(colnames(terrenos))

#Veo cuántos barrios hay
length(unique(terrenos$BARRIO))

#Creo variable logaritmo neperiano precio en dolares 
terrenos1 <- terrenos1 %>%
  mutate(lnprecio = log(preciousd))

#Creo dummies por barrio
terrenos1 <- terrenos1 %>%
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
    nunez = ifelse(barrio == "NUNEZ", 1, 0),
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
    v_sars = ifelse(barrio == "VELEZ SARSFIELD", 1, 0),
    versalles = ifelse(barrio == "VERSALLES", 1, 0),
    vcrespo = ifelse(barrio == "VILLA CRESPO", 1, 0),
    vdelparq = ifelse(barrio == "VILLA DEL PARQUE", 1, 0),
    v_d_nor = ifelse(barrio == "VILLA DEVOTO NORTE", 1, 0),
    v_d_sur = ifelse(barrio == "VILLA DEVOTO SUR", 1, 0),
    vgmitre = ifelse(barrio == "VILLA GRAL. MITRE", 1, 0),
    vlugano = ifelse(barrio == "VILLA LUGANO", 1, 0),
    vluro = ifelse(barrio == "VILLA LURO", 1, 0),
    vortuzar = ifelse(barrio == "VILLA ORTUZAR", 1, 0),
    vpuerr = ifelse(barrio == "VILLA PUEYRREDON", 1, 0),
    vreal = ifelse(barrio == "VILLA REAL", 1, 0),
    vriachu = ifelse(barrio == "VILLA RIACHUELO", 1, 0),
    vsrita = ifelse(barrio == "VILLA SANTA RITA", 1, 0),
    vsoldati = ifelse(barrio == "VILLA SOLDATI", 1, 0),
    vurquiza = ifelse(barrio == "VILLA URQUIZA", 1, 0)
  )

#Modelo regresion lineal 
names(terrenos1)
modelo <- lm(lnprecio ~ m2total + almagro + balvanera + barrac_e + barrac_o + belgrano + boedo + boca + caballito + chacarita +
                coghlan + colegial + constitu + fl_norte + fl_sur + floresta + liniers + mataderos + mt_castro +
                montserr + nva_pomp + nunez + palermo + p_avell + p_chacab + p_chas + p_patric + paternal + p_mader +
                recoleta + retiro + saavedra + san_cris + san_nico + san_telmo + v_sars + versalles + vcrespo +
                vdelparq + v_d_nor + v_d_sur + vgmitre + vlugano + vluro + vortuzar + vpuerr + vreal + vriachu +
                vsrita + vsoldati + vurquiza, data = terrenos1)

#resumen del modelo
summary(modelo)

# Asigno etiquetas a las variables
label(terrenos1$lnprecio) <- "Logaritmo del precio en USD"
label(terrenos1$m2total) <- "Metros cuadrados totales"

# Vector con nombres de las variables dummy
barrios <- c("almagro", "balvanera", "barrac_e", "barrac_o", "belgrano", 
             "boedo", "boca", "caballito", "chacarita", "coghlan", 
             "colegial", "constitu", "fl_norte", "fl_sur", "floresta", 
             "liniers", "mataderos", "mt_castro", "montserr", "nva_pomp", 
             "nunez", "palermo", "p_avell", "p_chacab", "p_chas", "p_patric", 
             "paternal", "p_mader", "recoleta", "retiro", "saavedra", 
             "san_cris", "san_nico", "san_telmo", "v_sars", "versalles", 
             "vcrespo", "vdelparq", "v_d_nor", "v_d_sur", "vgmitre", 
             "vlugano", "vluro", "vortuzar", "vpuerr", "vreal", 
             "vriachu", "vsrita", "vsoldati", "vurquiza")

# Vector con las etiquetas que solo incluyen el nombre del barrio
etiquetas_barrios <- c("Almagro", "Balvanera", "Barracas Este", "Barracas Oeste", 
                       "Belgrano", "Boedo", "La Boca", "Caballito", 
                       "Chacarita", "Coghlan", "Colegiales", "Constitución", 
                       "Flores Norte", "Flores Sur", "Floresta", "Liniers", 
                       "Mataderos", "Monte Castro", "Monserrat", "Nueva Pompeya", 
                       "Núñez", "Palermo", "Parque Avellaneda", 
                       "Parque Chacabuco", "Parque Chas", "Parque Patricios", 
                       "Paternal", "Puerto Madero", "Recoleta", "Retiro", 
                       "Saavedra", "San Cristóbal", "San Nicolás", 
                       "San Telmo", "Vélez Sarsfield", "Versalles", 
                       "Villa Crespo", "Villa del Parque", "Villa Devoto Norte", 
                       "Villa Devoto Sur", "Villa General Mitre", "Villa Lugano", 
                       "Villa Luro", "Villa Ortuzar", "Villa Pueyrredón", 
                       "Villa Real", "Villa Riachuelo", "Villa Santa Rita", 
                       "Villa Soldati", "Villa Urquiza")

# Loop para asignar las etiquetas a las variables
for (i in seq_along(barrios)) {
  label(terrenos1[[barrios[i]]]) <- etiquetas_barrios[i]
}

stargazer(modelo, type = "latex",
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
          out = "resultados_regresion.tex")


# Guardo los residuos del modelo
residuos <- modelo$residuals

# Añadir los residuos al data frame
terrenos1$residuos <- residuos

write.csv(terrenos1, "C:/Users/judit/OneDrive - Económicas - UBA/Documentos/GitHub/HW1-Espacial/terrenos1_con_residuos.csv", row.names = FALSE)


#Descargo nueva base .csv
write.csv(terrenos1,"C:/Users/sofia/Desktop/Maestría/Optativas/Segundo trimestre/Econometría Espacial/TP Espacial/dataset1.csv", row.names = FALSE)



