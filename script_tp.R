
#descargo paquetes
rm(list=ls()) # remueve toda informacion
library(sf) # trabaja con objetos espaciales
library(tidyverse) # limpiar datos 
require(gridExtra)

#importo shapefile
terrenos <- read.csv("C:/Users/sofia/Desktop/Maestría/Optativas/Segundo trimestre/Econometría Espacial/TP Espacial/Terrenos-en-venta-2019.csv")

#paso los nombres de las variables a minúscula 
terrenos <- terrenos %>% 
  rename_all(tolower)
print(colnames(terrenos))

#renombro 2 variables a menos de 10 caracteres
terrenos <- terrenos %>%
  rename(preciusdm = preciousdm)
terrenos <- terrenos %>%
  rename(preciarsm = precioarsm)

#usamos como proyección POSGAR 2017 - 6 (EPSG = 5348)
class(terrenos) 


# Suponiendo que tu data frame se llama `mi_base_datos` y la variable `mi_variable`
terrenos$longitud<- as.numeric(terrenos$longitud)
terrenos$latitud<- as.numeric(terrenos$latitud)

#vemos los tipos de variable que tenemos
sapply(terrenos, class)
#cambio valor erróneo
terrenos[813, "longitud"] <- -58.480852

#creo variable logaritmo neperiano precio en dolares 
terrenos1 <- terrenos %>%
  mutate(lnpriceusd = log(preciousd))

#corrijo error tipográfico
terrenos1$barrios <- ifelse(terrenos$barrios == "NU├æEZ", "NUÑEZ", terrenos$barrios)

#creo dummies por barrio
table(terrenos1$barrios) #veo cantidad de terrenos por barrio 
length(unique(terrenos1$barrios)) #cuento cuántos barrios hay
terrenos1 <- terrenos1 %>%
  mutate(
    agronomia = ifelse(barrios == "AGRONOMIA", 1, 0),
    almagro = ifelse(barrios == "ALMAGRO", 1, 0),
    balvanera = ifelse(barrios == "BALVANERA", 1, 0),
    barrac_e = ifelse(barrios == "BARRACAS ESTE", 1, 0),
    barrac_o = ifelse(barrios == "BARRACAS OESTE", 1, 0),
    belgrano = ifelse(barrios == "BELGRANO", 1, 0),
    boedo = ifelse(barrios == "BOEDO", 1, 0),
    boca = ifelse(barrios == "BOCA", 1, 0),
    caballito = ifelse(barrios == "CABALLITO", 1, 0),
    chacarita = ifelse(barrios == "CHACARITA", 1, 0),
    coghlan = ifelse(barrios == "COGHLAN", 1, 0),
    colegial = ifelse(barrios == "COLEGIALES", 1, 0),
    constitu = ifelse(barrios == "CONSTITUCION", 1, 0),
    fl_norte = ifelse(barrios == "FLORES NORTE", 1, 0),
    fl_sur = ifelse(barrios == "FLORES SUR", 1, 0),
    floresta = ifelse(barrios == "FLORESTA", 1, 0),
    liniers = ifelse(barrios == "LINIERS", 1, 0),
    mataderos = ifelse(barrios == "MATADEROS", 1, 0),
    mt_castro = ifelse(barrios == "MONTE CASTRO", 1, 0),
    montserr = ifelse(barrios == "MONTSERRAT", 1, 0),
    nva_pomp = ifelse(barrios == "NUEVA POMPEYA", 1, 0),
    nunez = ifelse(barrios == "NUÑEZ", 1, 0),
    palermo = ifelse(barrios == "PALERMO", 1, 0),
    p_avell = ifelse(barrios == "PARQUE AVELLANEDA", 1, 0),
    p_chacab = ifelse(barrios == "PARQUE CHACABUCO", 1, 0),
    p_chas = ifelse(barrios == "PARQUE CHAS", 1, 0),
    p_patric = ifelse(barrios == "PARQUE PATRICIOS", 1, 0),
    paternal = ifelse(barrios == "PATERNAL", 1, 0),
    p_mader = ifelse(barrios == "PUERTO MADERO", 1, 0),
    recoleta = ifelse(barrios == "RECOLETA", 1, 0),
    retiro = ifelse(barrios == "RETIRO", 1, 0),
    saavedra = ifelse(barrios == "SAAVEDRA", 1, 0),
    san_cris = ifelse(barrios == "SAN CRISTOBAL", 1, 0),
    san_nico = ifelse(barrios == "SAN NICOLAS", 1, 0),
    san_telmo = ifelse(barrios == "SAN TELMO", 1, 0),
    vl_sars = ifelse(barrios == "VELEZ SARSFIELD", 1, 0),
    versalles = ifelse(barrios == "VERSALLES", 1, 0),
    vl_cresp = ifelse(barrios == "VILLA CRESPO", 1, 0),
    vl_d_park = ifelse(barrios == "VILLA DEL PARQUE", 1, 0),
    vl_d_nor = ifelse(barrios == "VILLA DEVOTO NORTE", 1, 0),
    vl_d_sur = ifelse(barrios == "VILLA DEVOTO SUR", 1, 0),
    vl_g_mtr = ifelse(barrios == "VILLA GRAL. MITRE", 1, 0),
    vl_lgn = ifelse(barrios == "VILLA LUGANO", 1, 0),
    vl_luro = ifelse(barrios == "VILLA LURO", 1, 0),
    vl_ortuzar = ifelse(barrios == "VILLA ORTUZAR", 1, 0),
    vl_puerr = ifelse(barrios == "VILLA PUEYRREDON", 1, 0),
    vl_real = ifelse(barrios == "VILLA REAL", 1, 0),
    vl_riachu = ifelse(barrios == "VILLA RIACHUELO", 1, 0),
    vl_st_rit = ifelse(barrios == "VILLA SANTA RITA", 1, 0),
    vl_soldat = ifelse(barrios == "VILLA SOLDATI", 1, 0),
    vl_urqza = ifelse(barrios == "VILLA URQUIZA", 1, 0)
  )


#modelo regresion lineal 
modelo <- lm(lnpriceusd ~ m2total + almagro + balvanera + barrac_e + barrac_o +
               belgrano + boedo + boca + caballito + chacarita + coghlan + colegial + constitu +
               fl_norte + fl_sur + floresta + liniers + mataderos + mt_castro + 
               montserr + nva_pomp + nunez + palermo + p_avell + 
               p_chacab + p_chas + p_patric + paternal + p_mader + 
               recoleta + retiro + saavedra + san_cris + san_nico + san_telmo + 
               vl_sars + versalles + vl_cresp + vl_d_park + vl_d_nor + 
               vl_d_sur + vl_g_mtr + vl_lgn + vl_luro + vl_ortuzar + 
               vl_puerr + vl_real + vl_riachu + vl_st_rit + 
               vl_soldat + vl_urqza, data = terrenos1)


#resumen del modelo
summary(modelo)

write.csv(terrenos1,"C:/Users/sofia/Desktop/Maestría/Optativas/Segundo trimestre/Econometría Espacial/TP Espacial/dataset2.csv", row.names = FALSE)


