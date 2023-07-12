library(foreign)
library(survey)
source('D:/ENADIS/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
t7 <- TAB[[16]]
#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
t7 <- t7[t7$RESUL_POB%in%c('A'), ]

t7$ENT_a <- as.numeric(as.character(t7$ENT))
t7$EDAD_a <- as.numeric(as.character(t7$EDAD))

estados <- c("Estados Unidos Mexicanos","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza","Colima","Chiapas","Chihuahua",
             "Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Estado de México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León",
             "Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")

#-------------------------------------------------------------------------------

f00 <- eval(parse(text = paste0("!t7$PO4_1_0",1:9 ,"%in%NA", collapse = "|")))
f01 <- eval(parse(text = paste0("!t7$PO4_1_",10:16,"%in%NA", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT <- ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)

f00 <- eval(parse(text = paste0("t7$PO4_1_0",1:9 ,"%in%'1'", collapse = "|")))
f01 <- eval(parse(text = paste0("t7$PO4_1_",10:16,"%in%'1'", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT_1 <- ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)

#Tasas totales
xx <- c('TOT', paste0('TOT_',1:1))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_22 <- Tab_2(xx, "ENT_a", estados)

#===============================================================================
Escribe_Excel_a(ruta, 82, tab_22[[1]])
