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

f2 <- eval(parse(text = paste0("!t7$PO4_2_",c(1:7) ,"%in%NA", collapse = "|")))
t7$TOT <- ifelse(f2%in%T, t7$FACTOR_PER, 0)

f2 <- eval(parse(text = paste0("t7$PO4_2_",c(1:7) ,"%in%'1'", collapse = "|")))
t7$TOT_1 <- ifelse(f2%in%T, t7$FACTOR_PER, 0)
#Tasas totales
xx <- c('TOT', paste0('TOT_',1:1))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_22 <- Tab_2(xx, "ENT_a", estados)
#-------------------------------------------------------------------------------

source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 84
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])
