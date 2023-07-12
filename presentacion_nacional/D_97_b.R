library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
bases <- list()
bases[[1]] <- TAB[[10]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===============================================================================
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$religion <- ifelse(t7$P4_2_04%in%c('A'), 1, 0)
#===============================================================================
etiquetas <- c("No sabía que podía hacerlo o dónde reportarlo",
               "Por temor",
               "Piensa que no le harían caso o es pérdida de tiempo",
               "No tenía importancia",
               "Porque se siente menos",
               "Otro")
#---- Primera col ----
f0 <- eval(parse(text = paste0("t7$PM9_1_",1:8,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(!t7$religion%in%0 & t7$PM9_2%in%'6' & f0%in%T  , t7$FACTOR_PER, 0)
for(i in c(1:6)) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(!t7$religion%in%0 & f0%in%T & t7$PM9_3%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:6))
tab1_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))

#---- Mujeres ----
t7$TOT <- ifelse(!t7$religion%in%0 & t7$PM9_2%in%'6' &  f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in c(1:6)) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(!t7$religion%in%0 & f0%in%T & t7$PM9_3%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:6))
tab1_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))


#---- Hombres ----
t7$TOT <- ifelse(!t7$religion%in%0 & t7$PM9_2%in%'6' &  f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in c(1:6)) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(!t7$religion%in%0 & f0%in%T & t7$PM9_3%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:6))
tab1_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))

#-------------------------------------------------------------------------------
tabulado <- pega_tab_1(tabla_0 = tab1_1, tabla_1 = tab1_2, tabla_2 = tab1_3)
#-------------------------------------------------------------------------------
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 66
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])