library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
bases <- list()
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===============================================================================
estois <- names(bases[[1]])[5:159]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c('A'),1,0)
#-------------------------------------------------------------------------------
etiquetas0 <- c("Ha tenido algún problema",
                  "Solo está en Internet (carece de acceso)",
                  "Se le dificulta usar el equipo (computadora, cajeros automáticos)",
                  "Las oficinas quedan lejos",
                  "Le niegan la información, no le explican",
                  "Desconoce dónde buscarla",
                  "Oficinas cerradas o trámites parados por la pandemia",
                  "Otro")
#-------------------------------------------------------------------------------

f1 <- eval(parse(text = paste0("t7$PM1_13_",1:8,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(t7$afro_1%in%1 & f1%in%T, t7$FACTOR_PER, 0)
f2 <- eval(parse(text = paste0("t7$PM1_13_",1:7,"%in%'1'", collapse = "|")))
t7$TOT_1 <- ifelse(t7$afro_1%in%1 & f2%in%T, t7$FACTOR_PER,0)
t7$TOT_2 <- ifelse(t7$afro_1%in%1 & t7$PM1_13_8%in%'1', t7$FACTOR_PER,0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab1_1 <- Tab_vert_a(xx, c("Ha buscado información", 'Ha tenido algún problema', 'No ha tenido ningún problema'))

f1 <- eval(parse(text = paste0("t7$PM1_13_",1:8,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(t7$SEXO%in%'2' & t7$afro_1%in%1 & f1%in%T, t7$FACTOR_PER, 0)
f2 <- eval(parse(text = paste0("t7$PM1_13_",1:7,"%in%'1'", collapse = "|")))
t7$TOT_1 <- ifelse(t7$SEXO%in%'2' & t7$afro_1%in%1 & f2%in%T, t7$FACTOR_PER,0)
t7$TOT_2 <- ifelse(t7$SEXO%in%'2' & t7$afro_1%in%1 & t7$PM1_13_8%in%'1', t7$FACTOR_PER,0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab1_2 <- Tab_vert_a(xx, c("Ha buscado información", 'Ha tenido algún problema', 'No ha tenido ningún problema'))

f1 <- eval(parse(text = paste0("t7$PM1_13_",1:8,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(t7$SEXO%in%'1' & t7$afro_1%in%1 & f1%in%T, t7$FACTOR_PER, 0)
f2 <- eval(parse(text = paste0("t7$PM1_13_",1:7,"%in%'1'", collapse = "|")))
t7$TOT_1 <- ifelse(t7$SEXO%in%'1' & t7$afro_1%in%1 & f2%in%T, t7$FACTOR_PER,0)
t7$TOT_2 <- ifelse(t7$SEXO%in%'1' & t7$afro_1%in%1 & t7$PM1_13_8%in%'1', t7$FACTOR_PER,0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab1_3 <- Tab_vert_a(xx, c("Ha buscado información", 'Ha tenido algún problema', 'No ha tenido ningún problema'))


#-------------------------------------------------------------------------------
f1 <- eval(parse(text = paste0("t7$PM1_13_",1:7,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(t7$afro_1%in%1 & f1, t7$FACTOR_PER, 0)
for(i in 1:7) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_13_",i,"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:7))
tab2_1 <- Tab_vert_a(xx, c( etiquetas0))

#col 2 y3 
f1 <- eval(parse(text = paste0("t7$PM1_13_",1:7,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(t7$SEXO%in%'2' & t7$afro_1%in%1 & f1, t7$FACTOR_PER, 0)
for(i in 1:7) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$afro_1%in%1 & t7$PM1_13_",i,"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:7))
tab2_2 <- Tab_vert_a(xx, c( etiquetas0))

f1 <- eval(parse(text = paste0("t7$PM1_13_",1:7,"%in%'1'", collapse = "|")))
t7$TOT <- ifelse(t7$SEXO%in%'1' & t7$afro_1%in%1 & f1, t7$FACTOR_PER, 0)
for(i in 1:7) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$afro_1%in%1 & t7$PM1_13_",i,"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:7))
tab2_3 <- Tab_vert_a(xx, c( etiquetas0))

#-------------------------------------------------------------------------------
si_y_no <- c('Ha buscado información', 'Nunca ha buscado información')
t7$TOT <- ifelse(t7$afro_1%in%1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(t7$afro_1%in%1 & t7$PM1_13_9%in%1, t7$FACTOR_PER, 0)
t7$TOT_1 <- t7$TOT-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab3_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", si_y_no))

t7$TOT <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(t7$afro_1%in%1 & t7$PM1_13_9%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <- t7$TOT-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab3_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", si_y_no))

t7$TOT <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(t7$afro_1%in%1 & t7$PM1_13_9%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <- t7$TOT-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab3_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", si_y_no))
#-------------------------------------------------------------------------------
tabulado1 <- pega_tab_1(tabla_0 = tab1_1, tabla_1 = tab1_2, tabla_2 = tab1_3)
tabulado2 <- pega_tab_1(tabla_0 = tab2_1, tabla_1 = tab2_2, tabla_2 = tab2_3)
tabulado3 <- pega_tab_1(tabla_0 = tab3_1, tabla_1 = tab3_2, tabla_2 = tab3_3)

#-------------------------------------------------------------------------------
tabulado <- purrr::map(.x = 1:4, .f=~rbind(tabulado3[[.x]][1:2, ],
                                           tabulado1[[.x]][2, ],
                                           tabulado2[[.x]][2:dim(tabulado2[[.x]])[1],],
                                           tabulado1[[.x]][3,],
                                           tabulado3[[.x]][3, ]) )
#-------------------------------------------------------------------------------

source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 21
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])