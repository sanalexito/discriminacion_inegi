library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
bases <- list()
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===============================================================================
estois <- names(bases[[1]])[5:153]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A'), 1, 0)

t7$EDAD <- as.numeric(as.character(t7$EDAD))
#-------------------------------------------------------------------------------
etiquetas <- c('Una autoridad del lugar (clínica, trabajo, oficina, autoridad comunitaria)',
               'El Ministerio Público, la policía o gobierno',
               'La Comisión Nacional o Estatal de Derechos Humanos',
               'El CONAPRED',
               'Otra instancia')


#---- Primera col ----
f0 <- t7$PM9_2%in%c('1',2,4,3,5)
t7$TOT <- ifelse(!t7$marca_indi%in%0 & t7$PM9_2%in%c('1',2,4,3,5) , t7$FACTOR_PER, 0)
for(i in c(1:3, 5)) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%",i,", t7$FACTOR_PER, 0)")))
t7$TOT_4 <- 0.0000000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:5))
tab1_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))

#---- Mujeres ----
t7$TOT <- ifelse(!t7$marca_indi%in%0 & t7$PM9_2%in%c('1',2,4,3,5) & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in c(1:3, 5)) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
t7$TOT_4 <- 0.0000000001
t7$TOT_5 <- 0.0000000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:5))
tab1_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))


#---- Hombres ----
t7$TOT <- ifelse(!t7$marca_indi%in%0 & t7$PM9_2%in%c('1',2,4,3,5) & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in c(1:3, 5)) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
t7$TOT_4 <- 0.0000000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:5))
tab1_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))

#-------------------------------------------------------------------------------
tabulado0 <- pega_tab_1(tabla_0 = tab1_1, tabla_1 = tab1_2, tabla_2 = tab1_3)
tabulado0[[3]] <- asteriscos_int_a(tabulado0[[3]])

#Parche para poner a los que si denunciaron
f0 <- eval(parse(text = paste0("t7$PM9_1_",1:8,"%in%'1'", collapse = "|")))

etiquetas <- c('Sí denunció', 'No lo informó')
t7$TOT <- ifelse(!t7$marca_indi%in%0 & f0%in%T , t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%c('1',2,3,5), t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%'6', t7$FACTOR_PER, 0)

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
bla <- Tab_vert_a(xx, c("Estados Unidos Mexicanos", etiquetas))

#---- Mujeres ----
t7$TOT <- ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T , t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%c('1',2,3,5), t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%'6', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c("Mujeres", etiquetas))


#---- Hombres ----
t7$TOT <- ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T , t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%c('1',2,3,5), t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T & t7$PM9_2%in%'6', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c("Hombres", etiquetas))



tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#-------------------------------------------------------------------------------
tabulado <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3, ]) )
for(i in c(1)){
tabulado[[i]][6,c(2,3)] <- "0*"
tabulado[[i]][6,c(3)] <- "0.0*"

tabulado[[i]][6,c(5,8)] <- "0*"
tabulado[[i]][6,c(6,9)] <- "0.0*"

tabulado[[i]][7,c(5)] <- "0*"
tabulado[[i]][7,c(6)] <- "0.0*"

}
for(i in c(2,4)){
  tabulado[[i]][6,c(2,3)] <- NA
  tabulado[[i]][6,c(3)] <- NA
  
  tabulado[[i]][6,c(5,8)] <- NA
  tabulado[[i]][6,c(6,9)] <- NA
  
  tabulado[[i]][7,c(5)] <- NA
  tabulado[[i]][7,c(6)] <- NA
}
tabulado[[3]][6, c(2,3,5,6,8,9,11,12,14,15,17,18)] <-"NA"
tabulado[[3]][7, c(8,9,11,12)] <-"NA"

#-------------------------------------------------------------------------------
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 8
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])