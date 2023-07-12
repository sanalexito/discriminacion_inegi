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
#---- defino las variables de los nacionales y hago los iniciales ----
#total

t7$SEXO <- as.numeric(as.character(t7$SEXO))
f0 <- eval(parse(text = paste0("!t7$PM9_4_",1:8,"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0( "t7$PM9_4_",1:8,"%in%c('1')", collapse = "|")))
f2 <- eval(parse(text = paste0( "t7$PM9_4_",1:8,"%in%c('2')", collapse = "&")))
# f3_1<- eval(parse(text = paste0("  t7$PM9_4_",1:8,"%in%c('2','8')", collapse = "&")))
# f3 <- f3_1 & !f2 

t7$TOT <- ifelse(!t7$afro_1%in%0 , t7$FACTOR_PER, 0)  
t7$TOT_1 <- ifelse(!t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)  
t7$TOT_2 <- ifelse(!t7$afro_1%in%0 & f2%in%T, t7$FACTOR_PER, 0)  
t7$TOT_3 <- t7$TOT- t7$TOT_1- t7$TOT_2

etiquetas0 <- c("Sí experimentó situaciones de discriminación3", 
                "No experimentó situaciones de discriminación",
                "No especificado")
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:3))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#sexo
sexo <- c("Hombres", "Mujeres")
for(i in 1:2)eval(parse(text = paste0("
t7$TOT <- ifelse(!t7$afro_1%in%0 & t7$SEXO%in%",i,", t7$FACTOR_PER, 0)  
t7$TOT_1<-ifelse(!t7$afro_1%in%0 & t7$SEXO%in%",i," & f1%in%T, t7$FACTOR_PER, 0)  
t7$TOT_2<-ifelse(!t7$afro_1%in%0 & t7$SEXO%in%",i," & f2%in%T, t7$FACTOR_PER, 0)  
t7$TOT_3 <- t7$TOT- t7$TOT_1- t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:3))
",sexo[i],"_0 <- Tab_vert_a(xx, c(sexo[i], etiquetas0))
")))

#----columnas----
#primera
b <- c(1:8)
for(i in 1:length(b))eval(parse(text = paste0("
f0",i," <-!t7$PM9_5_1_",b[i],"%in%NA |!t7$PM9_5_2_",b[i],"%in%NA |!t7$PM9_5_3_",b[i],"%in%NA |!t7$PM9_5_4_",b[i],"%in%NA |
                        !t7$PM9_5_5_",b[i],"%in%NA | !t7$PM9_5_6_",b[i],"%in%NA | !t7$PM9_5_7_",b[i],"%in%NA | !t7$PM9_5_8_",b[i],"%in%NA
")))

for(i in 1:length(b))eval(parse(text = paste0("
f1",i,"<-t7$PM9_5_1_",b[i],"%in%'1' |t7$PM9_5_2_",b[i],"%in%'1' |t7$PM9_5_3_",b[i],"%in%'1' |t7$PM9_5_4_",b[i],"%in%'1' |
                       t7$PM9_5_5_",b[i],"%in%'1' |t7$PM9_5_6_",b[i],"%in%'1' |t7$PM9_5_7_",b[i],"%in%'1' |t7$PM9_5_8_",b[i],"%in%'1'
")))

t7$TOT <- ifelse(!t7$afro_1%in%0 & (f01 | f02 | f03 | f04 | f05 | f06 | f07 | f08), t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(!t7$afro_1%in%0 & f1",i,"%in%T, t7$FACTOR_PER, 0)
")))
t7$TOT_8<- ifelse(!t7$afro_1%in%0 & f18%in%T, t7$FACTOR_PER, 0)

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:8))
edu_1 <- Tab_vert_a(xx, c('Sí experimentó situaciones de discriminación3', 
                          'Por ser indígena',
                          'Por ser persona afrodescendiente',
                          'Por ser persona con discapacidad',
                          'Porque nació en el extranjero o cambió su lugar de residencia',
                          'Por su religión',
                          'Por su edad',
                          'Por ser mujer',
                          'Otra situación'))
#sexo
for(i in 1:length(b))eval(parse(text = paste0("
f0",i," <-!t7$PM9_5_1_",b[i],"%in%NA |!t7$PM9_5_2_",b[i],"%in%NA |!t7$PM9_5_3_",b[i],"%in%NA |!t7$PM9_5_4_",b[i],"%in%NA |!t7$PM9_5_5_",b[i],"%in%NA |
!t7$PM9_5_6_",b[i],"%in%NA |!t7$PM9_5_7_",b[i],"%in%NA |!t7$PM9_5_8_",b[i],"%in%NA 
")))

for(i in 1:length(b))eval(parse(text = paste0("
f1",i,"<-t7$PM9_5_1_",b[i],"%in%'1' |t7$PM9_5_2_",b[i],"%in%'1' |t7$PM9_5_3_",b[i],"%in%'1' |t7$PM9_5_4_",b[i],"%in%'1' |t7$PM9_5_5_",b[i],"%in%'1' |
t7$PM9_5_6_",b[i],"%in%'1' |t7$PM9_5_7_",b[i],"%in%'1' |t7$PM9_5_8_",b[i],"%in%'1'
")))

t7$TOT <- ifelse(!t7$afro_1%in%0 & t7$SEXO%in%'2' & (f01 | f02 | f03 | f04 | f05 | f06 | f07 | f08), t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(!t7$afro_1%in%0 & t7$SEXO%in%'2' & f1",i,"%in%T, t7$FACTOR_PER, 0)
")))
t7$TOT_8 <- ifelse(!t7$afro_1%in%0 & t7$SEXO%in%'2' & f18%in%T, t7$FACTOR_PER, 0)

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:8))
edu_Mujeres <- Tab_vert_a(xx, c('Sí experimentó situaciones de discriminación3', 
                                'Por ser indígena',
                                'Por ser persona afrodescendiente',
                                'Por ser persona con discapacidad',
                                'Porque nació en el extranjero o cambió su lugar de residencia',
                                'Por su religión',
                                'Por su edad',
                                'Por ser mujer',
                                'Otra situación'))
#---

t7$TOT <- ifelse(!t7$afro_1%in%0 & t7$SEXO%in%'1' & (f01 | f02 | f03 | f04 | f05 | f06 | f07 | f08), t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(!t7$afro_1%in%0 & t7$SEXO%in%'1' & f1",i,"%in%T, t7$FACTOR_PER, 0)")))
t7$TOT_7 <- 0.0000000001
#como solo se preguntó a muhjeres se debe poner NA en la de hombres.

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
edu_Hombres <- Tab_vert_a(xx, c('Sí experimentó situaciones de discriminación3', 
                                'Por ser indígena',
                                'Por ser persona afrodescendiente',
                                'Por ser persona con discapacidad',
                                'Porque nació en el extranjero o cambió su lugar de residencia',
                                'Por su religión',
                                'Por su edad',
                                'Por ser mujer',
                                'Otra situación'))

#---- Armado de los bloques ----
#armo bloques
nacional <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)
bloque1 <- pega_tab_1(tabla_0 = edu_1, tabla_1 = edu_Mujeres, tabla_2 = edu_Hombres)
#-------------------------------------------------------------------------------
tabulado <- purrr::map(.x =1:4, .f= ~rbind(nacional[[.x]][1,], nacional[[.x]][2,], bloque1[[.x]][2:9,],nacional[[.x]][3:4,])) 
tabulado[[3]] <- asteriscos_int_a(tabulado[[3]])

for(i in c(1,2,4)){
  tabulado[[i]][9, 8:9] <- "NA"
}

tabulado[[3]][9,c(14,15, 17, 18)] <- "NA"
#-------------------------------------------------------------------------------
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 29
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])