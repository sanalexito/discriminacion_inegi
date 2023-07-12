library(foreign)
library(survey)
source('D:/ENADIS/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
bases <- list()

#INDIGENAS 12 a 17 ######################################################################
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===
estois <- names(bases[[1]])[5:153]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A') & t7$EDAD%in%c('12',13:17), 1, 0)


t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#Parche para acompletar indigenas 
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_indigenas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#AFRODESCENDIENTES 12 a 17 ##############################################################
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:159]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c("A") & t7$EDAD%in%c('12',13:17),1,0)

#Parche para acompletar afrodescendientes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
t7$TOT_3 <- 0.0000000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_afrodescendientes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#DISCAPACIDAD 12 a 17 ###################################################################
bases[[1]] <- TAB[[7]]
bases[[2]] <- TAB[[2]]
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:160]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois,  "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A') & t7$EDAD%in%c('12',13:17), 1, 0)
sum(t7$discapacidad)
# -> Población verificada con discapacidad
#t7$POBF1_1 <- ifelse( t7$FILTRO_2%in%c('1', '2') & t7$PMVD_1%in%'1',1,0)
t7$POBF1_1 <- t7$discapacidad
t7$EDAD <- as.numeric(as.character(t7$EDAD))

#Parche para acompletar discapacidad
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_discapacidad <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MIGRANTES de 12 a 17######################################################################
bases[[1]] <- TAB[[9]]
bases[[2]] <- TAB[[2]]
#Migrantes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:151]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$migrante <- ifelse(t7$P4_2_03%in%c('A') & t7$EDAD%in%c('12',13:17), 1, 0)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---

#Parche para acompletar migrantes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_migrantes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#RELIGIÖN de 12 a 17 ##################################################################
bases[[1]] <- TAB[[10]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$religion <- ifelse(t7$P4_2_04%in%c('A') & t7$EDAD%in%c('12',13:17), 1, 0)
#sum(t7$religion)
#---
#Parche para acompletar religión
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_religion<- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#MAYORES DE 60########################################################################
# bases[[1]] <- TAB[[12]]
# bases[[2]] <- TAB[[2]]
# #mayor
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
# #---
# estois <- names(bases[[1]])[5:145]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$mayor <- ifelse(t7$P4_2_05%in%c('A')  & t7$EDAD%in%c('12',13:17), 1, 0)
# #sum(t7$mayor)
# 
# #---
# #Parche para acompletar 
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(!t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(!t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# 
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

#tab_mayores <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
tab_mayores <- NA

#Adolescentes###################################################################
bases[[1]] <- TAB[[14]]
bases[[2]] <- TAB[[2]]
#adolesc
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:171]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$adolesc <- ifelse(t7$P4_2_07%in%c('A') & t7$EDAD%in%c('12', 13:17), 1, 0)
#sum(t7$adolesc)
#---
#Parche para acompletar 
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$adolesc%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_adolescentes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MUJERES########################################################################
# 
# bases[[1]] <- TAB[[15]]
# bases[[2]] <- TAB[[2]]
# #mujer
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
# #---
# estois <- names(bases[[1]])[5:163]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$mujer <- ifelse(t7$P4_2_08%in%c('A') & t7$EDAD%in%c('12', 13:17), 1, 0)
# #---
# #Parche para acompletar mujeres
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$mujer%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(!t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(!t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_mujeres <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
tab_mujeres <- NA

#Mujeres TRABAJDORAS DEL HOGAR##################################################
#--- 
# muj_remunerada <- t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & t7$EDAD%in%c('12', 13:17)
# 
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(muj_remunerada%in%T, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(muj_remunerada%in%T & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(muj_remunerada%in%T & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# 
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# tab_remuneradas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)
tab_remuneradas <- NA
#COE ######################################################################

# t7 <- TAB[[16]]
# #COE
# t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
# t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
# t7 <- t7[t7$RESUL_POB%in%c('A'), ]
# 
# t7$EDAD <- as.numeric(as.character(t7$EDAD))
# 
# #---
# #Parche para acompletar 
# f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$FACTOR_PER%in%NA & t7$EDAD%in%c('12', 13:17), t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(f0%in%T & t7$EDAD%in%c('12', 13:17), t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(f1%in%T & t7$EDAD%in%c('12', 13:17), t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# 
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & t7$EDAD%in%c('12', 13:17), t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('12', 13:17) & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('12', 13:17) & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'1'& t7$EDAD%in%c('12', 13:17), t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('12', 13:17) & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('12', 13:17) & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_COE <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
tab_COE <- NA 

#DIVERSIDAD SEXUAL y de GÉNERO##################################################
#Este sale del COE
#col 1

# fil <-t7$EDAD%in%c('12', 13:17) & (t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6))
# #Parche para acompletar 
# f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(fil%in%T & !t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(fil%in%T & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(fil%in%T & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# 
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_div <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
tab_div <- NA

#===============================================================================
cc <- c("est", "cv", "int", "err")
grupos <- c("Población indígena", 
            "Población afrodescendiente", 
            "Población con discapacidad",
            "Población migrante", 
            "Población de diversidad religiosa",
            "Población de 60 años y más",
            "Adolescentes y jóvenes", 
            "Mujeres de 18 años y más",
            "Trabajadoras del hogar",
            "Población de 18 años y más", 
            "Población de 18 años y más con diversidad sexual y de género")

###
#Si experimentó----
tab_1 <- purrr::map(.x =1:4, 
                     .f = ~ rbind(NA,tab_indigenas[[.x]][2, ], 
                                  tab_afrodescendientes[[.x]][2, ],
                                  tab_discapacidad[[.x]][2, ],
                                  tab_migrantes[[.x]][2, ],
                                  tab_religion[[.x]][2, ],
                                  NA,
                                  tab_adolescentes[[.x]][2, ],
                                  NA,
                                  NA,
                                  NA,
                                  NA))
for(i in 1:4){tab_1[[1]][,1] <- c("Estados Unidos Mexicanos", grupos)}

for(i in c(1,2,4)){
  tab_1[[i]][c(7, 9:12), c(2,3,5,6,8,9)] <- "NA"
  tab_1[[i]][1, 1] <- "12 a 17 años"
}




################################################################################
################################################################################
#de 18 a 29 años
#INDIGENAS 12 a 17 ######################################################################
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===
estois <- names(bases[[1]])[5:153]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A') & t7$EDAD%in%c('18', 19:29), 1, 0)


t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#Parche para acompletar indigenas 
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_indigenas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#AFRODESCENDIENTES 12 a 17 ##############################################################
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:159]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c("A") & t7$EDAD%in%c('18', 19:29),1,0)

#Parche para acompletar afrodescendientes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_afrodescendientes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#DISCAPACIDAD 12 a 17 ###################################################################
bases[[1]] <- TAB[[7]]
bases[[2]] <- TAB[[2]]
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:160]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois,  "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A') & t7$EDAD%in%c('18', 19:29), 1, 0)
sum(t7$discapacidad)
# -> Población verificada con discapacidad
#t7$POBF1_1 <- ifelse( t7$FILTRO_2%in%c('1', '2') & t7$PMVD_1%in%'1',1,0)
t7$POBF1_1 <- t7$discapacidad
t7$EDAD <- as.numeric(as.character(t7$EDAD))

#Parche para acompletar discapacidad
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_discapacidad <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MIGRANTES de 12 a 17######################################################################
bases[[1]] <- TAB[[9]]
bases[[2]] <- TAB[[2]]
#Migrantes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:151]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$migrante <- ifelse(t7$P4_2_03%in%c('A') & t7$EDAD%in%c('18', 19:29), 1, 0)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---

#Parche para acompletar migrantes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_migrantes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#RELIGIÖN de 12 a 17 ##################################################################
bases[[1]] <- TAB[[10]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$religion <- ifelse(t7$P4_2_04%in%c('A') & t7$EDAD%in%c('18', 19:29), 1, 0)
#sum(t7$religion)
#---
#Parche para acompletar religión
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_religion<- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#MAYORES DE 60########################################################################
# bases[[1]] <- TAB[[12]]
# bases[[2]] <- TAB[[2]]
# #mayor
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
# #---
# estois <- names(bases[[1]])[5:145]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$mayor <- ifelse(t7$P4_2_05%in%c('A')  & t7$EDAD%in%c('18', 19:29), 1, 0)
# #sum(t7$mayor)
# #---
# #Parche para acompletar
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(!t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(!t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_mayores <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#Adolescentes###################################################################
bases[[1]] <- TAB[[14]]
bases[[2]] <- TAB[[2]]
#adolesc
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:171]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$adolesc <- ifelse(t7$P4_2_07%in%c('A') & t7$EDAD%in%c('18', 19:29), 1, 0)
#sum(t7$adolesc)
#---
#Parche para acompletar 
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$adolesc%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_adolescentes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MUJERES########################################################################

bases[[1]] <- TAB[[15]]
bases[[2]] <- TAB[[2]]
#mujer
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:163]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mujer <- ifelse(t7$P4_2_08%in%c('A') & t7$EDAD%in%c('18', 19:29), 1, 0)
#---
#Parche para acompletar mujeres
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_mujeres <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#Mujeres TRABAJDORAS DEL HOGAR##################################################
#--- 
muj_remunerada <- t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & t7$EDAD%in%c('18', 19:29)

f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(muj_remunerada%in%T, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(muj_remunerada%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(muj_remunerada%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
tab_remuneradas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)
#COE ######################################################################

t7 <- TAB[[16]]
#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
t7 <- t7[t7$RESUL_POB%in%c('A'), ]

t7$EDAD <- as.numeric(as.character(t7$EDAD))

#---
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$FACTOR_PER%in%NA & t7$EDAD%in%c('18', 19:29), t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0%in%T & t7$EDAD%in%c('18', 19:29), t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f1%in%T & t7$EDAD%in%c('18', 19:29), t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & t7$EDAD%in%c('18', 19:29), t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('18', 19:29) & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('18', 19:29) & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1'& t7$EDAD%in%c('18', 19:29), t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('18', 19:29) & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('18', 19:29) & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_COE <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#DIVERSIDAD SEXUAL y de GÉNERO##################################################
#Este sale del COE
#col 1

fil <- t7$EDAD%in%c('18', 19:29)& (t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6))
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(fil%in%T & !t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(fil%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(fil%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_div <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

###
#Si experimentó---
tab_2 <- purrr::map(.x =1:4, 
                     .f = ~ rbind(NA,tab_indigenas[[.x]][2, ], 
                                  tab_afrodescendientes[[.x]][2, ],
                                  tab_discapacidad[[.x]][2, ],
                                  tab_migrantes[[.x]][2, ],
                                  tab_religion[[.x]][2, ],
                                  NA,
                                  tab_adolescentes[[.x]][2, ],
                                  tab_mujeres[[.x]][2, ],
                                  tab_remuneradas[[.x]][2, ],
                                  tab_COE[[.x]][2, ],
                                  tab_div[[.x]][2, ]))
for(i in 1:4){tab_2[[1]][,1] <- c("Estados Unidos Mexicanos", grupos)}
#----
for(i in c(1,2,4)){
  tab_2[[i]][c(7), c(2,3,5,6,8,9)] <- "NA"
  tab_2[[i]][1, 1] <- "18 a 29 años"
}



################################################################################
################################################################################
#de 30 a 59 años


#INDIGENAS 12 a 17 ######################################################################
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===
estois <- names(bases[[1]])[5:153]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A') & t7$EDAD%in%c('30', 31:59), 1, 0)


t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#Parche para acompletar indigenas 
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_indigenas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#AFRODESCENDIENTES 12 a 17 ##############################################################
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:159]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c("A") & t7$EDAD%in%c('30', 31:59),1,0)

#Parche para acompletar afrodescendientes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_afrodescendientes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#DISCAPACIDAD 12 a 17 ###################################################################
bases[[1]] <- TAB[[7]]
bases[[2]] <- TAB[[2]]
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:160]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois,  "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A') & t7$EDAD%in%c('30', 31:59), 1, 0)
sum(t7$discapacidad)
# -> Población verificada con discapacidad
#t7$POBF1_1 <- ifelse( t7$FILTRO_2%in%c('1', '2') & t7$PMVD_1%in%'1',1,0)
t7$POBF1_1 <- t7$discapacidad
t7$EDAD <- as.numeric(as.character(t7$EDAD))

#Parche para acompletar discapacidad
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_discapacidad <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MIGRANTES de 12 a 17######################################################################
bases[[1]] <- TAB[[9]]
bases[[2]] <- TAB[[2]]
#Migrantes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:151]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$migrante <- ifelse(t7$P4_2_03%in%c('A') & t7$EDAD%in%c('30', 31:59), 1, 0)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---

#Parche para acompletar migrantes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_migrantes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#RELIGIÖN de 12 a 17 ##################################################################
bases[[1]] <- TAB[[10]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$religion <- ifelse(t7$P4_2_04%in%c('A') & t7$EDAD%in%c('30', 31:59), 1, 0)
#sum(t7$religion)
#---
#Parche para acompletar religión
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_religion<- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#MAYORES DE 60########################################################################
# bases[[1]] <- TAB[[12]]
# bases[[2]] <- TAB[[2]]
# #mayor
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
# #---
# estois <- names(bases[[1]])[5:145]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$mayor <- ifelse(t7$P4_2_05%in%c('A')  & t7$EDAD%in%c('30', 31:59), 1, 0)
# #sum(t7$mayor)
# #---
# #Parche para acompletar
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(!t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(!t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_mayores <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#Adolescentes###################################################################
# bases[[1]] <- TAB[[14]]
# bases[[2]] <- TAB[[2]]
# #adolesc
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
# #---
# estois <- names(bases[[1]])[5:171]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$adolesc <- ifelse(t7$P4_2_07%in%c('A') & t7$EDAD%in%c('30', 31:59), 1, 0)
# #sum(t7$adolesc)
# #---
# #Parche para acompletar 
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$adolesc%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(!t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(!t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_adolescentes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MUJERES########################################################################

bases[[1]] <- TAB[[15]]
bases[[2]] <- TAB[[2]]
#mujer
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:163]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mujer <- ifelse(t7$P4_2_08%in%c('A') & t7$EDAD%in%c('30', 31:59), 1, 0)
#---
#Parche para acompletar mujeres
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_mujeres <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#Mujeres TRABAJDORAS DEL HOGAR##################################################
#--- 
muj_remunerada <- t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & t7$EDAD%in%c('30', 31:59)

f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(muj_remunerada%in%T, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(muj_remunerada%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(muj_remunerada%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
tab_remuneradas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)
#COE ######################################################################

t7 <- TAB[[16]]
#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
t7 <- t7[t7$RESUL_POB%in%c('A'), ]

t7$EDAD <- as.numeric(as.character(t7$EDAD))

#---
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$FACTOR_PER%in%NA & t7$EDAD%in%c('30', 31:59), t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0%in%T & t7$EDAD%in%c('30', 31:59), t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f1%in%T & t7$EDAD%in%c('30', 31:59), t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & t7$EDAD%in%c('30', 31:59), t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('30', 31:59) & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('30', 31:59) & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1'& t7$EDAD%in%c('30', 31:59), t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('30', 31:59) & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('30', 31:59) & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_COE <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#DIVERSIDAD SEXUAL y de GÉNERO##################################################
#Este sale del COE
#col 1

fil <- t7$EDAD%in%c('30', 31:59)& (t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6))
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(fil%in%T & !t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(fil%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(fil%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_div <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


###
#Si experimentó---
tab_3 <- purrr::map(.x =1:4, 
                     .f = ~ rbind(NA,tab_indigenas[[.x]][2, ], 
                                  tab_afrodescendientes[[.x]][2, ],
                                  tab_discapacidad[[.x]][2, ],
                                  tab_migrantes[[.x]][2, ],
                                  tab_religion[[.x]][2, ],
                                  NA,
                                  NA,
                                  tab_mujeres[[.x]][2, ],
                                  tab_remuneradas[[.x]][2, ],
                                  tab_COE[[.x]][2, ],
                                  tab_div[[.x]][2, ]))

#----

for(i in 1:4){tab_3[[1]][,1] <- c("Estados Unidos Mexicanos", grupos)}
for(i in c(1,2,4)){
  tab_3[[i]][c(7,8), c(2,3,5,6,8,9)] <- "NA"
  tab_3[[i]][1, 1] <- "30 a 59 años"
}

################################################################################
################################################################################
#De 60 a 96 años

#INDIGENAS 60a 96 ######################################################################
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===
estois <- names(bases[[1]])[5:153]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A') & t7$EDAD%in%c('60', 61:96), 1, 0)


t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#Parche para acompletar indigenas 
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$marca_indi%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_indigenas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#AFRODESCENDIENTES 60 a 96 ##############################################################
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:159]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c("A") & t7$EDAD%in%c('60', 61:96),1,0)

#Parche para acompletar afrodescendientes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
t7$TOT_3 <- 0.0000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$afro_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$afro_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_afrodescendientes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#DISCAPACIDAD 60 a 96 ###################################################################
bases[[1]] <- TAB[[7]]
bases[[2]] <- TAB[[2]]
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:160]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois,  "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A') & t7$EDAD%in%c('60', 61:96), 1, 0)
sum(t7$discapacidad)
# -> Población verificada con discapacidad
#t7$POBF1_1 <- ifelse( t7$FILTRO_2%in%c('1', '2') & t7$PMVD_1%in%'1',1,0)
t7$POBF1_1 <- t7$discapacidad
t7$EDAD <- as.numeric(as.character(t7$EDAD))

#Parche para acompletar discapacidad
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$POBF1_1%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_discapacidad <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MIGRANTES de 60 a 96######################################################################
bases[[1]] <- TAB[[9]]
bases[[2]] <- TAB[[2]]
#Migrantes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:151]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$migrante <- ifelse(t7$P4_2_03%in%c('A') & t7$EDAD%in%c('60', 61:96), 1, 0)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---

#Parche para acompletar migrantes
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$migrante%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$migrante%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_migrantes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#RELIGIÖN de 60 a 96 ##################################################################
bases[[1]] <- TAB[[10]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$religion <- ifelse(t7$P4_2_04%in%c('A') & t7$EDAD%in%c('60', 61:96), 1, 0)
#sum(t7$religion)
#---
#Parche para acompletar religión
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$religion%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$religion%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_religion<- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#MAYORES DE 60########################################################################
bases[[1]] <- TAB[[12]]
bases[[2]] <- TAB[[2]]
#mayor
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:145]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mayor <- ifelse(t7$P4_2_05%in%c('A')  & t7$EDAD%in%c('60', 61:96), 1, 0)
#sum(t7$mayor)
#---
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$mayor%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$mayor%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$mayor%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_mayores <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#Adolescentes###################################################################
# bases[[1]] <- TAB[[14]]
# bases[[2]] <- TAB[[2]]
# #adolesc
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
# #---
# estois <- names(bases[[1]])[5:171]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$adolesc <- ifelse(t7$P4_2_07%in%c('A') & t7$EDAD%in%c('60', 61:96), 1, 0)
# #sum(t7$adolesc)
# #---
# #Parche para acompletar 
# f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
# f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))
# 
# si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
# t7$TOT <- ifelse(!t7$adolesc%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <- ifelse(!t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <- ifelse(!t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$adolesc%in%0, t7$FACTOR_PER, 0)
# t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
# t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$adolesc%in%0 & f1%in%T, t7$FACTOR_PER, 0)
# t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
# asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
# bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
# 
# tab_adolescentes <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#MUJERES########################################################################

bases[[1]] <- TAB[[15]]
bases[[2]] <- TAB[[2]]
#mujer
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:163]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mujer <- ifelse(t7$P4_2_08%in%c('A') & t7$EDAD%in%c('60', 61:96), 1, 0)
#---
#Parche para acompletar mujeres
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$mujer%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$mujer%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_mujeres <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)


#Mujeres TRABAJDORAS DEL HOGAR##################################################
#--- 
muj_remunerada <- t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & t7$EDAD%in%c('60', 61:96)

f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(muj_remunerada%in%T, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(muj_remunerada%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(muj_remunerada%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
tab_remuneradas <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)
#COE ######################################################################

t7 <- TAB[[16]]
#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
t7 <- t7[t7$RESUL_POB%in%c('A'), ]

t7$EDAD <- as.numeric(as.character(t7$EDAD))

#---
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$FACTOR_PER%in%NA & t7$EDAD%in%c('60', 61:96), t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0%in%T & t7$EDAD%in%c('60', 61:96), t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f1%in%T & t7$EDAD%in%c('60', 61:96), t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & t7$EDAD%in%c('60', 61:96), t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('60', 61:96) & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & t7$EDAD%in%c('60', 61:96) & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1'& t7$EDAD%in%c('60', 61:96), t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('60', 61:96) & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1'& t7$EDAD%in%c('60', 61:96) & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_COE <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#DIVERSIDAD SEXUAL y de GÉNERO##################################################
#Este sale del COE
#col 1

fil <- t7$EDAD%in%c('60', 61:96)& (t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6))
#Parche para acompletar
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(fil%in%T & !t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(fil%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(fil%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'2' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( fil%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(fil%in%T & t7$SEXO%in%'1' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tab_div <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

###
#Si experimentó----
tab_4<- purrr::map(.x =1:4, 
                     .f = ~ rbind(NA,tab_indigenas[[.x]][2, ], 
                                  tab_afrodescendientes[[.x]][2, ],
                                  tab_discapacidad[[.x]][2, ],
                                  tab_migrantes[[.x]][2, ],
                                  tab_religion[[.x]][2, ],
                                  tab_mayores[[.x]][2, ],
                                  NA,
                                  tab_mujeres[[.x]][2, ],
                                  tab_remuneradas[[.x]][2, ],
                                  tab_COE[[.x]][2, ],
                                  tab_div[[.x]][2, ]))

for(i in 1:4){tab_4[[1]][,1] <- c("Estados Unidos Mexicanos", grupos)}
for(i in c(1,2,4)){
  tab_4[[i]][c(7,8), c(2,3,5,6,8,9)] <- "NA"
  tab_4[[i]][1, 1] <- "60 a 96 años"
}


#===============================================================================
tabulado <- purrr::map(.x =1:4, 
                  .f =~rbind(NA,tab_1[[.x]], NA, tab_2[[.x]], NA, tab_3[[.x]], NA, tab_4[[.x]]))

for(i in 1:4){tabulado[[i]][1,1] <- "Estados Unidos Mexicanos"}
#===============================================================================
#write.csv(tabulado[[2]], "D:/ENADIS/Entregable_enadis_2022/Tabs/enadis_2022_presentacion/d171_cv.csv")
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 78
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])
