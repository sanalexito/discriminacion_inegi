library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
#===============================================================================
nombres_bases <- names(TAB)

bases <- list()

#INDIGENA=======================================================================
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")

#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM1_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A'), 1, 0)


#col 1
t7$TOT <- ifelse(t7$marca_indi%in%1 , t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$marca_indi%in%1 & t7$PM1_1%in%",i,", t7$FACTOR_PER, 0)")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$marca_indi%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$marca_indi%in%1 & t7$PM1_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$marca_indi%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$marca_indi%in%1 & t7$PM1_1%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

tab_ind <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)


#AFROS==========================================================================
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM1_8", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c("A"),1,0)
#---

#col 1
t7$TOT <- ifelse(t7$afro_1%in%1, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_8%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$afro_1%in%1 &t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_8%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_8%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_1 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)


#DISCAPACIDAD===================================================================
bases[[1]] <- TAB[[7]]
bases[[2]] <- TAB[[2]]
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM2_1",  "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A'),1, 0)
sum(t7$discapacidad)
t7$POBF1_1 <- t7$discapacidad

#col 1
t7$no_respondieron <- ifelse(t7$PM2_1%in%NA, t7$FACTOR_PER, 0)
t7$TOT <- ifelse(t7$POBF1_1%in%1 , t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$POBF1_1%in%'1' & t7$PM2_1%in%",i,", t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$POBF1_1%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$POBF1_1%in%'1' & t7$PM2_1%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$POBF1_1%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$POBF1_1%in%'1' & t7$PM2_1%in%",i,"& t7$SEXO%in%'1', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_3 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_2 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)


#MIGRANTES======================================================================
bases[[1]] <- TAB[[9]]
bases[[2]] <- TAB[[2]]
#Migrantes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM3_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$migrante <- ifelse(t7$P4_2_03%in%c('A'), 1, 0)
#---

#col 1
t7$TOT <- ifelse(t7$migrante%in%1, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$migrante%in%1 & t7$PM3_1%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$migrante%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$migrante%in%1 & t7$PM3_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$migrante%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$migrante%in%1 & t7$PM3_1%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_3 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)



#RELIGION=======================================================================
bases[[1]] <- TAB[[10]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM4_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$religion <- ifelse(t7$P4_2_04%in%c('A'), 1, 0)
#---
#col 1
t7$TOT <- ifelse(t7$religion%in%1, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$religion%in%1 & t7$PM4_1%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$religion%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$religion%in%1 & t7$PM4_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$religion%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$religion%in%1 & t7$PM4_1%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_4 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)


#MAYORES (60 Y MÁS)=============================================================
bases[[1]] <- TAB[[12]]
bases[[2]] <- TAB[[2]]
#mayores de 60
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM5_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mayor <- ifelse(t7$P4_2_05%in%c('A'), 1, 0)
#---
#col 1
t7$TOT <- ifelse(t7$mayor%in%1, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mayor%in%1 & t7$PM5_1%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$mayor%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$mayor%in%1 & t7$PM5_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$mayor%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$mayor%in%1 & t7$PM5_1%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_5 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)


#ADOLESCENTES===================================================================
bases[[1]] <- TAB[[14]]
bases[[2]] <- TAB[[2]]
#adolescentes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM7_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$adol <- ifelse(t7$P4_2_07%in%c('A'), 1, 0)
#---
#col 1
t7$TOT <- ifelse(t7$adol%in%1, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$adol%in%1 & t7$PM7_1%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$adol%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$adol%in%1 & t7$PM7_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(t7$adol%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$adol%in%1 & t7$PM7_1%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_6 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)


#MUJERES========================================================================
bases[[1]] <- TAB[[15]]
bases[[2]] <- TAB[[2]]
#mujeres
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM8_1", "PM8_8", "PM8_9", "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mujer <- ifelse(t7$P4_2_08%in%c('A'), 1, 0)
#---
#col 1
t7$TOT <- ifelse(t7$mujer%in%1, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_1%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$mujer%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

bla_7 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Mujeres_0)


#MUJERES REMUNERADAS============================================================
#col 1
t7$TOT <- ifelse(t7$mujer%in%1 & t7$PM8_8%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_9%in%",i," & t7$PM8_8%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$mujer%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_9%in%",i," & t7$PM8_8%in%'1'& t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

bla_8 <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Mujeres_0)

nombres <- c("Población indígena de 12 años y más",
             "Población afrodescendiente de 12 años y más",
             "Población de 12 años y más con discapacidad",
             "Población migrante de 15 años y más",
             "Población de la diversidad religiosa de 12 años y más",
             "Población de 60 años y más",
             "Población de adolescentes y jóvenes",
             "Población de mujeres de 18 años y más",
             "Población de trabajadoras del hogar remuneradas")
  colnames(bla_1[[1]]) <- colnames(bla_2[[1]]) <- colnames(bla_3[[1]]) <- colnames(bla_4[[1]]) <- colnames(bla_5[[1]]) <- colnames(bla_6[[1]]) <- colnames(bla_7[[1]]) <- colnames(bla_8[[1]]) <- colnames(tab_ind[[1]]) 
  colnames(bla_1[[2]]) <- colnames(bla_2[[2]]) <- colnames(bla_3[[2]]) <- colnames(bla_4[[2]]) <- colnames(bla_5[[2]]) <- colnames(bla_6[[2]]) <- colnames(bla_7[[2]]) <- colnames(bla_8[[2]]) <- colnames(tab_ind[[2]]) 
  colnames(bla_1[[3]]) <- colnames(bla_2[[3]]) <- colnames(bla_3[[3]]) <- colnames(bla_4[[3]]) <- colnames(bla_5[[3]]) <- colnames(bla_6[[3]]) <- colnames(bla_7[[3]]) <- colnames(bla_8[[3]]) <- colnames(tab_ind[[3]]) 
  colnames(bla_1[[4]]) <- colnames(bla_2[[4]]) <- colnames(bla_3[[4]]) <- colnames(bla_4[[4]]) <- colnames(bla_5[[4]]) <- colnames(bla_6[[4]]) <- colnames(bla_7[[4]]) <- colnames(bla_8[[4]]) <- colnames(tab_ind[[4]]) 
  
tab_22 <- purrr::map(.x = 1:4, 
                     .f=~ pegadora(list=tab_ind[[.x]], cuantos = 8, prec =.x , renglon = 2, nombres, columna = 3))




#===============================================================================
#2017

#INDIGENAS======================================================================
nombre_bases <- dir('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/')

bases <- purrr::map(.x = c(7,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM1_1", "FACTOR_PER")], by = "ID_PER", all.x = T)

#col 1
t7$TOT <- ifelse(  !t7$PM1_1%in%NA, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$PM1_1%in%",i,", t7$FACTOR_PER, 0)")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
edu_0 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(!t7$PM1_1%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM1_1%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Mujeres_0 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(!t7$PM1_1%in%NA & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for( i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM1_1%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
Hombres_0 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

tab_ind <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)

#AFRODES========================================================================
bla_1 <- NA

#DISCAPACIDAD===================================================================
bases <- purrr::map(.x = c(4,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR,bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM2_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
#col 1
t7$TOT <- ifelse( !t7$PM2_1%in%NA, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM2_1%in%",i,", t7$FACTOR_PER, 0) ")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(!t7$PM2_1%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM2_1%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(!t7$PM2_1%in%NA & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM2_1%in%",i,"& t7$SEXO%in%'1', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_3 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_3 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)


#MIGRANTES======================================================================
bla_4 <- NA
#RELIGION=======================================================================
bases <- purrr::map(.x = c(11,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR,bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM3_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
#col 1
t7$TOT <- ifelse( !t7$PM3_1%in%NA, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM3_1%in%",i,", t7$FACTOR_PER, 0) ")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(!t7$PM3_1%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM3_1%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(!t7$PM3_1%in%NA & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM3_1%in%",i,"& t7$SEXO%in%'1', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_3 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_5 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)

#MAYORES DE 60==================================================================
bases <- purrr::map(.x = c(2,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#mayor de 60
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR,bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM4_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
#col 1
t7$TOT <- ifelse( !t7$PM4_1%in%NA, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM4_1%in%",i,", t7$FACTOR_PER, 0) ")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(!t7$PM4_1%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM4_1%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(!t7$PM4_1%in%NA & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM4_1%in%",i,"& t7$SEXO%in%'1', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_3 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_6 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)


#ADOLESCENTES===================================================================
bases <- purrr::map(.x = c(1,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#adolescentes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR,bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM6_1", "FACTOR_PER")], by = "ID_PER", all.x = T)
#col 1
t7$TOT <- ifelse( !t7$PM6_1%in%NA, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM6_1%in%",i,", t7$FACTOR_PER, 0) ")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(!t7$PM6_1%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM6_1%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col 3
t7$TOT <- ifelse(!t7$PM6_1%in%NA & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM6_1%in%",i,"& t7$SEXO%in%'1', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_3 <- Tab_vert_a(xx, c("Hombres",etiquetas0))

bla_7 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)


#MUJERES========================================================================
bases <- purrr::map(.x = c(9,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#mujers
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR,bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
#---
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM7_1" ,"PM7_7", "PM7_8", "FACTOR_PER")], by = "ID_PER", all.x = T)
#col 1
t7$TOT <- ifelse( !t7$PM7_1%in%NA, t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM7_1%in%",i,", t7$FACTOR_PER, 0) ")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(!t7$PM7_1%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$PM7_1%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

bla_8 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_2)


#MUJERES REMUNERADAS============================================================
t7$TOT <- ifelse( t7$PM7_7%in%'1' , t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
  t7$TOT_",i," <- ifelse(t7$PM7_8%in%",i,", t7$FACTOR_PER, 0) ")))
etiquetas0 <- c('Mucho','Algo','Poco', 'Nada')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#col 2
t7$TOT <- ifelse(t7$PM7_7%in%'1' & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$PM7_8%in%",i,"& t7$SEXO%in%'2', t7$FACTOR_PER, 0) ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

bla_9 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_2)



#===============================================================================
colnames(bla_3[[1]]) <- colnames(bla_5[[1]]) <- colnames(bla_6[[1]]) <- colnames(bla_7[[1]]) <- colnames(bla_8[[1]]) <- colnames(tab_ind[[1]]) 
colnames(bla_3[[2]]) <- colnames(bla_5[[2]]) <- colnames(bla_6[[2]]) <- colnames(bla_7[[2]]) <- colnames(bla_8[[2]]) <- colnames(tab_ind[[2]]) 
colnames(bla_3[[3]]) <- colnames(bla_5[[3]]) <- colnames(bla_6[[3]]) <- colnames(bla_7[[3]]) <- colnames(bla_8[[3]]) <- colnames(tab_ind[[3]]) 
colnames(bla_3[[4]]) <- colnames(bla_5[[4]]) <- colnames(bla_6[[4]]) <- colnames(bla_7[[4]]) <- colnames(bla_8[[4]]) <- colnames(tab_ind[[4]]) 

tab_17 <- list()
for(i in 1:4){
tab_17[[i]] <- rbind(tab_ind[[i]][2,], 
                       bla_3[[i]][2,],  
                       bla_5[[i]][2,],  
                       bla_6[[i]][2,],  
                       bla_7[[i]][2,],  
                       bla_8[[i]][2,],  
                       bla_9[[i]][2,])}


tab_17 <- purrr::map(.x = 1:4,
                     .f=~ rbind(tab_17[[.x]][1,], NA,tab_17[[.x]][2,], NA,
                                tab_17[[.x]][3:7,]))
for(i in 1:4){
  tab_17[[i]][,1] <- nombres
}

#===============================================================================
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/hace_ph.R')

aa <- purrr::map(.x = c(3,6,9), .f = ~hace_df(tab_17, tab_22, columna = .x))
pa_ph <- purrr::map(.x = 1:2, .f= ~rbind(NA,aa[[1]][[.x]], NA, NA, aa[[2]][[.x]], NA,NA, aa[[3]][[.x]]))
df <- ph_a(pa_ph[[1]])

df[1,1] <- "Estados Unidos Mexicanos"
df[12, 1] <- "Mujeres"
df[23, 1] <- "Hombres"

cc <- c("Población afrodescendiente de 12 años y más",
        "Población migrante de 15 años y más")

df[which(df[,1]%in%cc), c(2,6)] <- "ND"
df[which(df[,1]%in%cc), c(4,8)] <- "NA"

escribe_a(ruta="D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx", 
          nom_hoja = "d165", 
          df = df, 
          cv = pa_ph[[2]])
