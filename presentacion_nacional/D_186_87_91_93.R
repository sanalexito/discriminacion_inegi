#D186, D187, D191, D193 
#Revisa que se imprimen en diferentes hojas :)




library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)

bases <- list()
#==== Indígenas ================================================================
bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_i <- as.numeric(as.character(bases[[1]]$FACTOR))
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "FACTOR_PER_i")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_i <- ifelse(t7$FACTOR_PER_i%in%NA, 0, t7$FACTOR_PER_i)
#==== Afrodescendientes ========================================================
bases[[1]] <- TAB[[5]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_a <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "FACTOR_PER_a")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_a <- ifelse(t7$FACTOR_PER_a%in%NA, 0, t7$FACTOR_PER_a)

#==== Discapacidad =============================================================
bases[[1]] <- TAB[[7]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_d <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER",  "FACTOR_PER_d")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_d <- ifelse(t7$FACTOR_PER_d%in%NA, 0, t7$FACTOR_PER_d)

#==== Migrnates =============================================================

bases[[1]] <- TAB[[9]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_mi <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER","FACTOR_PER_mi")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_mi <- ifelse(t7$FACTOR_PER_mi%in%NA, 0, t7$FACTOR_PER_mi)

#==== Religion =================================================================

bases[[1]] <- TAB[[10]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_r <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "FACTOR_PER_r")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_r <- ifelse(t7$FACTOR_PER_r%in%NA, 0, t7$FACTOR_PER_r)


#==== MAYOR ====================================================================
bases[[1]] <- TAB[[12]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_mayor <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "FACTOR_PER_mayor")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_mayor <- ifelse(t7$FACTOR_PER_mayor%in%NA, 0, t7$FACTOR_PER_mayor)


#==== NIÑO ====================================================================
bases[[1]] <- TAB[[13]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER_niño <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "FACTOR_PER_niño")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_niño <- ifelse(t7$FACTOR_PER_niño%in%NA, 0, t7$FACTOR_PER_niño)

#==== ADOLESCENTE ==============================================================
bases[[1]] <- TAB[[14]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_adol <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "FACTOR_PER_adol")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_adol <- ifelse(t7$FACTOR_PER_adol%in%NA, 0, t7$FACTOR_PER_adol)

#==== Mujeres y Mujeres trabajadoras ===========================================

bases[[1]] <- TAB[[15]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_PER_mu <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "PM8_8","FACTOR_PER_mu")], by = "ID_PER", all.x = T)
t7$FACTOR_PER_mu <- ifelse(t7$FACTOR_PER_mu%in%NA, 0, t7$FACTOR_PER_mu)


#==== COE ===========================================

bases[[1]] <- TAB[[16]]
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$FACTOR_COE <- as.numeric(as.character(bases[[1]]$FACTOR))
t7 <- merge(t7, bases[[1]][,c("ID_PER", "PO5_2", "PO5_3", "FACTOR_COE")], by = "ID_PER", all.x = T)
t7$FACTOR_COE <- ifelse(t7$FACTOR_COE%in%NA, 0, t7$FACTOR_COE)

#-----------------------------------------------------------------------------
etiquetas0 <- c('Indígena (12 a 96 años)',
                'Afrodescendiente (12 a 96 años)',
                'Discapacidad (12 a 96 años)',
                'Migrante (15 a 96 años)',
                'Diversidad religiosa (12 a 96 años)',
                'Mayor de 60 años (60 a 96 años)',
                'Adolescentes o jóvenes (12 a 29 años)',
                'Adolescentes (12 a 17 años)',
                'Jóvenes (18 a 29 años)',
                'Mujeres (18 a 96 años)',
                'Trabajadoras del hogar (18 a 96 años)',
                'Diversidad sexual y de género')

#===============================================================================
#Tabulado de escolaridad D_186----

#--- Nacional
f0 <- !t7$NIV%in%NA
f1 <- t7$NIV%in%c(paste0('0',3:9), 10) | t7$P3_18%in%'1'
f2 <- t7$P3_18%in%'2'
f3 <- t7$P3_18%in%'9'

t7$TOT <- ifelse(f0%in%T, t7$FACTOR, 0)
for(i in 1:3)eval(parse(text = paste0("t7$TOT_",i," <- ifelse(f",i,"%in%T, t7$FACTOR, 0)")))
xx <- c('TOT', paste0('TOT_', 1:3))
xxx <- c("Estados Unidos Mexicanos", 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_2(xx, 'SEXO', xxx)

#--- indígenas
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A'), t7$FACTOR_PER_i, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_i, 0)
")))
xxx <- c(etiquetas0[1], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_1 <- Tab_2(xx, 'SEXO', xxx)

#--- afrodescendientes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A'), t7$FACTOR_PER_a, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_a, 0)
")))
xxx <- c(etiquetas0[2], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_2 <- Tab_2(xx, 'SEXO', xxx)

#--- discapacidad
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A'), t7$FACTOR_PER_d, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_d, 0)
")))
xxx <- c(etiquetas0[3], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_3 <- Tab_2(xx, 'SEXO', xxx)

#--- migrante
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A'), t7$FACTOR_PER_mi, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mi, 0)
")))
xxx <- c(etiquetas0[4], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_4 <- Tab_2(xx, 'SEXO', xxx)

#--- diversidad religiosa
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A'), t7$FACTOR_PER_r, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_r, 0)
")))
xxx <- c(etiquetas0[5], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_5 <- Tab_2(xx, 'SEXO', xxx)

#--- mayor 60
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A'), t7$FACTOR_PER_mayor, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mayor, 0)
")))
xxx <- c(etiquetas0[6], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_6 <- Tab_2(xx, 'SEXO', xxx)

#--- Adolescentes y jóvenes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[7], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_7 <- Tab_2(xx, 'SEXO', xxx)

#--- solo adolescentes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[8], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_8 <- Tab_2(xx, 'SEXO', xxx)

#--- solo jovenes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[9], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_9 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[10], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_10 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres trabajadoras hogar
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[11], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_11 <- Tab_2(xx, 'SEXO', xxx)

#--- Diversidad sexual
fil <- t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6)

t7$TOT <- ifelse(fil%in%T & f0%in%T & t7$P4_2_10%in%c('A'), t7$FACTOR_COE, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(fil%in%T & t7$EDAD%in%18:96 & t7$P4_2_10%in%c('A') & f",i,"%in%T, t7$FACTOR_COE, 0)
")))
xxx <- c(etiquetas0[12], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_12 <- Tab_2(xx, 'SEXO', xxx)


#Arma bolques
nacional<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x, renglon = 1, 
                                             c("Estados Unidos Mexicanos",etiquetas0)))
hombres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 2, 
                                            c("Hombres",etiquetas0)))
mujeres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 3, 
                                            c("Mujeres",etiquetas0)))
tab <- purrr::map(.x =1:4, .f=~rbind(nacional[[.x]], NA, mujeres[[.x]], NA, hombres[[.x]]))
#---
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 85
Escribe_Excel_a(ruta, num_hoja, tab[[1]])


#===============================================================================
#Tabulado de asistecia a la escuela D_187----

#--- Nacional
f0 <- !t7$P3_19%in%NA
f1 <- t7$P3_19%in%'1'
f2 <- t7$P3_19%in%'2'
f3 <- t7$P3_19%in%'9'

t7$TOT <- ifelse(f0%in%T , t7$FACTOR, 0)
for(i in 1:3)eval(parse(text = paste0("t7$TOT_",i," <- ifelse(f",i,"%in%T, t7$FACTOR, 0)")))
xx <- c('TOT', paste0('TOT_', 1:3))
xxx <- c("Estados Unidos Mexicanos", 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_2(xx, 'SEXO', xxx)

#--- indígenas
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A'), t7$FACTOR_PER_i, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_i, 0)
")))
xxx <- c(etiquetas0[1], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_1 <- Tab_2(xx, 'SEXO', xxx)

#--- afrodescendientes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A'), t7$FACTOR_PER_a, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_a, 0)
")))
xxx <- c(etiquetas0[2], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_2 <- Tab_2(xx, 'SEXO', xxx)

#--- discapacidad
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A'), t7$FACTOR_PER_d, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_d, 0)
")))
xxx <- c(etiquetas0[3], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_3 <- Tab_2(xx, 'SEXO', xxx)

#--- migrante
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A'), t7$FACTOR_PER_mi, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mi, 0)
")))
xxx <- c(etiquetas0[4], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_4 <- Tab_2(xx, 'SEXO', xxx)

#--- diversidad religiosa
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A'), t7$FACTOR_PER_r, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_r, 0)
")))
xxx <- c(etiquetas0[5], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_5 <- Tab_2(xx, 'SEXO', xxx)

#--- mayor 60
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A'), t7$FACTOR_PER_mayor, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mayor, 0)
")))
xxx <- c(etiquetas0[6], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_6 <- Tab_2(xx, 'SEXO', xxx)

#--- Adolescentes y jóvenes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[7], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_7 <- Tab_2(xx, 'SEXO', xxx)

#--- solo adolescentes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[8], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_8 <- Tab_2(xx, 'SEXO', xxx)

#--- solo jovenes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[9], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_9 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[10], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_10 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres trabajadoras hogar
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[11], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_11 <- Tab_2(xx, 'SEXO', xxx)

#--- Diversidad sexual y de género
t7$TOT <- ifelse(f0%in%T & fil%in%T & t7$P4_2_10%in%c('A'), t7$FACTOR_COE, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(fil%in%T & t7$P4_2_10%in%c('A') & f",i,"%in%T, t7$FACTOR_COE, 0)
")))
xxx <- c(etiquetas0[12], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_12 <- Tab_2(xx, 'SEXO', xxx)

#Arma bolques

nacional<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x, renglon = 1, 
                                             c("Estados Unidos Mexicanos",etiquetas0)))
hombres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 2, 
                                            c("Hombres",etiquetas0)))
mujeres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 3, 
                                            c("Mujeres",etiquetas0)))
tab <- purrr::map(.x =1:4, .f=~rbind(nacional[[.x]], NA, mujeres[[.x]], NA, hombres[[.x]]))

num_hoja <- 86
Escribe_Excel_a(ruta, num_hoja, tab[[1]])
#===============================================================================
#Tabulado de contrato D_191 ----
#--- Nacional
f0 <- t7$P3_21%in%c('1', '2')
f1 <- t7$P3_23_1%in%'1'
f2 <- t7$P3_23_4%in%'1'

lonja <- 2

t7$TOT <- ifelse(f0%in%T , t7$FACTOR, 0)
for(i in 1:lonja)eval(parse(text = paste0("t7$TOT_",i," <- ifelse(f",i,"%in%T, t7$FACTOR, 0)")))
xx <- c('TOT', paste0('TOT_', 1:lonja))
xxx <- c("Estados Unidos Mexicanos", 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_2(xx, 'SEXO', xxx)

#--- indígenas
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A'), t7$FACTOR_PER_i, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_i, 0)
")))
xxx <- c(etiquetas0[1], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_1 <- Tab_2(xx, 'SEXO', xxx)

#--- afrodescendientes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A'), t7$FACTOR_PER_a, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_a, 0)
")))
xxx <- c(etiquetas0[2], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_2 <- Tab_2(xx, 'SEXO', xxx)

#--- discapacidad
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A'), t7$FACTOR_PER_d, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_d, 0)
")))
xxx <- c(etiquetas0[3], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_3 <- Tab_2(xx, 'SEXO', xxx)

#--- migrante
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A'), t7$FACTOR_PER_mi, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mi, 0)
")))
xxx <- c(etiquetas0[4], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_4 <- Tab_2(xx, 'SEXO', xxx)

#--- diversidad religiosa
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A'), t7$FACTOR_PER_r, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_r, 0)
")))
xxx <- c(etiquetas0[5], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_5 <- Tab_2(xx, 'SEXO', xxx)

#--- mayor 60
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A'), t7$FACTOR_PER_mayor, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mayor, 0)
")))
xxx <- c(etiquetas0[6], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_6 <- Tab_2(xx, 'SEXO', xxx)

#--- Adolescentes y jóvenes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[7], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_7 <- Tab_2(xx, 'SEXO', xxx)

#--- solo adolescentes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[8], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_8 <- Tab_2(xx, 'SEXO', xxx)

#--- solo jovenes
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[9], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_9 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[10], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_10 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres trabajadoras hogar
t7$TOT <- ifelse(f0%in%T & t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[11], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_11 <- Tab_2(xx, 'SEXO', xxx)

#--- Diversidad sexual y de género
t7$TOT <- ifelse(fil%in%T & f0%in%T & t7$P4_2_10%in%c('A'), t7$FACTOR_COE, 0)
for(i in 1:3) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(fil%in%T & t7$P4_2_10%in%c('A') & f",i,"%in%T, t7$FACTOR_COE, 0)
")))
xxx <- c(etiquetas0[12], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_12 <- Tab_2(xx, 'SEXO', xxx)

#Arma bolques

nacional<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x, renglon = 1, 
                                             c("Estados Unidos Mexicanos",etiquetas0)))
hombres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 2, 
                                            c("Hombres",etiquetas0)))
mujeres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 3, 
                                            c("Mujeres",etiquetas0)))
tab <- purrr::map(.x =1:4, .f=~rbind(nacional[[.x]], NA, mujeres[[.x]], NA, hombres[[.x]]))

num_hoja <- 87
Escribe_Excel_a(ruta, num_hoja, tab[[1]])


#===============================================================================
#Tabulados de uso de servicios de salud D_193----
cc <- c(paste0('0', 1:9), 10)
for(i in 1:length(cc)) eval(parse(text = paste0("f",i," <- t7$P3_9%in%'",cc[i],"' ")))
lonja <- 10
#--- Nacional
t7$TOT <- ifelse(!t7$P3_9%in%NA, t7$FACTOR, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(f",i,"%in%T, t7$FACTOR, 0)#ninguno
")))
xx <- c('TOT', paste0('TOT_', 1:lonja))
xxx <- c("Estados Unidos Mexicanos", 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_2(xx, 'SEXO', xxx)

#--- indígenas
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A'), t7$FACTOR_PER_i, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_1%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_i, 0)
")))
xxx <- c(etiquetas0[1], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_1 <- Tab_2(xx, 'SEXO', xxx)

#--- afrodescendientes
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A'), t7$FACTOR_PER_a, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$EDAD%in%12:96 & t7$P4_2_01_2%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_a, 0)
")))
xxx <- c(etiquetas0[2], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_2 <- Tab_2(xx, 'SEXO', xxx)

#--- discapacidad
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A'), t7$FACTOR_PER_d, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_02%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_d, 0)
")))
xxx <- c(etiquetas0[3], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_3 <- Tab_2(xx, 'SEXO', xxx)

#--- migrante
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A'), t7$FACTOR_PER_mi, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%15:96 & t7$P4_2_03%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mi, 0)
")))
xxx <- c(etiquetas0[4], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_4 <- Tab_2(xx, 'SEXO', xxx)

#--- diversidad religiosa
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A'), t7$FACTOR_PER_r, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:96 & t7$P4_2_04%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_r, 0)
")))
xxx <- c(etiquetas0[5], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_5 <- Tab_2(xx, 'SEXO', xxx)

#--- mayor 60
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A'), t7$FACTOR_PER_mayor, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%60:96 & t7$P4_2_05%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mayor, 0)
")))
xxx <- c(etiquetas0[6], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_6 <- Tab_2(xx, 'SEXO', xxx)

#--- Adolescentes y jóvenes
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[7], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_7 <- Tab_2(xx, 'SEXO', xxx)

#--- solo adolescentes
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%12:17 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[8], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_8 <- Tab_2(xx, 'SEXO', xxx)

#--- solo jovenes
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A'), t7$FACTOR_PER_adol, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:29 & t7$P4_2_07%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_adol, 0)
")))
xxx <- c(etiquetas0[9], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_9 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[10], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_10 <- Tab_2(xx, 'SEXO', xxx)

#--- Mujeres trabajadoras hogar
t7$TOT <- ifelse(!t7$P3_9%in%NA & t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A'), t7$FACTOR_PER_mu, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') & f",i,"%in%T, t7$FACTOR_PER_mu, 0)
")))
xxx <- c(etiquetas0[11], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_11 <- Tab_2(xx, 'SEXO', xxx)

#--- Diversidad sexual y de género
fil <- t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6)

t7$TOT <- ifelse(fil%in%T & !t7$P3_9%in%NA & t7$P4_2_10%in%c('A'), t7$FACTOR_COE, 0)
for(i in 1:lonja) eval(parse(text = paste0("
t7$TOT_",i,"<-ifelse(fil%in%T & t7$P4_2_10%in%c('A') & f",i,"%in%T, t7$FACTOR_COE, 0)
")))
xxx <- c(etiquetas0[12], 'Hombres', 'Mujeres')
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_12 <- Tab_2(xx, 'SEXO', xxx)

#Arma bolques

nacional<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x, renglon = 1, 
                                             c("Estados Unidos Mexicanos",etiquetas0)))
hombres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 2, 
                                            c("Hombres",etiquetas0)))
mujeres<- purrr::map(.x=1:4, .f =~ pegadora(lista=bla[[.x]], cuantos = 12,  prec = .x,  renglon = 3, 
                                            c("Mujeres",etiquetas0)))
tab <- purrr::map(.x =1:4, .f=~rbind(nacional[[.x]], NA, mujeres[[.x]], NA, hombres[[.x]]))

num_hoja <- 88
Escribe_Excel_a(ruta, num_hoja, tab[[1]])

#===============================================================================
