library(foreign)
library(survey)
source('D:/ENADIS/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
bases <- list()

#INDIGENAS######################################################################
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
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A'), 1, 0)


t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1

b <- c(paste0('0', 1:9), 10:16)

f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'


t7$TOT <- ifelse(!t7$marca_indi%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$marca_indi%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$marca_indi%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para acompletar 
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_indigenas <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )


#AFRODESCENDIENTES##############################################################
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

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c("A"),1,0)
#---
#col 1
b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'
t7$TOT <- ifelse(t7$afro_1%in%1 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_afrodescendientes <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )


#DISCAPACIDAD###################################################################
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
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A'),1, 0)
sum(t7$discapacidad)
# -> Población verificada con discapacidad
#t7$POBF1_1 <- ifelse( t7$FILTRO_2%in%c('1', '2') & t7$PMVD_1%in%'1',1,0)
t7$POBF1_1 <- t7$discapacidad
t7$EDAD <- as.numeric(as.character(t7$EDAD))

#---
#col 1

b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'
t7$TOT <- ifelse(!t7$POBF1_1%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$POBF1_1%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$POBF1_1%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$POBF1_1%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$POBF1_1%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$POBF1_1%in%0 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_discapacidad <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )

#MIGRANTES######################################################################
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
t7$migrante <- ifelse(t7$P4_2_03%in%c('A'), 1, 0)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
b <- c(paste0('0', 1:9), 10:16)

f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'


t7$TOT <- ifelse(!t7$migrante%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$migrante%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$migrante%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$migrante%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$migrante%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$migrante%in%0 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para acompletar 
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_migrantes <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )


#RELIGIÖN#######################################################################
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
t7$religion <- ifelse(t7$P4_2_04%in%c('A'), 1, 0)
#sum(t7$religion)
#---
#col 1
b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'
t7$TOT <- ifelse(!t7$religion%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$religion%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$religion%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$religion%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$religion%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$religion%in%0 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
#---
tab_religion <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )


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
t7$mayor <- ifelse(t7$P4_2_05%in%c('A'), 1, 0)
#sum(t7$mayor)

#---
#col 1
b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'
t7$TOT <- ifelse(!t7$mayor%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$mayor%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$mayor%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$mayor%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$mayor%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$mayor%in%0 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
#---
tab_mayores <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                              tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                              tabulado_extra[[.x]][3:4, ]) )



#Niños##########################################################################
bases[[1]] <- TAB[[13]]
bases[[2]] <- TAB[[2]]
#nino
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:58]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$nino <- ifelse(t7$P4_2_06%in%c('A'), 1, 0)
#sum(t7$nino)
#---
#col 1
casos<- c('Forma de vestir',
          'Manera de hablar o expresarse',
          'Peso o estatura',
          'Ser niña o niño',
          'Colonia o localidad donde vive',
          'Nombre',
          'Tono de piel',
          'No tener los mismos juguetes o aparatos que otras niñas y niños',
          'Otro motivo')

b <- c(1:9)
f0 <- t7$PM6_8_1%in%'1' | t7$PM6_8_2%in%'1' | t7$PM6_8_3%in%'1' | t7$PM6_8_4%in%'1' | t7$PM6_8_5%in%'1' | 
  t7$PM6_8_6%in%'1' | t7$PM6_8_7%in%'1' | t7$PM6_8_8%in%'1' | t7$PM6_8_9%in%'1'
t7$TOT <- ifelse(!t7$nino%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$nino%in%0 & t7$PM6_8_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:9))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$nino%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$nino%in%0 & t7$SEXO%in%'2' & t7$PM6_8_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$nino%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$nino%in%0 & t7$SEXO%in%'1' & t7$PM6_8_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
f0 <- eval(parse(text = paste0("t7$PM6_8_",1:9,"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM6_8_",1:9,"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$nino%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$nino%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$nino%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2' & !t7$nino%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & !t7$nino%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & !t7$nino%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1' & !t7$nino%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & !t7$nino%in%0 & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & !t7$nino%in%0 & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
#---
tab_ninos <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                            tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                            tabulado_extra[[.x]][3:4, ]) )


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
t7$adolesc <- ifelse(t7$P4_2_07%in%c('A'), 1, 0)
#sum(t7$adolesc)
#---
#col 1
casos<- c('Tono de piel',
          'Manera de hablar',
          'Peso o estatura',
          'Forma de vestir o arreglo personal (tatuajes, ropa, forma de peinarse, perforaciones)',
          'Clase social',
          'Lugar donde vive',
          'Creencias religiosas',
          'Mujer (Hombre)',
          'Edad',
          'Preferencia sexual',
          'Ser persona indígena o afrodescendiente',
          'Tener alguna discapacidad',
          'Tener alguna enfermedad',
          'Opiniones políticas',
          'Estado civil o su situación de pareja o familiar (embarazada, huérfana(o) o adoptada(o))',
          'Otro motivo')
b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1' 

t7$TOT<- ifelse(!t7$adolesc%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$adolesc%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$adolesc%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$adolesc%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$adolesc%in%0 & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$adolesc%in%0 & t7$SEXO%in%'1' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
#---
tab_adolescentes <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                                   tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                                   tabulado_extra[[.x]][3:4, ]) )



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
t7$mujer <- ifelse(t7$P4_2_08%in%c('A'), 1, 0)
#sum(t7$mujer)
#---
#col 1
b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
  t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
  t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
  t7$PM9_6_16%in%'1'
t7$TOT <- ifelse(!t7$mujer%in%0 & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$mujer%in%0 & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(!t7$mujer%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$mujer%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(!t7$mujer%in%0 & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(!t7$mujer%in%0 & t7$SEXO%in%'2' & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)
#---
tab_mujeres <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )



#Mujeres TRABAJDORAS DEL HOGAR##################################################
#--- 
f0 <- t7$PM9_6_01%in%'1' | t7$PM9_6_02%in%'1' | t7$PM9_6_03%in%'1' | t7$PM9_6_04%in%'1' | t7$PM9_6_05%in%'1' | 
       t7$PM9_6_06%in%'1' | t7$PM9_6_07%in%'1' | t7$PM9_6_08%in%'1' | t7$PM9_6_09%in%'1' | t7$PM9_6_10%in%'1' |
        t7$PM9_6_11%in%'1' | t7$PM9_6_12%in%'1' | t7$PM9_6_13%in%'1' | t7$PM9_6_14%in%'1' | t7$PM9_6_15%in%'1' |
         t7$PM9_6_16%in%'1'
muj_remunerada <- t7$EDAD%in%18:96 & t7$PM8_8%in%'1' & t7$P4_2_08%in%c('A') 
t7$TOT <- ifelse(muj_remunerada%in%T & f0%in%T , t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(muj_remunerada%in%T & t7$PM9_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab0, tabla_2 = tab0)
#---
f0 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM9_6_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(muj_remunerada%in%T, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(muj_remunerada%in%T & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(muj_remunerada%in%T & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))
tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)

tab_remuneradas <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )




#save.image("D:/ENADIS/Codigos/Tableros/tabs_discrim_motivos.Rdata")
#load("D:/ENADIS/Codigos/Tableros/tabs_discrim_motivos.Rdata")

#COE ######################################################################

t7 <- TAB[[16]]
#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
t7 <- t7[t7$RESUL_POB%in%c('A'), ]

t7$EDAD <- as.numeric(as.character(t7$EDAD))

#---
#col 1
b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PO4_1_01%in%'1' | t7$PO4_1_02%in%'1' | t7$PO4_1_03%in%'1' | t7$PO4_1_04%in%'1' | t7$PO4_1_05%in%'1' | 
  t7$PO4_1_06%in%'1' | t7$PO4_1_07%in%'1' | t7$PO4_1_08%in%'1' | t7$PO4_1_09%in%'1' | t7$PO4_1_10%in%'1' |
  t7$PO4_1_11%in%'1' | t7$PO4_1_12%in%'1' | t7$PO4_1_13%in%'1' | t7$PO4_1_14%in%'1' | t7$PO4_1_15%in%'1' |
  t7$PO4_1_16%in%'1'
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse( t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))


#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10:16),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_COE <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )



#DIVERSIDAD SEXUAL y de GÉNERO##################################################
#Este sale del COE
#col 1

fil <- t7$PO5_2%in%c(1:2,4) | t7$SEX%in%'1' & t7$PO5_3%in%2:6 | t7$SEX%in%'2' & t7$PO5_3%in%c(1,3:6)

b <- c(paste0('0', 1:9), 10:16)
f0 <- t7$PO4_1_01%in%'1' | t7$PO4_1_02%in%'1' | t7$PO4_1_03%in%'1' | t7$PO4_1_04%in%'1' | t7$PO4_1_05%in%'1' | 
  t7$PO4_1_06%in%'1' | t7$PO4_1_07%in%'1' | t7$PO4_1_08%in%'1' | t7$PO4_1_09%in%'1' | t7$PO4_1_10%in%'1' |
  t7$PO4_1_11%in%'1' | t7$PO4_1_12%in%'1' | t7$PO4_1_13%in%'1' | t7$PO4_1_14%in%'1' | t7$PO4_1_15%in%'1' |
  t7$PO4_1_16%in%'1'

t7$TOT <- ifelse(fil%in%T & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(fil%in%T &  t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:16))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(fil%in%T & t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(fil%in%T & t7$SEXO%in%'2' & t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(fil%in%T & t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(fil%in%T & t7$SEXO%in%'1' & t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))


#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
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

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_div <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                          tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                          tabulado_extra[[.x]][3:4, ]) )


#===============================================================================
cc <- c("est", "cv", "int", "err")
grupos <- c("Población indígena de 12 años y más", 
            "Población afrodescendiente de 12 años y más", 
            "Población de 12 años y más con discapacidad",
            "Población migrante de 15 años y más", 
            "Población de diversidad religiosa de 12 años y más",
            "Población de 60 años y más",
            "Población de 12 a 29 años", 
            "Mujeres de 18 años y más",
            "Trabajadoras del hogar",
            "Población de 18 años y más", 
            "Población de 18 años y más con diversidad sexual y de género")

###
#Si experimentó----
tab_22 <- purrr::map(.x =1:4, 
               .f = ~ rbind(tab_indigenas[[.x]][2, ], 
                    tab_afrodescendientes[[.x]][2, ],
                         tab_discapacidad[[.x]][2, ],
                            tab_migrantes[[.x]][2, ],
                             tab_religion[[.x]][2, ],
                              tab_mayores[[.x]][2, ],
                         tab_adolescentes[[.x]][2, ],
                              tab_mujeres[[.x]][2, ],
                          tab_remuneradas[[.x]][2, ],
                                  tab_COE[[.x]][2, ],
                                  tab_div[[.x]][2, ]))
for(i in 1:4){tab_22[[1]][,1] <- grupos} 


##############################################################################
#2017



# INDIGENA #####################################################################
bases <- purrr::map(.x = c(7,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
casos<- c('Tono de piel',
          'Manera de hablar',
          'Peso o estatura',
          'Forma de vestir o arreglo personal (tatuajes)',
          'Clase social',
          'Lugar donde vive',
          'Creencias religiosas',
          'Mujer (Hombre)',
          'Edad',
          'Preferencia sexual',
          'Ser persona indígena o afrodescendiente',
          'Tener alguna discapacidad',
          'Tener alguna enfermedad',
          'Opiniones políticas',
          'Estado civil o su situación de pareja o familiar (embarazada, huérfana(o) o adoptada(o))',
          'Otro motivo')

b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0(" t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para que sea como el de 2022
si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación")
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_indigenas <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                                tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                                tabulado_extra[[.x]][3, ]) )
for(i in c(1,2,4)){
  tab_indigenas[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}


# DISCAPACIDAD #################################################################
bases <- purrr::map(.x = c(4,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1

b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_discapacidad <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                                   tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                                   tabulado_extra[[.x]][3, ]) )
for(i in c(1,2,4)){
  tab_discapacidad[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}



# RELIGION #####################################################################
bases <- purrr::map(.x = c(11,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#religión
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))
#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_religion <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                               tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                               tabulado_extra[[.x]][3, ]) )


for(i in c(1,2,4)){
  tab_religion[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}



# MAYOR ########################################################################
#Mayores
bases <- purrr::map(.x = c(2,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#religión
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))
#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_mayores <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                              tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                              tabulado_extra[[.x]][3, ]) )

for(i in c(1,2,4)){
  tab_mayores[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}



# NIÑOS ########################################################################
#Niños
bases <- purrr::map(.x = c(10,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#niños
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM5_7_", 1:7), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
casos<- c('Forma de vestir',
          'Manera de hablar o expresarse',
          'Peso o estatura',
          'Ser niña o niño',
          'Colonia o localidad donde vive',
          'Gustos o preferencias musicales',
          'Nombre',
          'Tono de piel',
          'No tener los mismos juguetes o aparatos que otras niñas y niños',
          'Otro motivo')
# 
# 
# casos<- c('Forma de vestir',
#           'Manera de hablar o expresarse',
#           'Peso o estatura',
#           'Ser niña o niño',
#           'Colonia o localidad donde vive',
#           'Nombre',
#           'Tono de piel',
#           'No tener los mismos juguetes o aparatos que otras niñas y niños',
#           'Otro motivo')

b <- 1:7
f0 <- t7$PM5_7_1%in%'1' | t7$PM5_7_2%in%'1' | t7$PM5_7_3%in%'1' | t7$PM5_7_4%in%'1' | t7$PM5_7_5%in%'1' | 
  t7$PM5_7_6%in%'1' | t7$PM5_7_7%in%'1'  
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM5_7_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
t7$TOT_8 <- 0.000000001
t7$TOT_9 <- 0.000000001
t7$TOT_10 <- 0.000000001

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM5_7_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
t7$TOT_8 <- 0.000000001
t7$TOT_9 <- 0.000000001
t7$TOT_10 <- 0.000000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM5_7_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
t7$TOT_8 <- 0.000000001
t7$TOT_9 <- 0.000000001
t7$TOT_10 <- 0.000000001
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))
#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM5_7_",1:7,"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM5_7_",1:7,"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM5_7_",1:7,"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_ninos <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                            tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                            tabulado_extra[[.x]][3, ]) )

for(i in c(1,2,4)){
  tab_ninos[[i]][10:12,c(2,3,5,6, 8,9)] <- "ND" 
}



# ADOLESCENTES #################################################################
#Adolescentes
bases <- purrr::map(.x = c(1,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#adolescentes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
casos<- c('Tono de piel',
          'Manera de hablar',
          'Peso o estatura',
          'Forma de vestir o arreglo personal (tatuajes)',
          'Clase social',
          'Lugar donde vive',
          'Creencias religiosas',
          'Mujer (Hombre)',
          'Edad',
          'Preferencia sexual',
          'Ser persona indígena o afrodescendiente',
          'Tener alguna discapacidad',
          'Tener alguna enfermedad',
          'Opiniones políticas',
          'Estado civil o su situación de pareja o familiar (embarazada, huérfana(o) o adoptada(o))',
          'Otro motivo')
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))
#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---

#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_adolescentes <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                                   tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                                   tabulado_extra[[.x]][3, ]) )

for(i in c(1,2,4)){
  tab_adolescentes[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}


# MUJERES ######################################################################
#Mujeres
bases <- purrr::map(.x = c(9,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#mujeres
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", "PM7_7", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#---
#col 1
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Muj)
#---
#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)

#---
tab_mujeres <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                              tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                              tabulado_extra[[.x]][3, ]) )

for(i in c(1,2,4)){
  tab_mujeres[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}



# Mujeres remuneradas ##########################################################
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T & t7$PM7_7%in%'1', t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1'& t7$PM7_7%in%'1', t7$FACTOR_PER, 0)
")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:length(casos)))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T& t7$PM7_7%in%'1', t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1'& t7$PM7_7%in%'1', t7$FACTOR_PER, 0)")))
for(i in 11:16)eval(parse(text = paste0("t7$TOT_",i," <- 0.0000000001")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Muj)
#---

#Parche para que sea como el de 2022
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0& t7$PM7_7%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1& t7$PM7_7%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2& t7$PM7_7%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla, tabla_2 = bla)

#---
tab_remuneradas <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                                  tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                                  tabulado_extra[[.x]][3, ]) )

for(i in c(1,2,4)){
  tab_remuneradas[[i]][13:18,c(2,3,5,6, 8,9)] <- "ND" 
}

# COE ##########################################################################
bases <- purrr::map(.x = c(3,3), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
t7 <- bases[[1]]
casos <- c("Tono de piel",
           "Manera de hablar",
           "Peso o estatura",
           "Forma de vestir o arreglo personal (tatuajes)",
           "Clase social",
           "Lugar donde vive",
           "Creencias religiosas",
           "Mujer (hombre)",
           "Edad",
           "Preferencia sexual")

#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))

t7$EDAD <- as.numeric(as.character(t7$EDAD))

#---
#col 1
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PO4_1_01%in%'1' | t7$PO4_1_02%in%'1' | t7$PO4_1_03%in%'1' | t7$PO4_1_04%in%'1' | t7$PO4_1_05%in%'1' | 
  t7$PO4_1_06%in%'1' | t7$PO4_1_07%in%'1' | t7$PO4_1_08%in%'1' | t7$PO4_1_09%in%'1' | t7$PO4_1_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse( t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:10))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PO4_1_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))


#---
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#---
#Parche para acompletar 
f0 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10),"%in%'1'", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PO4_1_",c(paste0('0',1:9),10),"%in%c('2')", collapse = "&")))

si_y_no <- c("Sí experimentó discriminación", "No experimentó discriminación", "No especificado")
t7$TOT <- ifelse(!t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2

asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'2' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

t7$TOT <- ifelse( t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <-ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
t7$TOT_2 <-ifelse(t7$SEXO%in%'1' & f1%in%T, t7$FACTOR_PER, 0)
t7$TOT_3 <- t7$TOT-t7$TOT_1-t7$TOT_2
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(c("TOT", "TOT_1", "TOT_2", "TOT_3"), c('Estados Unidos Mexicanos', si_y_no))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#---
tab_COE <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                          tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                          tabulado_extra[[.x]][3:4, ]) )


#===============================================================================
grupos <- c("Personas indígenas (12 años y más)", 
            "Personas con discapacidad (12 años y más)",
            "Personas de la diversidad religiosa (12 años y más)",
            "Personas mayores (60 años y más)",
            "Adolescentes y jóvenes (de 12 a 29 años)", 
            "Mujeres (de 18 años y más)", 
            "Trabajadoras del hogar remuneradas (de 18 años y más)",
            "Población de 18 años y más")

grupos_2 <- c("Población indígena de 12 años y más", 
              "Población afrodescendiente de 12 años y más", 
              "Población de 12 años y más con discapacidad",
              "Población migrante de 15 años y más", 
              "Población de diversidad religiosa de 12 años y más",
              "Población de 60 años y más",
              "Población de 12 a 29 años", 
              "Mujeres de 18 años y más",
              "Trabajadoras del hogar",
              "Población de 18 años y más", 
              "Población de 18 años y más con diversidad sexual y de género")

###

#Si experimentó----
tab_17 <- purrr::map(.x =1:4, 
                     .f = ~ rbind(tab_indigenas[[.x]][2,], 
                                  NA,
                                  tab_discapacidad[[.x]][2,],
                                  NA,
                                  tab_religion[[.x]][2,],
                                  tab_mayores[[.x]][2,],
                                  tab_adolescentes[[.x]][2,],
                                  tab_mujeres[[.x]][2,],
                                  tab_remuneradas[[.x]][2,],
                                  tab_COE[[.x]][2,],
                                  NA))
for(i in 1:4){tab_17[[i]][,1] <- grupos_2} 



#===============================================================================
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/hace_ph.R')

aa <- purrr::map(.x = c(3,6,9), .f = ~hace_df(tab_17, tab_22, columna = .x))
pa_ph <- purrr::map(.x = 1:2, .f= ~rbind(aa[[1]][[.x]], NA, aa[[2]][[.x]], NA, aa[[3]][[.x]]))
df <- ph_a(pa_ph[[1]])

aa1 <- c('Estados Unidos Mexicanos', rep(NA, 7))
aa2 <- c('Mujeres', rep(NA, 7))
aa3 <- c('Hombres', rep(NA, 7))

df <- rbind(aa1, df[1:12, ], aa2, df[13:24, ], aa3, df[25:dim(df)[1],])

cc <- c("Población afrodescendiente de 12 años y más",
        "Población migrante de 15 años y más",
        "Población de 18 años y más con diversidad sexual y de género")

df[which(df[,1]%in%cc), c(2,6)] <- "ND"
df[which(df[,1]%in%cc), c(4,8)] <- "NA"

escribe_a(ruta="D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx", 
          nom_hoja = "d169", 
          df = df, 
          cv = pa_ph[[2]])








 