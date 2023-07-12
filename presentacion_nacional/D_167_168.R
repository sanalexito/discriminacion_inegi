library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)

bases <- list()

# INDIGENA =====================================================================

bases[[1]] <- TAB[[3]]
bases[[2]] <- TAB[[2]]
#indigena
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:153]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A'), 1, 0)

#---

etiquetas0 <- c('Falta de atención médica',
                'Falta de empleo',
                'Falta de oportunidades para seguir estudiando',
                'Falta de recursos económicos (comer, vestir)',
                'Discriminación por su apariencia',
                'Discriminación por hablar una lengua indígena',
                'Falta de apoyo del gobierno en programas sociales',
                'Falta de respeto a sus costumbres y tradiciones',
                'Otro')
#col 1
t7$TOT <- ifelse( t7$marca_indi%in%1 , t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$marca_indi%in%1 & t7$PM1_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:9))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(t7$marca_indi%in%1  & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$marca_indi%in%1 & t7$PM1_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col3
t7$TOT <- ifelse( t7$marca_indi%in%1& t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$marca_indi%in%1 & t7$PM1_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

tab_ind <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- tab_ind[[1]][c(1, order(tab_ind[[1]][2:dim(tab_ind[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){tab_ind[[i]] <-tab_ind[[i]][rownames(indice),]}


# AFRODESCENDIENTES ============================================================
bases[[1]] <- TAB[[5]]
bases[[2]] <- TAB[[2]]
#Afrodescendientes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:159]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$FACTOR_PER <- ifelse(t7$FACTOR_PER%in%NA, 0, t7$FACTOR_PER)
t7$EDAD <- as.numeric(as.character(t7$EDAD))

t7$afro_1 <- ifelse(t7$P4_2_01_2%in%c('A'),1,0)
#---
etiquetas0 <- c('Falta de atención médica',
                'Falta de empleo',
                'Falta de oportunidades para seguir estudiando',
                'Falta de recursos económicos (comer, vestir)',
                'Discriminación por su apariencia', 
                'Falta de apoyo del gobierno en programas sociales', 
                'Falta de respeto a sus costumbres y tradiciones', 
                'Detención por parte de la policía, el ejército o personal de migración sin justificación alguna',
                'Otro')
#col 1
t7$TOT <- ifelse( t7$afro_1%in%1, t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_9%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:9))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(  t7$afro_1%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_9%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col3
t7$TOT <- ifelse(  t7$afro_1%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$afro_1%in%1 & t7$PM1_9%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_1 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- bla_1[[1]][c(1, order(bla_1[[1]][2:dim(bla_1[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_1[[i]] <-bla_1[[i]][rownames(indice),]}


# DISCAPACIDAD =================================================================
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
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A'),1, 0)
sum(t7$discapacidad)
# -> Población verificada con discapacidad
#t7$POBF1_1 <- ifelse( t7$FILTRO_2%in%c('1', '2') & t7$PMVD_1%in%'1',1,0)
t7$POBF1_1 <- t7$discapacidad

#---
etiquetas0 <- c('Calles, instalaciones y transportes inadecuados a sus condiciones',
                'Costo en cuidados, terapias y tratamientos',
                'Discriminación por su apariencia',
                'Ser excluidas o rechazadas en la toma de decisiones',
                'Falta de oportunidades para encontrar empleo',
                'Falta de un trato justo ante la autoridad',
                'Falta de oportunidades para seguir estudiando',
                'Falta de un sistema de apoyo para una vida independiente',
                'Otro')

#col 1
t7$TOT <- ifelse(t7$POBF1_1%in%1, t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$POBF1_1%in%1 & t7$PM2_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:9))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

#Col2
t7$TOT <- ifelse(t7$POBF1_1%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$POBF1_1%in%1 &t7$PM2_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:9))
tab_2 <- Tab_vert_a(xx, c("Mujeres",etiquetas0))

#Col3
t7$TOT <- ifelse(t7$POBF1_1%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:9)eval(parse(text = paste0("t7$TOT_",i," <- ifelse(t7$POBF1_1%in%1 &t7$PM2_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:9))
tab_3 <- Tab_vert_a(xx, c("Hombres",etiquetas0))


bla_2 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- bla_2[[1]][c(1, order(bla_2[[1]][2:dim(bla_2[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_2[[i]] <-bla_2[[i]][rownames(indice),]}


# MIGRANTES ====================================================================
bases[[1]] <- TAB[[9]]
bases[[2]] <- TAB[[2]]
#Migrantes
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#---
estois <- names(bases[[1]])[5:151]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$migrante <- ifelse(t7$P4_2_03%in%c('A') , 1, 0)
#---
etiquetas0 <- c('Falta de empleo',
                'Falta de recursos económicos (comer, vestir)',
                'Discriminación por venir de otro lugar',
                'Abuso de las autoridades',
                'Falta de respeto a sus costumbres y tradiciones',
                'Violencia y hostigamiento',
                'Falta de atención de las autoridades migratorias',
                'Otro')
#col 1
t7$TOT <- ifelse( t7$migrante%in%1 , t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$migrante%in%1 & t7$PM3_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:8))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse( t7$migrante%in%1  & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$migrante%in%1 & t7$PM3_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col3
t7$TOT <- ifelse( t7$migrante%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$migrante%in%1 & t7$PM3_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_3 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- bla_3[[1]][c(1, order(bla_3[[1]][2:dim(bla_3[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_3[[i]] <- bla_3[[i]][rownames(indice),]}


# RELIGION =====================================================================
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
t7$religion <- ifelse(t7$P4_2_04%in%'A', 1, 0)
#---

etiquetas0 <- c('Desigualdad de trato en el trabajo o escuela',
                'Son rechazadas o aisladas por la sociedad',
                'Falta de oportunidad para mostrar sus creencias a la sociedad',
                'Falta de respeto a sus costumbres y tradiciones',
                'El gobierno apoya más a la comunidad católica',
                'Otro')

#col 1
t7$TOT <- ifelse( t7$religion%in%1, t7$FACTOR_PER, 0)
for(i in 1:6)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$religion%in%1 & t7$PM4_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:6))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(t7$religion%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:6)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$religion%in%1 & t7$PM4_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col3
t7$TOT <- ifelse( t7$religion%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:6)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$religion%in%1 & t7$PM4_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_4 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- bla_4[[1]][c(1, order(bla_4[[1]][2:dim(bla_4[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_4[[i]] <- bla_4[[i]][rownames(indice),]}




# MAYORES DE 60 ================================================================

bases[[1]] <- TAB[[12]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")

etiquetas0 <- c("Su pensión no alcanza para cubrir sus necesidades básicas",
                 "Falta de oportunidades para encontrar trabajo",
                 "Depender económicamente de sus familiares",
                 "Carecer de acceso a servicios de salud",
                 "Maltrato, abandono o despojo de sus bienes",
                 "Despojo o robo de sus bienes (casa, terreno, joyas)",
                 "Carecer de pensión o jubilación",
                 "Otro")
#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mayor <- ifelse(t7$P4_2_05%in%'A', 1, 0)

#col 1
t7$TOT <- ifelse( t7$mayor%in%1, t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mayor%in%1 & t7$PM5_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:8))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(t7$mayor%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mayor%in%1 & t7$PM5_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col3
t7$TOT <- ifelse( t7$mayor%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mayor%in%1 & t7$PM5_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_5 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- bla_5[[1]][c(1, order(bla_5[[1]][2:dim(bla_5[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_5[[i]] <- bla_5[[i]][rownames(indice),]}


# ADOLESCENTES =================================================================
bases[[1]] <- TAB[[14]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")

etiquetas0 <- c("Falta de empleo",
                "Falta de oportunidades para seguir estudiando",
                "Adicciones (alcohol, tabaco, droga)",
                "Violencia e inseguridad",
                "Embarazo en la adolescencia",
                "Acoso escolar o bullying",
                "Problemas personales o familiares",
                "Otro")
                
#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$adol <- ifelse(t7$P4_2_07%in%'A', 1, 0)

#col 1
t7$TOT <- ifelse( t7$adol%in%1, t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$adol%in%1 & t7$PM7_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:8))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(t7$adol%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$adol%in%1 & t7$PM7_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col3
t7$TOT <- ifelse( t7$adol%in%1 & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
for(i in 1:8)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$adol%in%1 & t7$PM7_2%in%",i," & t7$SEXO%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_3 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_6 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
indice <- bla_6[[1]][c(1, order(bla_6[[1]][2:dim(bla_6[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_6[[i]] <- bla_6[[i]][rownames(indice),]}


# MUJERES ======================================================================
bases[[1]] <- TAB[[15]]
bases[[2]] <- TAB[[2]]
#Religiosa
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%'A'), ]
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")

etiquetas0 <- c("Delincuencia e inseguridad",
                "Desigualdad entre hombres y mujeres",
                "Falta de oportunidades para encontrar trabajo",
                "Machismo",
                "Dificultades para combinar vida familiar y laboral",
                "Violencia hacia las mujeres",
                "Otro")

#---
estois <- names(bases[[1]])[5:139]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$mujer <- ifelse(t7$P4_2_08%in%'A', 1, 0)

#col 1
t7$TOT <- ifelse( t7$mujer%in%1, t7$FACTOR_PER, 0)
for(i in 1:7)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_2%in%",i,", t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:7))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(t7$mujer%in%1 & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:7)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_2%in%",i," & t7$SEXO%in%'2', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_7 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_2)
indice <- bla_7[[1]][c(1, order(bla_7[[1]][2:dim(bla_7[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_7[[i]] <- bla_7[[i]][rownames(indice),]}


# MUJERES REMUNERADAS ==========================================================
etiquetas0 <- c("Maltrato o abuso de las personas que las contratan (empleador/a)",
                              "Malas condiciones de trabajo (horarios extensos, bajo salario)",
                              "Falta de prestaciones laborales (seguro, aguinaldo, vacaciones pagadas, etcétera)",
                              "Otro")

t7$TOT <- ifelse( t7$mujer%in%1 & t7$PM8_8%in%'1', t7$FACTOR_PER, 0)
for(i in 1:4)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_10%in%",i," & t7$PM8_8%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:4))
tab_1 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))
#Col2
t7$TOT <- ifelse(t7$mujer%in%1 & t7$PM8_8%in%'1' & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
for(i in 1:4)eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(t7$mujer%in%1 & t7$PM8_10%in%",i," & t7$SEXO%in%'2'  & t7$PM8_8%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_2 <- Tab_vert_a(xx, c("Estados Unidos Mexicanos",etiquetas0))

bla_8 <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_2)
indice <- bla_8[[1]][c(1, order(bla_8[[1]][2:dim(bla_8[[1]])[1],5],decreasing = TRUE)+1),]
for(i in 1:4){bla_8[[i]] <- bla_8[[i]][rownames(indice),]}


#===============================================================================
nombres0 <- list()
nombres0[[1]] <- c("Población indígena de 12 años y más", rep(NA, 8))
nombres0[[2]] <- c("Población afrodescendiente de 12 años y más", rep(NA, 8))
nombres0[[3]] <- c("Población de 12 años y más con discapacidad", rep(NA, 8))
nombres0[[4]] <- c("Población migrante de 15 años y más", rep(NA, 8))
nombres0[[5]] <- c("Población de la diversidad religiosa de 12 años y más", rep(NA, 8))
nombres0[[6]] <- c("Población de 60 años y más", rep(NA, 8))
nombres0[[7]] <- c("Población de adolescentes y jóvenes", rep(NA, 8))
nombres0[[8]] <- c("Mujeres de 18 años y más", rep(NA, 8))
nombres0[[9]] <- c("Trabajadoras del hogar remuneradas de 18 años y más", rep(NA, 8))

nombres_1 <- c(tab_ind[[1]][2,1])
for(i in 1:8)eval(parse(text = paste0(" nombres_1 <- c(nombres_1, bla_",i,"[[1]][2,1]) ")))

tab_22 <- purrr::map(.x = 1:4, 
                     .f=~ pegadora(list=tab_ind[[.x]], cuantos = 8, prec =.x , renglon = 2, nombres_1, columna = 3))
for(i in c(1, 2, 4)){
tab_22[[1]][c(8,9), c(8,9)] <- NA
}
tab_22[[3]][c(8,9), c(14:18)] <- NA

bb <- list()
for(i in 1:4){
bb[[i]] <- purrr::map(.x = 1:9, .f =~rbind(nombres0[[.x]], tab_22[[i]][.x,]))
}  


tab_22 <- purrr::map(.x = 1:4, .f=~ do.call(rbind, bb[[.x]]))
#-------------------------------------------------------------------------------
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 75
Escribe_Excel_a(ruta, num_hoja, tab_22[[1]])
