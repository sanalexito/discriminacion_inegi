library(foreign)
library(survey)
 source('~/Tab_vertical.R')
 source('~/Pega_tab_1.R')
 source('~/Tab_2.R')
 options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
 load('D:/ENADIS/Base/Bases_datos/vistas_2022/ENADIS_2022_V9_2023-02-16_V00.Rdata')
 nombres_bases <- names(TAB)
 bases <- list()
 bases[[1]] <- TAB[[3]]
 bases[[2]] <- TAB[[2]]
 #indigena
 bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
 bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
 #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
 #tsdem
 bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
 #===============================================================================
 estois <- names(bases[[1]])[5:153]
 t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
 t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A','B'), 1, 0)

#===============================================================================
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

#-------------------------------------------------------------------------------
tabulado <- pega_tab_1(tabla_0 = edu_0, tabla_1 = Mujeres_0, tabla_2 = Hombres_0)
#------------------------------------------------------------------------------
