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

#-------------------------------------------------------------------------------
tabulado <- pega_tab_1(tabla_0 = tab_1, tabla_1 = tab_2, tabla_2 = tab_3)
tabulado[[3]] <- asteriscos_int_a(tabulado[[3]])

#-------------------------------------------------------------------------------
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 18
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])