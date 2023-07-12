library(foreign)
library(survey)
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
bases <- list()
bases[[1]] <- TAB[[7]]
bases[[2]] <- TAB[[2]]
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".")
bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".")
#===============================================================================
estois <- names(bases[[1]])[5:160]
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois,  "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$discapacidad <- ifelse(t7$P4_2_02%in%c('A'),1, 0)
sum(t7$discapacidad)
t7$POBF1_1 <- t7$discapacidad

t7$EDAD <- as.numeric(as.character(t7$EDAD))
#-------------------------------------------------------------------------------
etiquetas0 <- c('Sí le ha sucedido',
                'No le ha sucedido')
#col 1
casos<- c("Le rechacen o excluyan de actividades sociales",
          "Le hagan sentir o miren de forma incómoda",
          "Le insulten, se burlen o le hayan dicho cosas que le molestaran",
          "Le amenacen, empujen o jaloneen",
          "Le obliguen a salir de alguna comunidad",
          "Le interrogue o detenga la policía sin razón alguna",
          "Alguna persona le haya agredido intencionalmente",
          "A propósito le dañaron o vandalizaron algún bien o propiedad")

for(i in 1:8)eval(parse(text = paste0("   
t7$TOT <- ifelse(!t7$POBF1_1%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$POBF1_1%in%0 & t7$PM9_4_",i,"%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$POBF1_1%in%0 & t7$PM9_4_",i,"%in%c('2', '3'), t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab0_",i," <- Tab_vert_a(xx, c(casos[",i,"],etiquetas0))
")))

#col 2 y3 
for(j in 1:2)
  for(i in 1:8)eval(parse(text = paste0("   
t7$TOT <- ifelse(  !t7$POBF1_1%in%0 & t7$SEXO%in%",j,", t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$POBF1_1%in%0 & t7$SEXO%in%",j," & t7$PM9_4_",i,"%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$POBF1_1%in%0 & t7$SEXO%in%",j," & t7$PM9_4_",i,"%in%c('2', '3'), t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab",j,"_",i," <- Tab_vert_a(xx, c(casos[",i,"],etiquetas0))
")))

#-------------------------------------------------------------------------------
nacional <- pega_tab_1(tabla_0 = tab0_1, tabla_1 = tab2_1, tabla_2 = tab1_1, nacional = T)
nacional <- purrr::map(.x = 1:4, .f = ~nacional[[.x]][1,])
for(i in 1:8)eval(parse(text = paste0("
bloque",i," <- pega_tab_1(tabla_0 = tab0_",i,", tabla_1 = tab2_",i,", tabla_2 = tab1_",i,")")))

for(j in 1:4) for(i in 1:8) eval(parse(text = paste0(" 
bloque",i,"[[j]][1,2:dim(bloque",i,"[[j]])[2]] <- NA ")))

tabulado <- purrr::map(.x =1:4, .f= ~rbind(nacional[[.x]])) 
for( j in 1:4) for(i in 1:8)
  eval(parse(text = paste0("
tabulado[[j]] <- rbind(tabulado[[j]], NA, bloque",i,"[[j]]) ")))
tabulado <- purrr::map(.x = 1:4, .f=~tabulado[[.x]][-2,])
#-------------------------------------------------------------------------------
source("D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/escribe_excel_a.R")

ruta <- "D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx"
num_hoja <- 43
Escribe_Excel_a(ruta, num_hoja, tabulado[[1]])