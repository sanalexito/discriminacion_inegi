# library(foreign)
# library(survey)
# source('D:/ENADIS/Calculadores/Tab_vertical.R')
# source('D:/ENADIS/Calculadores/Pega_tab_1.R')
# source('D:/ENADIS/Calculadores/Tab_2.R')
# options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
# load('D:/ENADIS/Base/Bases_datos/vistas_2022/ENADIS_2022_V9_2023-02-16_V00.Rdata')
# nombres_bases <- names(TAB)
# bases <- list()
# bases[[1]] <- TAB[[3]]
# bases[[2]] <- TAB[[2]]
# #indigena
# bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL,bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
# bases[[1]]$FACTOR_PER <- as.numeric(as.character(bases[[1]]$FACTOR))
# #bases[[1]] <- bases[[1]][bases[[1]]$RESUL_POB%in%c('A','B'), ]
# #tsdem
# bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
# #===============================================================================
# estois <- names(bases[[1]])[5:153]
# t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", estois, "FACTOR_PER")], by = "ID_PER", all.x = T)
# t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A','B'), 1, 0)
t7 <- B_S3


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
t7$TOT <- ifelse(!t7$marca_indi%in%0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & t7$PM9_4_",i,"%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & t7$PM9_4_",i,"%in%c('2', '3'), t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:2))
tab0_",i," <- Tab_vert_a(xx, c(casos[",i,"],etiquetas0))
")))

#col 2 y3 
for(j in 1:2)
  for(i in 1:8)eval(parse(text = paste0("   
t7$TOT <- ifelse(  !t7$marca_indi%in%0 & t7$SEXO%in%",j,", t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & t7$SEXO%in%",j," & t7$PM9_4_",i,"%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & t7$SEXO%in%",j," & t7$PM9_4_",i,"%in%c('2', '3'), t7$FACTOR_PER, 0)
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
source("D:/ENADIS/Calculadores/escribe_excel_a.R")
dir <- "D:/ENADIS/Tabs/pubs_2022/S3/"
num_hoja <- 12
archivos <- c('III_poblacion_indigena_2022_est.xlsx', 'III_poblacion_indigena_2022_cv.xlsx',
              'III_poblacion_indigena_2022_int.xlsx', 'III_poblacion_indigena_2022_err.xlsx')
purrr::map(.x = 1:length(archivos), .f =~ Escribe_Excel_a(paste0(dir, archivos[.x]), num_hoja, tabulado[[.x]]))
