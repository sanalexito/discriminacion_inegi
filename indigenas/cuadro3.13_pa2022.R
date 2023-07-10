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
# t7$marca_indi <- ifelse(t7$P4_2_01_1%in%c('A'), 1, 0)
t7 <- B_S3


t7$EDAD <- as.numeric(as.character(t7$EDAD))
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
tabulado <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )

#-------------------------------------------------------------------------------

source("D:/ENADIS/Calculadores/escribe_excel_a.R")
dir <- "D:/ENADIS/Tabs/pubs_2022/S3/"
num_hoja <- 14
archivos <- c('III_poblacion_indigena_2022_est.xlsx', 'III_poblacion_indigena_2022_cv.xlsx',
              'III_poblacion_indigena_2022_int.xlsx', 'III_poblacion_indigena_2022_err.xlsx')
purrr::map(.x = 1:length(archivos), .f =~ Escribe_Excel_a(paste0(dir, archivos[.x]), num_hoja, tabulado[[.x]]))

