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

#-------------------------------------------------------------------------------
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
tab_22 <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][1:2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],],
                                           tabulado_extra[[.x]][3:4, ]) )
tab_22 <- purrr::map(.x = 1:4, .f = ~tab_22[[.x]][-c(1,19:20),])



#===============================================================================
#2017
nombre_bases <- dir('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/')
bases <- purrr::map(.x = c(4,13), .f = ~ read.dbf(paste0('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Base/Bases_datos/', nombre_bases[[.x]])))
#discapacidad
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PM8_6_", c(paste0(0, 1:9),10)), "FACTOR_PER")], by = "ID_PER", all.x = T)
t7$EDAD <- as.numeric(as.character(t7$EDAD))
#-------------------------------------------------------------------------------
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
          'Preferencia sexual')
b <- c(paste0('0', 1:9), 10)
f0 <- t7$PM8_6_01%in%'1' | t7$PM8_6_02%in%'1' | t7$PM8_6_03%in%'1' | t7$PM8_6_04%in%'1' | t7$PM8_6_05%in%'1' | 
  t7$PM8_6_06%in%'1' | t7$PM8_6_07%in%'1' | t7$PM8_6_08%in%'1' | t7$PM8_6_09%in%'1' | t7$PM8_6_10%in%'1' 
t7$TOT <- ifelse( f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)
")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:10))
tab0 <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

#col 2 y3 
t7$TOT <- ifelse(t7$SEXO%in%'2' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'2' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:10))
tab_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(t7$SEXO%in%'1' & f0%in%T, t7$FACTOR_PER, 0)
for(i in 1:length(b))eval(parse(text = paste0("   
t7$TOT_",i," <- ifelse(t7$SEXO%in%'1' & t7$PM8_6_",b[i],"%in%'1', t7$FACTOR_PER, 0)")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
xx <-c('TOT', paste0('TOT_',1:10))
tab_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))
#-------------------------------------------------------------------------------
tabulado0 <- pega_tab_1(tabla_0 = tab0, tabla_1 = tab_Muj, tabla_2 = tab_Hom)
#-------------------------------------------------------------------------------

#Parche para que sea como el de 2022
casos <- c("Con discriminación", "Sin discriminación")
xx <- c("TOT", "TOT_1", "TOT_2")

f0 <- eval(parse(text = paste0("!t7$PM8_6_",c(paste0(0,1:9), 10),"%in%NA", collapse = "|")))
f1 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%'1'", collapse = "|")))
f2 <- eval(parse(text = paste0("t7$PM8_6_",c(paste0(0,1:9), 10),"%in%c('2', '8')", collapse = "&")))

t7$TOT <- ifelse(f0, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(f2, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Muj <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

t7$TOT <- ifelse(f0%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1<-ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_2<-ifelse(f2%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_Hom <- Tab_vert_a(xx, c('Estados Unidos Mexicanos', casos))

tabulado_extra <- pega_tab_1(tabla_0 = bla, tabla_1 = bla_Muj, tabla_2 = bla_Hom)

#-------------------------------------------------------------------------------
tab_17 <- purrr::map(.x = 1:4, .f=~rbind(tabulado_extra[[.x]][2, ],
                                           tabulado0[[.x]][2:dim(tabulado0[[.x]])[1],] ))

tab_17 <- purrr::map(.x =1:4, .f= ~ rbind(tab_17[[.x]], NA, NA,NA, NA,NA, NA))
casos <- c('Estados Unidos Mexicanos',
  'Tono de piel',
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
for(i in 1:4) {
  tab_17[[i]][,1] <- casos
}

#===============================================================================
source('D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Calculadores/hace_ph.R')

aa <- purrr::map(.x = c(3,6,9), .f = ~hace_df(tab_17, tab_22, columna = .x))
pa_ph <- purrr::map(.x = 1:2, .f= ~rbind(aa[[1]][[.x]], NA, aa[[2]][[.x]], NA, aa[[3]][[.x]]))
df <- ph_a(pa_ph[[1]])
df[grep("Estados Unidos Mexicanos", df[,1])[2],1] <- "Mujeres"
df[grep("Estados Unidos Mexicanos", df[,1])[2],1] <- "Hombres"


df[which(df[,1]%in%cc), c(2,6)] <- "ND"
df[which(df[,1]%in%cc), c(4,8)] <- "NA"

escribe_a(ruta="D:/ENADIS/Entregable_enadis_2022/SCRIPTS_ENADIS_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx", 
          nom_hoja = "d74", 
          df = df, 
          cv = pa_ph[[2]])


