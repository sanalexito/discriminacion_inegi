library(foreign)
library(survey)
source('D:/ENADIS/Calculadores/Tab_vertical.R')
source('D:/ENADIS/Calculadores/Pega_tab_1.R')
source('D:/ENADIS/Calculadores/Tab_2.R')
options("survey.lonely.psu"="adjust", digits = 9, scipen = 999)
load('D:/ENADIS/Entregable_enadis_2022/Base/ENADIS_2022_V9_2_2023-02-16_V00.Rdata')
nombres_bases <- names(TAB)
t7 <- TAB[[16]]
#COE
t7$ID_PER <- paste(t7$UPM, t7$VIV_SEL,t7$HOGAR, t7$N_REN, sep = ".")
t7$FACTOR_PER <- as.numeric(as.character(t7$FACTOR))
t7 <- t7[t7$RESUL_POB%in%c('A'), ]

t7$ENT_a <- as.numeric(as.character(t7$ENT))
t7$EDAD_a <- as.numeric(as.character(t7$EDAD))

estados <- c("Estados Unidos Mexicanos","Aguascalientes","Baja California","Baja California Sur","Campeche","Coahuila de Zaragoza","Colima","Chiapas","Chihuahua",
             "Ciudad de México","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","Estado de México","Michoacán de Ocampo","Morelos","Nayarit","Nuevo León",
             "Oaxaca","Puebla","Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz de Ignacio de la Llave","Yucatán","Zacatecas")

#-------------------------------------------------------------------------------

f00 <- eval(parse(text = paste0("!t7$PO4_1_0",1:9 ,"%in%NA", collapse = "|")))
f01 <- eval(parse(text = paste0("!t7$PO4_1_",10:16,"%in%NA", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT <- ifelse(f1%in%T, t7$FACTOR_PER, 0)

f00 <- eval(parse(text = paste0("t7$PO4_1_0",1:9 ,"%in%'1'", collapse = "|")))
f01 <- eval(parse(text = paste0("t7$PO4_1_",10:16,"%in%'1'", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT_1 <- ifelse(f1%in%T, t7$FACTOR_PER, 0)

#Tasas totales
xx <- c('TOT', paste0('TOT_',1:1))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_22<- Tab_2(xx, "ENT_a", estados)

#hombres---
f00 <- eval(parse(text = paste0("!t7$PO4_1_0",1:9 ,"%in%NA", collapse = "|")))
f01 <- eval(parse(text = paste0("!t7$PO4_1_",10:16,"%in%NA", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT <- ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)

f00 <- eval(parse(text = paste0("t7$PO4_1_0",1:9 ,"%in%'1'", collapse = "|")))
f01 <- eval(parse(text = paste0("t7$PO4_1_",10:16,"%in%'1'", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT_1 <- ifelse(f1%in%T & t7$SEXO%in%'1', t7$FACTOR_PER, 0)

#Tasas totales
xx <- c('TOT', paste0('TOT_',1:1))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_22_h<- Tab_2(xx, "ENT_a", estados)


#mujeres---
f00 <- eval(parse(text = paste0("!t7$PO4_1_0",1:9 ,"%in%NA", collapse = "|")))
f01 <- eval(parse(text = paste0("!t7$PO4_1_",10:16,"%in%NA", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT <- ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)

f00 <- eval(parse(text = paste0("t7$PO4_1_0",1:9 ,"%in%'1'", collapse = "|")))
f01 <- eval(parse(text = paste0("t7$PO4_1_",10:16,"%in%'1'", collapse = "|")))
f1 <- f00%in%T | f01%in%T
t7$TOT_1 <- ifelse(f1%in%T & t7$SEXO%in%'2', t7$FACTOR_PER, 0)

#Tasas totales
xx <- c('TOT', paste0('TOT_',1:1))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_22_m<- Tab_2(xx, "ENT_a", estados)

#===============================================================================
#2017

nombre_bases <- dir('D:/ENADIS/Entregable_enadis_2022/Base/Bases_datos/')
bases <- purrr::map(.x = c(3,13), .f = ~ read.dbf(paste0('D:/ENADIS/Base/Bases_datos/', nombre_bases[[.x]])))
#COE
bases[[1]]$ID_PER <- paste(bases[[1]]$UPM, bases[[1]]$VIV_SEL, bases[[1]]$HOGAR, bases[[1]]$N_REN, sep = ".") 
bases[[1]]$ENT_a <- as.numeric(as.character(bases[[1]]$ENT))

#tsdem
bases[[2]]$ID_PER <- paste(bases[[2]]$UPM, bases[[2]]$VIV_SEL, bases[[2]]$HOGAR, bases[[2]]$N_REN, sep = ".") 
t7 <- merge(bases[[2]], bases[[1]][,c("ID_PER", paste0("PO4_1_0", 1:9), "PO4_1_10", "FACTOR_PER","ENT_a")], by = "ID_PER", all.x = T)
t7$ENT <- as.numeric(as.character(t7$ENT))

f0 <- eval(parse(text = paste0("t7$PO4_1_0",1:9,"%in%'1'", collapse = "|")))
f0 <- f0%in%T | t7$PO4_1_10%in%'1'

#Tasas totales
xx <-c('TOT', paste0('TOT_',1:1))
t7$TOT <- ifelse( !t7$FACTOR_PER%in%NA, t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0 , t7$FACTOR_PER , 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_17 <- Tab_2(xx, "ENT_a", estados)


#hombres---
f0 <- eval(parse(text = paste0("t7$PO4_1_0",1:9,"%in%'1'", collapse = "|")))
f0 <- f0%in%T | t7$PO4_1_10%in%'1'

#Tasas totales
xx <-c('TOT', paste0('TOT_',1:1))
t7$TOT <- ifelse( !t7$FACTOR_PER%in%NA & t7$SEXO%in%'1', t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0 & t7$SEXO%in%'1' , t7$FACTOR_PER , 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_17_h <- Tab_2(xx, "ENT_a", estados)


#mujeres---
f0 <- eval(parse(text = paste0("t7$PO4_1_0",1:9,"%in%'1'", collapse = "|")))
f0 <- f0%in%T | t7$PO4_1_10%in%'1'

#Tasas totales
xx <-c('TOT', paste0('TOT_',1:1))
t7$TOT <- ifelse( !t7$FACTOR_PER%in%NA & t7$SEXO%in%'2', t7$FACTOR_PER, 0)
t7$TOT_1 <- ifelse(f0 & t7$SEXO%in%'2' , t7$FACTOR_PER , 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
tab_17_m <- Tab_2(xx, "ENT_a", estados)

#===============================================================================
#===============================================================================
source('D:/ENADIS/Entregable_enadis_2022/Calculadores/hace_ph.R')
dfs <- list()
aa <- purrr::map(.x = c(5), .f = ~hace_df(tab_17, tab_22, columna = .x))
pa_ph <- purrr::map(.x = 1:2, .f= ~rbind(aa[[1]][[.x]]))
dfs[[1]] <- ph_a(pa_ph[[1]])

aa <- purrr::map(.x = c(5), .f = ~hace_df(tab_17_h, tab_22_h, columna = .x))
pa_ph <- purrr::map(.x = 1:2, .f= ~rbind(aa[[1]][[.x]]))
dfs[[2]] <- ph_a(pa_ph[[1]])

aa <- purrr::map(.x = c(5), .f = ~hace_df(tab_17_m, tab_22_m, columna = .x))
pa_ph <- purrr::map(.x = 1:2, .f= ~rbind(aa[[1]][[.x]]))
dfs[[3]] <- ph_a(pa_ph[[1]])

uno <- rbind(dfs[[1]][1, ], dfs[[2]][1, ], dfs[[3]][1, ])
uno$estrella <- ifelse(!uno$Tendencia%in%c('Igual', NA), paste0(round(uno[,3],1),'*'), uno$Tendencia) 

dos <- rbind(dfs[[2]][1, ], dfs[[2]][2, ], dfs[[3]][2, ])


ponle <- c("Estados Unidos Mexicanos", "Hombres", "Mujeres")

uno[,1] <- ponle
dos[,1] <- ponle
escribe_a(ruta="D:/ENADIS/Entregable_enadis_2022/Tabs/enadis_2022_presentacion/presn_asp.xlsx", 
          nom_hoja = "d172", 
          df = uno, 
          cv = dos)


