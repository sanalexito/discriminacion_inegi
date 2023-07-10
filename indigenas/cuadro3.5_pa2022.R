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

#===============================================================================
etiquetas0 <- c('Las oficinas o servicios de gobierno',
                'Los tribunales o juzgados',
                'Los servicios de salud',
                'Las escuelas',
                'Los negocios (tiendas, restaurantes)',
                'Buscar empleo')
xx <- c("TOT", paste0("TOT_",1:4))
sexo <- c('Hombres', 'Mujeres')
#--- iniciales ----
#Col 2 y 3 (no puede y sí puede)
p1 <- eval(parse(text=paste0("t7$PM1_5_",1:6,"%in%'1' ", collapse = "|"))) 
p2 <- eval(parse(text=paste0("t7$PM1_5_",1:6,"%in%'2' ", collapse = "|"))) 
p3 <- eval(parse(text=paste0("t7$PM1_5_",1:6,"%in%'3' ", collapse = "|"))) 
p4 <- eval(parse(text=paste0("t7$PM1_5_",1:6,"%in%'4' ", collapse = "|"))) 

#bloque nacional
t7$TOT<- ifelse(!t7$marca_indi%in%0, t7$FACTOR_PER,0)
for(i in 1:4) eval(parse(text = paste0("
t7$TOT_",i," <- ifelse(!t7$marca_indi%in%0 & p",i,"%in%T, t7$FACTOR_PER,0)
 ")))
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla <- Tab_2(xx,"SEXO",c("Estados Unidos Mexicanos", sexo))


for(j in 1:6) eval(parse(text = paste0("
t7$TOT_1 <- ifelse(!t7$marca_indi%in%0 & p1%in%T & t7$PM1_5_",j,"%in%1, t7$FACTOR_PER, 0)
t7$TOT_2 <- ifelse(!t7$marca_indi%in%0 & p2%in%T & t7$PM1_5_",j,"%in%2, t7$FACTOR_PER, 0)
t7$TOT_3 <- ifelse(!t7$marca_indi%in%0 & p3%in%T & t7$PM1_5_",j,"%in%3, t7$FACTOR_PER, 0)
t7$TOT_4 <- ifelse(!t7$marca_indi%in%0 & p4%in%T & t7$PM1_5_",j,"%in%4, t7$FACTOR_PER, 0)
asp <- svydesign(id=~as.numeric(UPM_DIS), weights=~1, data=t7, strata=~as.numeric(EST_DIS))
bla_",j," <- Tab_2(xx,\"SEXO\",c(etiquetas0[j], sexo))
")))

#-------------------------------------------------------------------------------
x<-list(); y <- list(); z <- list()

x[[1]] <- rbind(bla[[1]][1,])
for(i in 1:6)eval(parse(text = paste0(" x[[1]] <- rbind(x[[1]], bla_",i,"[[1]][1, ])")))
x[[2]] <- rbind(bla[[2]][1,])
for(i in 1:6)eval(parse(text = paste0(" x[[2]] <- rbind(x[[2]], bla_",i,"[[2]][1, ])")))
x[[3]] <- rbind(bla[[3]][1,])
for(i in 1:6)eval(parse(text = paste0(" x[[3]] <- rbind(x[[3]], bla_",i,"[[3]][1, ])")))
x[[4]] <- rbind(bla[[4]][1,])
for(i in 1:6)eval(parse(text = paste0(" x[[4]] <- rbind(x[[4]], bla_",i,"[[4]][1, ])")))

y[[1]] <- rbind(bla[[1]][2,])
for(i in 1:6)eval(parse(text = paste0(" y[[1]] <- rbind(y[[1]], bla_",i,"[[1]][2,])")))
y[[2]] <- rbind(bla[[2]][2,])
for(i in 1:6)eval(parse(text = paste0(" y[[2]] <- rbind(y[[2]], bla_",i,"[[2]][2,])")))
y[[3]] <- rbind(bla[[3]][2,])
for(i in 1:6)eval(parse(text = paste0(" y[[3]] <- rbind(y[[3]], bla_",i,"[[3]][2,])")))
y[[4]] <- rbind(bla[[4]][2,])
for(i in 1:6)eval(parse(text = paste0(" y[[4]] <- rbind(y[[4]], bla_",i,"[[4]][2,])")))

z[[1]] <- rbind(bla[[1]][3,])
for(i in 1:6)eval(parse(text = paste0(" z[[1]] <- rbind(z[[1]], bla_",i,"[[1]][3,])")))
z[[2]] <- rbind(bla[[2]][3,])
for(i in 1:6)eval(parse(text = paste0(" z[[2]] <- rbind(z[[2]], bla_",i,"[[2]][3,])")))
z[[3]] <- rbind(bla[[3]][3,])
for(i in 1:6)eval(parse(text = paste0(" z[[3]] <- rbind(z[[3]], bla_",i,"[[3]][3,])")))
z[[4]] <- rbind(bla[[4]][3,])
for(i in 1:6)eval(parse(text = paste0(" z[[4]] <- rbind(z[[4]], bla_",i,"[[4]][3,])")))

#-------------------------------------------------------------------------------
for(i in 1:4){
  y[[i]][ ,1] <- c(sexo[1], etiquetas0)
  z[[i]][ ,1] <- c(sexo[2], etiquetas0)
  x[[i]][2:7, 2:3] <- NA
  y[[i]][2:7, 2:3] <- NA
  z[[i]][2:7, 2:3] <- NA
  
  x[[i]][1,4:dim(x[[i]])[2]] <- NA
  y[[i]][1,4:dim(y[[i]])[2]] <- NA
  z[[i]][1,4:dim(z[[i]])[2]] <- NA
  }


tabulado <- purrr::map(.x = 1:4, .f =~ rbind(x[[.x]], NA, z[[.x]], NA, y[[.x]]))

#-------------------------------------------------------------------------------
source("D:/ENADIS/Calculadores/escribe_excel_a.R")
dir <- "D:/ENADIS/Tabs/pubs_2022/S3/"
num_hoja <- 6
archivos <- c('III_poblacion_indigena_2022_est.xlsx', 'III_poblacion_indigena_2022_cv.xlsx',
              'III_poblacion_indigena_2022_int.xlsx', 'III_poblacion_indigena_2022_err.xlsx')
purrr::map(.x = 1:length(archivos), .f =~ Escribe_Excel_a(paste0(dir, archivos[.x]), num_hoja, tabulado[[.x]]))



