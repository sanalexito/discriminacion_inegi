#Esta función divide entre el total que se ubica hasta arriba. 
Tab_vert_a<-function(xx, etiquetas)
{
  pob0 <- svytotal(t7[, xx], asp)
  pob <- as.data.frame(pob0)
  cv_pob <- as.data.frame(cv(pob0)*100)
  int_pob <- confint(pob0, level = 0.90)
  int_pob <- as.data.frame(int_pob)
  se_pob <- pob[2]
 
  rel0 <- svyratio(t7[,xx], denominator=t7[,xx[1]], asp)
  rel_pob <- as.data.frame(rel0[[1]]*100)
  cv_rel_pob <- cv(rel0)*100
  int_rel_pob <- confint(rel0, level = 0.90)*100
  int_rel_pob <- as.data.frame(int_rel_pob)
  se_rel_pob <- as.data.frame(SE(rel0)*100)
  

#--------------------------------------------------------
xx <- list()
xx[[1]] <- cbind(etiquetas, pob[1], rel_pob[1])
xx[[2]] <- cbind(etiquetas, cv_pob, cv_rel_pob)
xx[[3]] <- cbind(etiquetas, int_pob,NA,int_rel_pob)
xx[[4]] <- cbind(etiquetas, se_pob, se_rel_pob)

#--------------------------------------------------------
for(j in c(2,4)){
for(i in 1:dim(xx[[2]])[1]){
if(xx[[j]][i,3]==0){
   xx[[j]][i,3] <- 0.0000001
  }
 }
}

for(i in 1:dim(xx[[3]])[1]){
  for(j in c(2,5)){
     if(xx[[3]][i,j]==xx[[3]][i,j+1]){
       xx[[3]][i,j]<- 999999999
       xx[[3]][i,j+1]<- 999999999
     }
  }
}


return(xx)

}
