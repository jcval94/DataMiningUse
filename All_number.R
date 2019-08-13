library(purrr)
library(tibble)
All_number<-function(df){
  df<-as_tibble(df)
  classes<-purrr::map_chr(df,class);nnm<-!classes %in% "numeric"
  no_num<-df[,names(df)[nnm]]
  #Ahora aplicamos la función categorizar
  no_mun_2<-categorizar(no_num,ALL = F)
  #Ahora combinamos las tablas para que todo sea numérico
  
  df_n<-cbind(df[,!nnm],no_mun_2)
  df_n
}
