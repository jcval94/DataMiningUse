estad<-function(b,ajuste=NULL,p_valor=NULL,anch=100){
  n1<-length(b)
  b<-na.omit(b)
  if(!is.numeric(b)){
    b<-as.character(b)
    bb<-as.numeric(gsub(",", "",b))
    b<-bb
  }
  if(length(b)<anch){#minimo numero de datos para hacer estad?stica
    ajuste<-"No hay suficientes datos o muchos NAs"
    p_valor<-"No hay suficientes datos o muchos NAs"
  }
  if(n1>0){
    if(((n1-length(b))/n1)>.15){#cantidad m?xima de NAs para que no haya un sesgo
      ajuste<-"No hay suficientes datos o muchos NAs"
      p_valor<-"No hay suficientes datos o muchos NAs"
    }
  }
  if (is.null(ajuste)|is.null(p_valor)){
    ajuste=ajustar(b,mus = 1)[[1]][1]
    p_valor=ajustar(b,mus = 1)[[2]][1]
  }
  if(length(b)==0){
    b<-NA
  }
  c<-data.frame(min=min(b,na.rm = T),
                q1=quantile(b,.25,na.rm=TRUE),
                q2=quantile(b,.50,na.rm=TRUE),
                q3=quantile(b,.75,na.rm=TRUE),
                max=max(b,na.rm = T),
                media=mean(b,na.rm = T),
                sd=sd(b,na.rm = T),
                Registros_Completos=length(b),
                vacios=n1-length(b),
                ajuste=as.character(ajuste),
                p_valor=as.character(p_valor)
  )
  return(c)
}
