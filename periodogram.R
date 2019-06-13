periodicidad<-function(df,place=10,fast=TRUE,pruebas=1000){
  #df es una serie de tiempo ordenada
  #Hacer el proceso máx 3 veces
  ddT<-data.frame(freq=c(),spec=c(),orden=c())
  #Desde el .3 hasta el final
  ords<-floor(length(df)*.3):length(df)
  #La propuesta es usar intervalos aleatorios distintos
  #De lo contrario se tardará mucho
  repp<-length(ords)
  if(repp>pruebas & fast){
    #Se reducirá a 1000
    ords<-sort(sample(x = ords,size = pruebas))
  }
  for(i in 1:2){
    for(lu in ords){
      if(i==1){
        #Se identifica estacionalidad si la hay
        p<-TSA::periodogram(df[1:lu],plot=F)
      }
      else if (i==2){
        p<-TSA::periodogram(df[(lu+1-min(ords)):length(df)],plot=F)
      }
      #spectrum(ts.anual,log="no")
      dds<- data.frame(freq=1/p$freq, spec=p$spec,orden=1:length(p$spec))
      dds<-head(dds[order(-dds$spec),],place)
      #si es la primera la omitimos porque es evidente la estacionalidad del total
      #Aqu? descartamos aquellas frecuencias que no alcanan el 6.5% de la freq m?xima
      ddT<-rbind(ddT,dds)
      #Regresa los datos agrupados conforme a las primeras 5 frecuencias
    }
  }
  
  ddT<-ddT[order(-ddT$spec),]
  Maxi<-max(ddT$spec)
  #Una periodicidad menor a 3 obs es sospechosa para un periodo
  ddT<-ddT[ddT$orden>2,]
  dd_Top<-head(ddT,30)
  #Opciones distintas
  dd_Top$Freq_Orden<-paste0(dd_Top$freq,"_",dd_Top$orden)
  dd_Top<-suppressWarnings(reshape2::dcast(dd_Top,Freq_Orden~.,max,value.var="spec"))
  dd_Top$.<-dd_Top$./Maxi
  dd_Top<-dd_Top[order(-dd_Top$.),]
  
  return(list(unique(as.numeric(do.call("rbind",strsplit(dd_Top$Freq_Orden,"_"))[,1])),dd_Top))
}


#library()
library(timeSeries)
data(LPP2005REC)
df<-LPP2005REC
#Tomar en cuenta que el tiempo para series mayor a 1000 datos es 20 segs.
periodicidad(df,place = 10)
# place es la cantidad de frecuencias que serán tomadas en cuenta para calcular

#En caso de tomar falso el factor de velocidad, habrá menos casos pero será más exacta (sólo se aplica para valores mayores a un valor X)
periodicidad(df,place = 10,F,pruebas=500)
