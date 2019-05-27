library(ggplot2)

Versus<-function(df,var1=names(df)[1],var2=names(df)[2],
                 minmax=NULL,plot=TRUE,disc=TRUE,min.disc=20,max.cont=500){
  #Una de las vaiables debe ser cont. y la otra disc.
  
  #Si dsc == T entonces v<-table()... en lugar de numeros
  #Si cualquier variable tiene un solo valor pierde el entido y brea
  
  ifelse(length(unique(df[,var1]))>min.disc,break(),"")
  #si es un factor volverla número
  #ifelse(class(df[,var1]) | length(unique(df[,var1]))>50,,)
  #minmax=c(0,1)
  a<-df[,var2]
  ifelse(is.null(minmax),c(minimo<-min(a),maximo<-max(a))
         ,c(minimo<-minmax[1],maximo<-minmax[2]))
  
  if(length(unique(df[,var2]))>max.cont){
    v<-as.data.frame(table(cut(df[,var2],seq(minimo, maximo, length.out=100),include.lowest = T),
                           df[,var1]))
  }else{
    v<-as.data.frame(table(df[,var2],df[,var1]))
  }
  
  
  ifelse(length(unique(v$Var1))<=length(unique(v$Var2)),
         c(xt<-unique(v$Var1),var<-"Var1",vir<-"Var2"),
         c(xt<-unique(v$Var2),var<-"Var2",vir<-"Var1"))
  list<-list()
  #var = la variable con mens valores
  for (i in 1:length(xt)) {
    list[[i]]<-v[v[,var]==xt[i],]
  }
  #by = la variable con m?s valores
  J<-list[[1]]
  for (i in 1:(length(list)-1)){
    J<-dplyr::left_join(J,list[[i+1]],by = vir)
  }
  sumas<-rowSums(J[,startsWith(names(J),"Freq")])
  porcent<-J
  proporciones<-J[,(startsWith(names(J),"Freq"))]/sumas
  for (y in 1:ncol(proporciones)){
    names(proporciones)[y] <- paste(var1,"=", unique(v$Var2)[y])
  }
  porcent<-cbind(porcent,proporciones)
  names(porcent)[1] <- paste("Intervalo",var2,"")
  
  
  if(plot==T){
    
    names(df)[1]<-"var1"
    names(df)[2]<-"var2"
    #plot_1<-ggplot(df,aes(var2))+geom_density(aes(fill=var1),alpha=.7)
    theme_set(theme_bw())
    plot_2<-ggplot(df,aes(var1,var2))+
      geom_violin(aes(fill=var1))+
      geom_boxplot(width = 0.2)
    plot_2
  }
  return(porcent)
}

#data(iris)


