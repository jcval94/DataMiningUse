Valuador<-function(vars=c("q","shape1","shape2"),
                   vars_range=list(c(0,1),c(0,100),c(0,100)),
                   fun=pbeta,
                   puntos_valuar=c(50,50,50)){
  
  param<-formalArgs(fun)
  eq_param<-vars %in% param
  if(!all(eq_param)){
    quit(paste0("Los siguientes parametros no se encontraron: ",
                paste0(vars[!vars %in% eq_param],collapse = ", ")))
  }
  
  vars_a_cambiar<-c(1:length(eq_param))[eq_param]
  
  if(length(vars_a_cambiar)!=length(vars_range)){
    quit("vars y vars_range deben tener la misma longitud")
  }
  
  parametros_int<-vars_range[eq_param]
  
  #Número de parámetros a cambiar y combinaciones que deben realizarse
  length(parametros_int)
  
  fijos<-length(parametros_int)
  
  valores<-parametros_int
  for(i in 1:length(parametros_int)){
    p_i<-parametros_int[[i]]
    valores[[i]]<-seq(p_i[1],p_i[2],length.out = puntos_valuar[i])
  }
  
  e.grid<-as.data.frame(expand.grid(valores))
  
  ####################
  #Se deben separar a las funciones en aquellas graficables y no graficables
  
  e.grid[["Y"]]<-fun(e.grid[[1]],e.grid[[2]],e.grid[[3]])
  
  for(i in vars_a_cambiar){
    #Aquí empieza el proceso de parametrización y cambios de la variable
    #Fijamos 1 parametro
    n<-n+1
    fun()
    valores
    
    formals(fun)[i]<-parametros_int
    
    Valuar100_muestras<-system.time({
      
    })
    
  }
  
  
  Param_vac
  
  #aplicar map a cada parámetro de la función
  
  N_param=length(vars)
  
  for(i in 1:N_param){
    fun
  }
  
  
}
