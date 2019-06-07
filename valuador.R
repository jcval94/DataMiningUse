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
  
  #Debe hacer un número de fors igual al número de combinaciones posibles
  #Sin embargo, lo solucionamos con el comango extra.grid
  
  e.grid<-as.data.frame(expand.grid(parametros_int))
  
  ####################
  #Se deben separar a las funciones en aquellas graficables y no graficables
  #A partir de aquí separamos los caminos si la función generó algún error
  
  #La primera forma es que los parámetros estén ordenados y sean 3
  #A modo de ejemplo se encuentra la siguiente línea
  
  #e.grid[["Y"]]<-fun(e.grid[[1]],e.grid[[2]],e.grid[[3]])
  
  #Sin embargo, se debe generalizar para cualquier número de parámetros en cualquier orden posible
  #Por ello es indispensable valuar la función cambiando sus parámetros mediante
  #la función formals
  
  #Obtendremos la posición de los parámetros mediante:
  Posicion<-(1:length(eq_param))[eq_param]
  #Y obtendremos el número de parámetros a reemplazar
  NP_REMP<-SUM(eq_param)
  
  for(i in vars_a_cambiar){
    #Aquí empieza el proceso de parametrización y cambios de la variable
    #Fijamos 1 parametro
    n<-n+1
    fun()
    parametros_int
    
    formals(fun)[i]<-parametros_int
    
    Valuar100_muestras<-system.time({
      
    })
    
  }
  
  
  
}
