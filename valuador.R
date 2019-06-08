Valuador<-function(vars=c("q","shape1","shape2"),
                   vars_range=list(c(0,1),c(0,100),c(0,100)),
                   fun=pbeta,
                   puntos_valuar=c(50,50,50)){
  
  
  param<-formalArgs(fun)
  eq_param<-vars %in% param
  #El objetivo sólo es valuar, si se desea optimizar, se recomienda
  #Usar Gradiente Esticástico.
  #si puntos a valuar o vars range de algún parámetro es 1, reemplazaremos ense parámetro en fun
  # L2<-purrr::map_dbl(vars_range,~length(unique(.x)))==1
  # L1<-puntos_valuar == 1
  # if((any(L2) | any(L1))){
  #   for(i in NP_REMP){
  #     #Aquí empieza el proceso de parametrización y cambios de la variable
  #     #Fijamos 1 parametro
  #     n<-n+1
  #     fun()
  #     parametros_int
  #     
  #     formals(fun)[i]<-parametros_int
  #     
  #     Valuar100_muestras<-system.time({
  #       
  #     })
  #     
  #   }
  # }
  
  if(!all(eq_param)){
    warning(paste0("Los siguientes parametros no se encontraron: ",
                paste0(vars[!vars %in% eq_param],collapse = ", ")))
    return(invisible())
  }
  
  vars_a_cambiar<-c(1:length(eq_param))[eq_param]
  
  if(length(vars_a_cambiar)!=length(vars_range)){
    warning("vars y vars_range deben tener la misma longitud")
    return(invisible())
  }
  
  parametros_int<-vars_range[eq_param]
  
  #Número de parámetros a cambiar y combinaciones que deben realizarse
  fijos<-length(parametros_int)
  
  #Debe hacer un número de fors igual al número de combinaciones posibles
  #Sin embargo, lo solucionamos con el comango extra.grid
  
  
  #En caso de haber ingresado los parámetros en desorden, los
  #ordenaremos conforme haya sido creada la función
  #Para evitar:
  #fun(e.grid[[3]],e.grid[[1]],e.grid[[2]])
  
  P1<-factor(param[vars_a_cambiar],levels = param[vars_a_cambiar])
  P2<-factor(vars,levels = param[vars_a_cambiar])
  
  
  vars<-vars[order(P2,P1)]
  vars_range<-vars_range[order(P2,P1)]
  puntos_valuar<-puntos_valuar[order(P2,P1)]
  
  
  #Y que siempre sea:
  #fun(e.grid[[1]],e.grid[[2]],e.grid[[3]])
  
  valuar<-list()
  for(i in 1:fijos){
    valuar[[i]]<-seq(min(vars_range[[i]]),
                     max(vars_range[[i]]),
                     length.out = puntos_valuar[[i]])
  }
  
  e.grid<-tibble::as_tibble(expand.grid(valuar))
  
  ####################
  #Se deben separar a las funciones en aquellas graficables y no graficables
  #A partir de aquí separamos los caminos si la función generó algún error
  
  #La primera forma es que los parámetros estén ordenados y sean 3
  #A modo de ejemplo se encuentra la siguiente línea
  
  #e.grid[["Y"]]<-fun(e.grid[[1]],e.grid[[2]],e.grid[[3]])
  
  #Sin embargo, se debe generalizar para cualquier número de parámetros posible
  #Por ello es indispensable valuar la función cambiando sus parámetros mediante
  #la función formals
  
  #Obtendremos la posición de los parámetros mediante:
  Posicion<-(1:length(eq_param))
  #Y obtendremos el número de parámetros a reemplazar
  NP_REMP<-sum(eq_param)
  #Crearemos formas básicas para optimizar el tiempo de ejecución mediante los casos:
  if(length(vars)==1){
    IE<-assertthat::is.error(try(e.grid[["Y"]]<-fun(e.grid[[1]])))
  }
  if(length(vars)==2){
    IE<-assertthat::is.error(try(e.grid[["Y"]]<-fun(e.grid[[1]],e.grid[[2]])))
  }
  if(length(vars)==3){
    IE<-assertthat::is.error(try(e.grid[["Y"]]<-fun(e.grid[[1]],e.grid[[2]],
                                                    e.grid[[3]])))
  }
  if(length(vars)==4){
    IE<-assertthat::is.error(try(e.grid[["Y"]]<-fun(e.grid[[1]],e.grid[[2]],
                                                    e.grid[[3]],,e.grid[[4]])))
  }
  #El objetivo es hacer la menor cantidad de pasos dentro del for
  #Se puede prescindir del mismo mediante los casos anteriores
  #Con la desventaja de no poder generalizar
  # for(i in NP_REMP){
  #   #Aquí empieza el proceso de parametrización y cambios de la variable
  #   #Fijamos 1 parametro
  #   n<-n+1
  #   fun()
  #   parametros_int
  #   
  #   formals(fun)[i]<-parametros_int
  #   
  #   Valuar100_muestras<-system.time({
  #     
  #   })
  #   
  # }
  #Edicion rapida
  NMs<-names(e.grid)
  StN<-startsWith(NMs,"Var")
  names(e.grid)[StN]<-paste0("X_",1:sum(StN))
  
  return(e.grid)
  
  
}
