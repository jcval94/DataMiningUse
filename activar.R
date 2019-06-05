activar<-function(){
  #S?lo es necesario usarla una vez a menos que se dese? agregar una nueva librer?a

  library(actuar)
  library(stats)
  library(extraDistr)
  library(Newdistns)
  #librer?as con ditstr
  libos<-c("actuar","stats","extraDistr","Newdistns")
  vas<-c()
  for (ll in 1:length(libos)){
    #library(libos[ll], lib.loc="~/R/R-3.5.0/library")


    #Cargar funciones de una paqueter?a
    lista<-ls(paste0("package:",libos[ll]))
    #Se quedan s?lo las funciones
    lista<-lista[purrr::map_lgl(lista,~is.function(get(.x)))]
    #me quedo con aquellas que empiecen con p,d,r, la q no es necesaria por el momento
    lista<-lista[startsWith(lista,c("p")) | #startsWith(lista,c("q"))|
                   startsWith(lista,c("d")) | startsWith(lista,c("r"))]

    #quitamos la primera letra y conservamos solo las distribuciones
    ista<-table(map_chr(lista,~substr(.x,2,nchar(.x))))

    ista<-names(ista[ista==3])

    vas<-c(ista,vas)

  }

  vas<-sort(vas[!duplicated(vas)])

  Dist_list<-list()
  Dist_list$Names<-vas
  #Dist_list$q<-purrr::map(vas,~get(paste0("q",.x)))
  Dist_list$r<-purrr::map(vas,~get(paste0("r",.x)))
  Dist_list$d<-purrr::map(vas,~get(paste0("d",.x)))
  Dist_list$p<-purrr::map(vas,~get(paste0("p",.x)))
  Dist_list$Indi<-c()
  Dist_list$Cont<-c()

  #que las r's generen 5 valores, si son igual al piso son disc.
  #max(map_int(Dist_list$r,~length(formalArgs(.x))))

  #Retiramos aquellas cuyas entradas son matrices
  rett<-(!Dist_list$Names %in% c("phtype","cat"))

  for (re in 1:length(Dist_list)) {
    Dist_list[[re]]<-Dist_list[[re]][rett]
  }

  #todas las desplazo por encima del 0

  for (ils in 1:length(Dist_list$r)) {
    #el primer argumento siempre es el tama?o de muestra, ser? siempre 5 para las r's
    formals(Dist_list$r[[ils]])[1]<-alist(obs=5)
  }

  formal<-map(Dist_list$r,~formals(.x))

  params_ll<-list()
  #fn's que necesitan par?metros definidos las llamaremos "huecas"
  fns_huec<-c()
  for (ils in 1:length(formal)) {
    class(formal[[ils]])<-"list"

    #si es igual a "name" es porque est? vac?o
    #y esos son los que nos interesan llenar
    vacio<-formal[[ils]][map_lgl(formal[[ils]],~class(.x) == "name")]
    params_ll[[ils]]<-vacio
    if(length(vacio)==0){next()}
    #sino guardamos ils
    fns_huec<-c(fns_huec,ils)
  }
  pprw<-names(do.call("c",params_ll))
  pprw<-sort(pprw[!duplicated(pprw)])

  #declaraciones de cada variable
  declaraciones<-function(names){

    ##Todo para que cada fn est? bien definida
    A_pp<-c("n","m","lambda","df","max","df1","df2","nu","kappa")
    pp_1<-rep(10,length(A_pp))
    B_pp<-c("rate","p0","prob","scale","ratelog","theta","pi","shape1","mean")
    pp_2<-rep(1/3,length(B_pp))
    C_pp<-c("location","shape","a","b","size","shape2","eta",
            "shape3","shape4","min","alpha","k","tau","shapelog","sd","mu","r","beta")
    pp_3<-rep(4,length(C_pp))
    D_pp<-c("spec","labels")
    pp_4<-rep("unif",length(D_pp))
    E_pp<-c("ncp")
    pp_5<-rep(F,length(E_pp))


    param_tot<-data.frame(names=c(A_pp,B_pp,C_pp,D_pp,E_pp),pars=c(pp_1,pp_2,pp_3,pp_4,pp_5))
    param_tot<-param_tot[param_tot$names %in% names,]

    Agregado<-list()
    for (ii in 1:nrow(param_tot)) {
      if(prod(!names %in% c(D_pp,E_pp))==1){Agregado[[ii]]<-as.numeric(as.character(param_tot[param_tot$names == names[ii],2]))}
      else{Agregado[[ii]]<-param_tot[param_tot$names == names[ii],2]}
    }
    #asumo que s?lo la nbinom tendr? esos par?metros en ese orden
    if(prod(names==c("size","prob","mu"))==1){
      names<-names[c(T,T,F)]
      Agregado<-Agregado[c(T,T,F)]
    }

    names(Agregado)<-names
    return(Agregado)
  }


  #A cada uno de ellos lo voy completando en su funci?n
  for (ils in fns_huec) {
    param_vaci<-params_ll[[ils]]
    param_vaci<-param_vaci[names(param_vaci)!="..."]
    #param_vaci<-names(formal[[ils]][map_lgl(formal[[ils]],~class(.x) == "name")])
    ###Aqu? entran las validaciones respecto al n?mero de par?metros faltantes
    #y c?mo rellenarlos
    #Si funciona sin la necesidad de agregar saltalo
    suppressWarnings(Err_Mues<-try(Dist_list$r[[ils]](),silent = T))
    if(prod(is.na(Err_Mues))==1){stop()}

    print(ils)
    print(Err_Mues)
    print(!assertthat::is.error(Err_Mues))
    if(!assertthat::is.error(Err_Mues)){next()}

    Decla<-declaraciones(names(param_vaci))
    # if (is.null(Decla)) {
    #   Dist_list$r[[ils]]<-NULL
    # }
    #declaraciones(names(param_vaci))
    formals(Dist_list$r[[ils]])[names(param_vaci)]<-Decla


    suppressWarnings(Err_Mues<-try(Dist_list$r[[ils]](),silent = T))

    if(!assertthat::is.error(Err_Mues)){next()}

    #Si es error redefine los par?metros
    #como con la nbinom


  }

  #el ?nico criterio restante es si est? en el 01
  #Las va's si son o no conts
  list(Dist_list)

}
