library(purrr)
library(assertthat)
library(readr)
library(textreadr)

Read_docs<-function(dir=getwd(),text_){
  
  if(missing(text_)){warning("text_ must have a value");return(invisible())}
  lt<-list.files(dir)
  SPL<-do.call(c,map(lt,~strsplit(.x,".",fixed = TRUE)))
  len<-map_int(SPL,~unlist(length(.x)))
  
  Reed<-lt[len>1]
  Repeat<-lt[len==1]
  
  #SPL<-SPL[purrr::map_lgl(SPL,~"R" %in% .x[[2]] | "txt" %in% .x[[2]] | "pdf" %in% .x[[2]])]
  
  Doov<-data.frame(Doc=1,Loc=1,Text=1)[-1,]
  if(length(Repeat)>0){
    
    for(i in 1:length(txt)){
      Doov=Read_docs(dir = paste0(dir,"/",Repeat),text_)
      #Doov<-rbind(Doov,data.frame(Doc=txt[i],Loc=Info[1],Text=Info[2]))
    }
  }
  if(length(Reed)>0){
    txt<-Reed[endsWith(Reed,".txt") | endsWith(Reed,"R") | endsWith(Reed,"pdf") | endsWith(Reed,"docx") ]
    
    dir_txt<-paste0(dir,"/",txt)
    readd<-function(tt,dir_,text_.=text_){
      i<-0
      if(length(tt)>0){
        for(t in 1:length(tt)){
          formato<-strsplit(tt,".",fixed = T)[[1]][2]
          if(formato=="docx"){
            Leer<-try(read_docx(dir_[t]),silent = T)
          }else if(formato=="pdf"){
            Leer<-try(read_pdf(dir_[t])$text,silent = T)
          }else{
            Leer<-try(readChar(, file.info(dir_[t])$size),silent = T)
          }
          
          if(is.error(Leer)){
            Leer<-try(read_file(dir_[t]))
            if(is.error(Leer)){
              Leer<-""
            }
          }
          return(
            c(dir_,grepl(text_,Leer))
          )
        }
      }
    }
    
    Doovs<-data.frame(Doc=1,Loc=1,Text=1)[-1,]
    for(i in 1:length(txt)){
      Info=readd(txt[i],dir_txt[i])
      Doovs<-rbind(Doovs,data.frame(Doc=txt[i],Loc=Info[1],Text=Info[2]))
    }
  }
  if(nrow(Doov)>0){
    return(rbind(Doov,Doovs))
  }else{
    return(Doovs)
  }
}

#Read_docs(text_ = "hi")

#Mejoras:
#Se puede crear un token, es decir, ya no tiene que leer todo el documento otra vez si  este no ha sido modificado
#Se puede conectar a google drive para hacer filtros personalizados, descargar documentos/imágenes automáticamnete, etc
#Se pueden programar horarios de descarga y análisis de texto automático
#Se puede saber si un texto fue copiado de internet (solo se separa por puntos y se analiza cada párrafo u oración)

#Tiempo de desarrollo por función: 4 días
