cp<-function(){
  #Crear y pegar
  base::writeLines("Pegar contenido","Paste.txt")
  file.show('Paste.txt')
  #Guarda y cierra
  Abierto<-file.info("Paste.txt")$mtime
  Abierto1<-Abierto
  while(Abierto1==Abierto){
    Sys.sleep(1)
    Abierto<-file.info("Paste.txt")$mtime
  }
  #Leer y borrar
  suppressWarnings(try(assign("Paste",read.table("Paste.txt",sep="\t"),.GlobalEnv)
                       ,silent = T))
  unlink('Paste.txt')
}

# cp()