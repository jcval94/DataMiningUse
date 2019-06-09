RBIND<-function(df1,df2){
  nd1<-ncol(df1);nd2<-ncol(df2)
  if(names(df1)==names(df2)){
    return(rbind(df1,df2))
  }
  else{
    mxm<-c(nd1,nd2)==max(c(nd1,nd2))
    dmx<-list(df1,df2)[mxm][[1]]
    dmn<-list(df1,df2)[!mxm][[1]]
    min_ex<-names(dmn)[!names(dmn) %in% names(dmx)]
    max_ex<-names(dmx)[!names(dmx) %in% names(dmn)]
    if(length(min_ex)==0){
      dmn<-cbind(dmn,as_tibble(matrix(NA,nrow(dmn),c(nd1,nd2)[mxm]-c(nd1,nd2)[!mxm],
                                      dimnames = list(1:nrow(dmn),max_ex))))
    }
    else{

      dmn<-cbind(dmn,as_tibble(matrix(NA,nrow(dmn),length(max_ex),
                                      dimnames = list(1:nrow(dmn),max_ex))))

      dmx<-cbind(dmx,as_tibble(matrix(NA,nrow(dmx),length(min_ex),
                                      dimnames = list(1:nrow(dmx),min_ex))))

    }
    return(rbind(dmx,dmn))
  }
}
