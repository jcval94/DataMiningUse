CBIND<-function(df1,df2){
  nd1<-nrow(df1);nd2<-nrow(df2)
  if(nd1==nd2){
    return(cbind(df1,df2))
  }
  else{
    mxm<-c(nd1,nd2)==max(c(nd1,nd2))
    dmx<-list(df1,df2)[mxm][[1]]
    dmn<-list(df1,df2)[!mxm][[1]]

    df_rw<-c(nd1,nd2)[mxm]-c(nd1,nd2)[!mxm]
    dmn<-rbind(dmn,as_tibble(matrix(NA,df_rw,ncol(dmn),
                                    dimnames = list(rep(NA,df_rw),names(dmn)))))

    return(cbind(dmx,dmn))
  }
}
