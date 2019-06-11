CBIND<-function(df1,df2){
  is_df<-purrr::map_lgl(list(df1,df2),is.data.frame)
  if(any(is_df)){#hay al menos uno que no es df
    df1<-tibble::as_tibble(df1);df2<-tibble::as_tibble(df2)
  }
  nd1<-nrow(df1);nd2<-nrow(df2)
  if(nd1==nd2){
    return(cbind(df1,df2))
  }
  else{
    mxm<-c(nd1,nd2)==max(c(nd1,nd2))
    dmx<-list(df1,df2)[mxm][[1]]
    dmn<-list(df1,df2)[!mxm][[1]]
    
    df_rw<-c(nd1,nd2)[mxm]-c(nd1,nd2)[!mxm]
    dmn<-rbind(dmn,tibble::as_tibble(matrix(NA,df_rw,ncol(dmn),
                                    dimnames = list(rep(NA,df_rw),names(dmn)))))

    return(cbind(dmx,dmn))
  }
}

data(iris)
d_1<-iris[,5]

df1<-iris
df2<-d_1

CBIND(iris,d_1)
