TrainTestSplit<-function (df,split=.75){
if(!any("data.frame" %in% class(df))){
exit("must be a data.frame object")
}
RndmV<-runif(nrow(df),0,1)
tr<-RndmV<split

list(df[tr,],df[!tr,])

}

####Ejemplo####
data(iris)

A<-TrainTestSplit(iris)
