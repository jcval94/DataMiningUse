na_prop<-function(X){
  sum(ifelse(is.na(X),1,0))/length(X)
}
#Example
na_prop(c(NA,rnorm(100)))
