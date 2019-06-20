# DataMiningUse
Usual Functions for Data Mining

#Functions to join Data frames without errors

data(iris)
d_1<-iris[,5]

df2<-d_1
df1<-rbind(iris,iris)

CBIND(df1,df2)
