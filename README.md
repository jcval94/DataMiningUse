# DataMiningUse
Usual Functions for Data Mining

#Functions to join Data frames without errors

data(iris)
d_1<-iris[,5]

df2<-d_1
df1<-rbind(iris,iris)

CBIND(df1,df2)

data(iris)

d_1<-iris[1,]
d_1$Species<-as.character(d_1$Species)
d_1[1,5]<-"Rse"
d_1[["Nueva"]]<-c("U")

ir<-iris
ir$W<-"rr"

tail(RBIND(df1=ir,df2=d_1))
head(RBIND(df1=d_1,df2=ir))

library(data)
#ejemplo de mala práctica
RBIND(iris,esoph)

#Functions to clean Data frames

iris[["A"]]<-0

head(iris)

head(drop.df(iris))

#Functions to modify Data frames

data(iris)
Transpose<-dft(iris)
View(Transpose)
