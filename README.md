# DataMiningUse
Usual Functions for Data Mining

#Functions to join Data frames without errors

```{r example,  echo=FALSE}
library(data.table)
data(iris)
df2<-iris[,5]
head(df2)
```

```{r example}
df1<-rbind(iris,iris)
```

```{r example}
CBIND(df1,df2)
```
There is a RBIND version as well
```{r example}
data(iris)

d_1<-iris[1,]
d_1$Species<-as.character(d_1$Species)
d_1[1,5]<-"Rse"
d_1[["Nueva"]]<-c("U")

ir<-iris
ir$W<-"rr"

tail(RBIND(df1=ir,df2=d_1))
head(RBIND(df1=d_1,df2=ir))
```
#Functions to clean Data frames
```{r example}
library(data)
#ejemplo de mala práctica
RBIND(iris,esoph)

iris[["A"]]<-0

head(iris)

head(drop.df(iris))
```

#Functions to modify Data frames
```{r example}
data(iris)
Transpose<-dft(iris)
View(Transpose)
```
