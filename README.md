Readme
================
Jose Carlos
24/7/2019

<!-- README.md is generated from README.Rmd. Please edit that file -->

\#\#Data Mining Use Usual Functions for Data Mining

\#Functions to join Data frames & vectors without errors

Data to join

``` r
df2<-iris[,5]
head(df2)
```

    ## [1] setosa setosa setosa setosa setosa setosa
    ## Levels: setosa versicolor virginica

``` r
df1<-rbind(iris,iris)
data.table(head(df1))
```

    ##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1:          5.1         3.5          1.4         0.2  setosa
    ## 2:          4.9         3.0          1.4         0.2  setosa
    ## 3:          4.7         3.2          1.3         0.2  setosa
    ## 4:          4.6         3.1          1.5         0.2  setosa
    ## 5:          5.0         3.6          1.4         0.2  setosa
    ## 6:          5.4         3.9          1.7         0.4  setosa

We will join a data frame of dim (300,5) with a vector of (150,1)

``` r
dfc<-suppressWarnings(CBIND(df1,df2))
head(dfc)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species  value
    ## 1          5.1         3.5          1.4         0.2  setosa setosa
    ## 2          4.9         3.0          1.4         0.2  setosa setosa
    ## 3          4.7         3.2          1.3         0.2  setosa setosa
    ## 4          4.6         3.1          1.5         0.2  setosa setosa
    ## 5          5.0         3.6          1.4         0.2  setosa setosa
    ## 6          5.4         3.9          1.7         0.4  setosa setosa

There is a RBIND version as well

``` r
data(iris)
d_1<-iris[1,]
d_1$Species<-as.character(d_1$Species)
d_1[1,5]<-"Rse"
d_1[["Nueva"]]<-c("U")
head(d_1)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Nueva
    ## 1          5.1         3.5          1.4         0.2     Rse     U

``` r
ir<-iris
ir$W<-"rr"
head(ir)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species  W
    ## 1          5.1         3.5          1.4         0.2  setosa rr
    ## 2          4.9         3.0          1.4         0.2  setosa rr
    ## 3          4.7         3.2          1.3         0.2  setosa rr
    ## 4          4.6         3.1          1.5         0.2  setosa rr
    ## 5          5.0         3.6          1.4         0.2  setosa rr
    ## 6          5.4         3.9          1.7         0.4  setosa rr

``` r
head(RBIND(d_1,ir))
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Nueva    W
    ## 1          5.1         3.5          1.4         0.2     Rse     U <NA>
    ## 2          5.1         3.5          1.4         0.2  setosa  <NA>   rr
    ## 3          4.9         3.0          1.4         0.2  setosa  <NA>   rr
    ## 4          4.7         3.2          1.3         0.2  setosa  <NA>   rr
    ## 5          4.6         3.1          1.5         0.2  setosa  <NA>   rr
    ## 6          5.0         3.6          1.4         0.2  setosa  <NA>   rr

\#Functions to clean Data frames

``` r
iris[["A"]]<-0
head(iris)
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species A
    ## 1          5.1         3.5          1.4         0.2  setosa 0
    ## 2          4.9         3.0          1.4         0.2  setosa 0
    ## 3          4.7         3.2          1.3         0.2  setosa 0
    ## 4          4.6         3.1          1.5         0.2  setosa 0
    ## 5          5.0         3.6          1.4         0.2  setosa 0
    ## 6          5.4         3.9          1.7         0.4  setosa 0

Cleaning process, dropping unused columns

``` r
head(drop.df(iris))
```

    ##   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
    ## 1          5.1         3.5          1.4         0.2  setosa
    ## 2          4.9         3.0          1.4         0.2  setosa
    ## 3          4.7         3.2          1.3         0.2  setosa
    ## 4          4.6         3.1          1.5         0.2  setosa
    ## 5          5.0         3.6          1.4         0.2  setosa
    ## 6          5.4         3.9          1.7         0.4  setosa

\#Functions to modify Data frames

``` r
dft(iris)[,1:5]
```

    ##                   1      2      3      4      5
    ## Sepal.Length    5.1    4.9    4.7    4.6    5.0
    ## Sepal.Width     3.5    3.0    3.2    3.1    3.6
    ## Petal.Length    1.4    1.4    1.3    1.5    1.4
    ## Petal.Width     0.2    0.2    0.2    0.2    0.2
    ## Species      setosa setosa setosa setosa setosa
    ## A                 0      0      0      0      0
