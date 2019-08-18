library(purrr)
drop.df <- function(df) {
    df[, purrr::map_lgl(df, ~length(unique(.x)) > 1)]
}
data(iris)
iris[["A"]] <- 0
head(iris)
head(drop.df(iris))
