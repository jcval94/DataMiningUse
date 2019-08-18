reducir.df.col <- function(df, max_na) {
    if (max_na > 1 | max_na < 0) {
        return(NULL)
    }
    df[, purrr::map_lgl(df, ~length(.x[is.na(.x)])/length(.x) < max_na)]
}
reducir(iris, 0.5)
