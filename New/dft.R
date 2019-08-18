dft <- function(df) {
    if (class(df) %in% "tibble") {
        Tib <- TRUE
    }
    else {
        Tib <- FALSE
    }
    df_1 <- as.data.frame(t(df))
    names(df_1) <- row.names(df)
    if (Tib) {
        return(dplyr::as_tibble(df_1))
    }
    else {
        return(df_1)
    }
}
data(iris)
Transpose <- dft(iris)
View(Transpose)
