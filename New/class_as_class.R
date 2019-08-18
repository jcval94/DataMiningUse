class_as_class <- function(df, class1, class2) {
    f_a_s <- function(X) {
        if (class(X) %in% class1) {
            fun_as <- get(paste0("as.", class2))
            X <- fun_as(X)
        }
        X
    }
    purrr::map_df(df, ~f_a_s(.x))
}
