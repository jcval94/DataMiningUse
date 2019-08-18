string_as_factor <- function(df) {
    f_a_s <- function(X) {
        if (class(X) %in% c("character")) {
            X <- as.factor(X)
        }
        X
    }
    purrr::map_df(df, ~f_a_s(.x))
}
