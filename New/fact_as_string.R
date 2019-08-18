fact_as_string <- function(df) {
    f_a_s <- function(X) {
        if (class(X) %in% c("factor")) {
            X <- as.character(X)
        }
        X
    }
    purrr::map_df(df, ~f_a_s(.x))
}
