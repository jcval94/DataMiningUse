multi_join <- function(list_dfs, join_type = "left", by, ...) {
    join_fun <- get(paste0(join_type, "_join"))
    Apl_jn <- function(df1, df2) {
        join_fun(df1, df2, by)
    }
    for (kt in 1:(length(list_dfs) - 1)) {
        if (kt == 1) {
            df_jn <- list_dfs[[kt]]
        }
        df_jn <- Apl_jn(df_jn, list_dfs[[kt + 1]])
    }
    df_jn
}
