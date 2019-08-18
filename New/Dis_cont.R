Dis_cont <- function(df) {
    vrbls <- map_chr(df, ~class(.x))
    num_var <- vrbls != "numeric"
    return(list(df[, num_var], df[, !num_var]))
}
