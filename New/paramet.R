paramet <- function(f, new_par, default = T) {
    n_par <- length(new_par)
    if (default) {
        repl <- nchar(as.character(formals(f))) <= 0
    }
    else {
        repl <- !(is.na(new_par) | is.null(new_par))
    }
    new_par <- new_par[repl]
    n_par <- length(new_par)
    ciclo <- (1:length(repl))[repl]
    for (i in ciclo) {
        formals(f)[i] <- new_par[i]
    }
    f
}
