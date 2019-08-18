Valuador <- function(vars = c("q", "shape1", "shape2"), vars_range = list(c(0, 1), c(0, 100), c(0, 100)), fun = pbeta, puntos_valuar = c(50, 50, 50)) {
    param <- formalArgs(fun)
    eq_param <- vars %in% param
    if (!all(eq_param)) {
        warning(paste0("Los siguientes parametros no se encontraron: ", paste0(vars[!vars %in% eq_param], collapse = ", ")))
        return(invisible())
    }
    vars_a_cambiar <- c(1:length(eq_param))[eq_param]
    if (length(vars_a_cambiar) != length(vars_range)) {
        warning("vars y vars_range deben tener la misma longitud")
        return(invisible())
    }
    parametros_int <- vars_range[eq_param]
    fijos <- length(parametros_int)
    P1 <- factor(param[vars_a_cambiar], levels = param[vars_a_cambiar])
    P2 <- factor(vars, levels = param[vars_a_cambiar])
    vars <- vars[order(P2, P1)]
    vars_range <- vars_range[order(P2, P1)]
    puntos_valuar <- puntos_valuar[order(P2, P1)]
    valuar <- list()
    for (i in 1:fijos) {
        valuar[[i]] <- seq(min(vars_range[[i]]), max(vars_range[[i]]), length.out = puntos_valuar[[i]])
    }
    e.grid <- tibble::as_tibble(expand.grid(valuar))
    Posicion <- (1:length(eq_param))
    NP_REMP <- sum(eq_param)
    if (length(vars) == 1) {
        IE <- assertthat::is.error(try(e.grid[["Y"]] <- fun(e.grid[[1]])))
    }
    if (length(vars) == 2) {
        IE <- assertthat::is.error(try(e.grid[["Y"]] <- fun(e.grid[[1]], e.grid[[2]])))
    }
    if (length(vars) == 3) {
        IE <- assertthat::is.error(try(e.grid[["Y"]] <- fun(e.grid[[1]], e.grid[[2]], e.grid[[3]])))
    }
    if (length(vars) == 4) {
        IE <- assertthat::is.error(try(e.grid[["Y"]] <- fun(e.grid[[1]], e.grid[[2]], e.grid[[3]], , e.grid[[4]])))
    }
    NMs <- names(e.grid)
    StN <- startsWith(NMs, "Var")
    names(e.grid)[StN] <- paste0("X_", 1:sum(StN))
    return(e.grid)
}
