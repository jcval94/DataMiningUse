activar <- function() {
    library(actuar)
    library(stats)
    library(extraDistr)
    library(Newdistns)
    libos <- c("actuar", "stats", "extraDistr", "Newdistns")
    vas <- c()
    for (ll in 1:length(libos)) {
        lista <- ls(paste0("package:", libos[ll]))
        lista <- lista[purrr::map_lgl(lista, ~is.function(get(.x)))]
        lista <- lista[startsWith(lista, c("p")) | startsWith(lista, c("d")) | startsWith(lista, c("r"))]
        ista <- table(map_chr(lista, ~substr(.x, 2, nchar(.x))))
        ista <- names(ista[ista == 3])
        vas <- c(ista, vas)
    }
    vas <- sort(vas[!duplicated(vas)])
    Dist_list <- list()
    Dist_list$Names <- vas
    Dist_list$r <- purrr::map(vas, ~get(paste0("r", .x)))
    Dist_list$d <- purrr::map(vas, ~get(paste0("d", .x)))
    Dist_list$p <- purrr::map(vas, ~get(paste0("p", .x)))
    Dist_list$Indi <- c()
    Dist_list$Cont <- c()
    rett <- (!Dist_list$Names %in% c("phtype", "cat"))
    for (re in 1:length(Dist_list)) {
        Dist_list[[re]] <- Dist_list[[re]][rett]
    }
    for (ils in 1:length(Dist_list$r)) {
        formals(Dist_list$r[[ils]])[1] <- alist(obs = 5)
    }
    formal <- map(Dist_list$r, ~formals(.x))
    params_ll <- list()
    fns_huec <- c()
    for (ils in 1:length(formal)) {
        class(formal[[ils]]) <- "list"
        vacio <- formal[[ils]][map_lgl(formal[[ils]], ~class(.x) == "name")]
        params_ll[[ils]] <- vacio
        if (length(vacio) == 0) {
            (next)()
        }
        fns_huec <- c(fns_huec, ils)
    }
    pprw <- names(do.call("c", params_ll))
    pprw <- sort(pprw[!duplicated(pprw)])
    declaraciones <- function(names) {
        A_pp <- c("n", "m", "lambda", "df", "max", "df1", "df2", "nu", "kappa")
        pp_1 <- rep(10, length(A_pp))
        B_pp <- c("rate", "p0", "prob", "scale", "ratelog", "theta", "pi", "shape1", "mean")
        pp_2 <- rep(1/3, length(B_pp))
        C_pp <- c("location", "shape", "a", "b", "size", "shape2", "eta", "shape3", "shape4", "min", "alpha", "k", "tau", "shapelog", "sd", "mu", "r", "beta")
        pp_3 <- rep(4, length(C_pp))
        D_pp <- c("spec", "labels")
        pp_4 <- rep("unif", length(D_pp))
        E_pp <- c("ncp")
        pp_5 <- rep(F, length(E_pp))
        param_tot <- data.frame(names = c(A_pp, B_pp, C_pp, D_pp, E_pp), pars = c(pp_1, pp_2, pp_3, pp_4, pp_5))
        param_tot <- param_tot[param_tot$names %in% names, ]
        Agregado <- list()
        for (ii in 1:nrow(param_tot)) {
            if (prod(!names %in% c(D_pp, E_pp)) == 1) {
                Agregado[[ii]] <- as.numeric(as.character(param_tot[param_tot$names == names[ii], 2]))
            }
            else {
                Agregado[[ii]] <- param_tot[param_tot$names == names[ii], 2]
            }
        }
        if (prod(names == c("size", "prob", "mu")) == 1) {
            names <- names[c(T, T, F)]
            Agregado <- Agregado[c(T, T, F)]
        }
        names(Agregado) <- names
        return(Agregado)
    }
    for (ils in fns_huec) {
        param_vaci <- params_ll[[ils]]
        param_vaci <- param_vaci[names(param_vaci) != "..."]
        suppressWarnings(Err_Mues <- try(Dist_list$r[[ils]](), silent = T))
        if (prod(is.na(Err_Mues)) == 1) {
            stop()
        }
        print(ils)
        print(Err_Mues)
        print(!assertthat::is.error(Err_Mues))
        if (!assertthat::is.error(Err_Mues)) {
            (next)()
        }
        Decla <- declaraciones(names(param_vaci))
        formals(Dist_list$r[[ils]])[names(param_vaci)] <- Decla
        suppressWarnings(Err_Mues <- try(Dist_list$r[[ils]](), silent = T))
        if (!assertthat::is.error(Err_Mues)) {
            (next)()
        }
    }
    list(Dist_list)
}
