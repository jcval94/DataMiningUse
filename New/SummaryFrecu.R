SummaryFrecu <- function(data, Vars_clasif, Var_medida, anch = 1000, reg_min = 1) {
    if (!class(data) %in% "data.frame") {
        stop(paste("data debe ser data frame"))
    }
    if (!Var_medida %in% names(data)) {
        stop(paste(Var_medida, "no se encuentra en el data frame"))
    }
    Medida <- data[[Var_medida]]
    data <- tibble::as_tibble(data)[, c(Vars_clasif)]
    data <- class_as_class(data, "factor", "character")
    data[is.na(data)] <- ""
    Var <- do.call(paste, purrr::map(data, ~.x))
    c1 <- data.frame()
    Frecuencias <- as.data.frame(table(Var))
    for (r in 1:length(unique(Var))) {
        Var_r <- Var[Var == unique(Var)[r]]
        if (length(Var_r) < reg_min) {
            c1 <- rbind(c1, matrix(NA, 1, 19, dimnames = list(NA, c("min", "q1", "q2", "q3", "max", "media", "sd", "Registros_Completos", "vacios", "ajuste", "p_valor", "Uni_min", "Uni_q2", "Uni_max", "Uni_media", "Uni_sd", "vacios", "Gto_Total", "Unidades_Totales"))))
        }
        else {
            a <- try(Medida[Var %in% unique(Var_r)], silent = T)
            if (assertthat::is.error(a)) {
                (next)()
            }
            if (length(a) == 0) {
                (next)()
            }
            b <- estad(a, anch = anch)
            totales <- data.frame(Total = sum(na.omit(a)), Var = unique(Var)[r])
            b <- cbind(b, totales)
            c1 <- rbind(c1, b)
        }
    }
    return(c1)
}
