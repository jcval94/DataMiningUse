Calendario <- function(frq, ma_af = 4) {
    PP <- try(periodicidad(frq[[2]]), silent = T)
    if (assertthat::is.error(PP)) {
        PP <- 7
    }
    Periodo <- round(PP[[1]], 0)[1]
    names(frq)[purrr::map_lgl(frq, is.numeric)] <- "Valor"
    names(frq)[!purrr::map_lgl(frq, is.numeric)] <- "Fecha"
    frq[["Fecha.0"]] <- frq[["Fecha"]] - as.numeric(frq[["Fecha"]])%%Periodo
    frqpv <- reshape2::dcast(frq, Fecha.0 ~ ., sum, value.var = "Valor")
    frqpv[["Div_est"]] <- row.names(frqpv)
    frqpv[["mensual"]] <- data.table::month(frqpv[["Fecha.0"]])
    frqpv[["anual"]] <- data.table::year(frqpv[["Fecha.0"]])
    ansdm <- c()
    for (k in 0:(nrow(frqpv) - 1)) {
        ansdm <- c(ansdm, mean(frqpv[["."]][(1 + k):min(ma_af + k, nrow(frqpv))]))
    }
    frqpv[["MA_n"]] <- ansdm
    kcl <- kmeans(frqpv[["MA_n"]], min(3, length(unique(ansdm))))
    frqpv[["Clu_n"]] <- kcl$cluster
    clcntrs <- as.data.frame(kcl$centers)
    Clu_cont_n <- ifelse(kcl$cluster == order(clcntrs[[1]])[1], 1, ifelse(kcl$cluster == order(clcntrs[[1]])[2], 2, 3))
    frqpv$Clu_n <- Clu_cont_n
    ansdm <- c()
    for (k in 0:(nrow(frqpv) - 1)) {
        ansdm <- c(ansdm, mean(Clu_cont_n[(1 + k):min(ma_af + k, nrow(frqpv))]))
    }
    frqpv[["Clu_cont_n"]] <- ansdm
    return(frqpv)
}
