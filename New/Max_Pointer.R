Max_Pointer <- function(df, clasif, date, medida) {
    if (missing(date)) {
        date <- names(df)[purrr::map_lgl(df, ~class(.x) == "Date")][1]
    }
    if (missing(medida)) {
        medida <- names(df)[purrr::map_lgl(df, ~class(.x) == "numeric")][1]
    }
    if (length(date) == 0) {
        stop(paste("No existe la variable ", date, " en el data frame"))
    }
    if (!date %in% names(df)) {
        stop(paste("No existe la variable ", date, " en el data frame"))
    }
    if (!assertthat::is.date(df[[date]])) {
        stop(paste("La variable ", date, " no es formato Date"))
    }
    if (!is.numeric(df[[medida]])) {
        medida <- try(as.numeric(medida), silent = T)
        if (assertthat::is.error(medida) | is.na(medida)) {
            stop(paste("La variable de medida ", medida, " debe ser num?rica"))
        }
    }
    muestra_tb <- sample(1:nrow(df), min(5000, nrow(df)))
    muestra_server <- df[, c(medida, clasif, date)]
    muestra_server$Semana <- muestra_server[[date]] - as.numeric(muestra_server[[date]])%%7
    muestra_server <- muestra_server[muestra_server$Semana > (min(df[[date]]) - 1), ]
    Semana_server <- purrr::map(c(mean, max), ~reshape2::dcast(muestra_server, formula(paste0("Semana~", date)), .x, value.var = medida))
    Semana_server.0 <- Semana_server[[2]][, -1]/Semana_server[[1]][, -1]
    Semana_server.0$Semana <- Semana_server[[1]][, 1]
    system.time({
        Semana_server.0[is.na(Semana_server.0)] <- 1
    })
    Semana_server.0 <- drop.df(Semana_server.0)
    QM95 <- quantile(as.matrix(Semana_server.0[, -ncol(Semana_server.0)]), 0.95)
    Semana_server_out <- Semana_server.0[, map_lgl(Semana_server.0, ~any(.x > QM95))]
    Semana_server_out_cont <- rbind(Semana_server_out[, -ncol(Semana_server_out)], as.data.frame(t(data.frame(colSums(Semana_server_out[, -ncol(Semana_server_out)])))))
    Semana_server_out_cont <- Semana_server_out_cont[, order(-Semana_server_out_cont[nrow(Semana_server_out_cont), ])]
    Score_Max <- dft(Semana_server_out_cont[nrow(Semana_server_out_cont), ]/(nrow(Semana_server_out_cont) - 1))
    Cortes_8 <- table(cut(Score_Max[[1]], 8))
    Porcent_aumento <- sum(Cortes_8/sum(Cortes_8) * (0:7))
    Alerta[[4]] <- round(Porcent_aumento, 3)
    Alerta
}
# library(timeSeries)
# data(LPP2005REC)
# Max_Pointer(df = LPP2005REC, clasif, date, medida)
