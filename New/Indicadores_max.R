Indicadores_max <- function(df, Var_clasif, Var_date, Var_medida) {
    if (missing(Var_date)) {
        Var_date <- names(df)[purrr::map_lgl(df, ~class(.x) == "Date")][1]
    }
    if (missing(Var_medida)) {
        Var_medida <- names(df)[purrr::map_lgl(df, ~class(.x) == "numeric")][1]
    }
    if (length(Var_date) == 0) {
        stop(paste("No existe la variable ", Var_date, " en el data frame"))
    }
    if (!Var_date %in% names(df)) {
        stop(paste("No existe la variable ", Var_date, " en el data frame"))
    }
    if (!assertthat::is.date(df[[Var_date]])) {
        stop(paste("La variable ", Var_date, " no es formato Date"))
    }
    if (!is.numeric(df[[Var_medida]])) {
        Var_medida <- try(as.numeric(Var_medida), silent = T)
        if (assertthat::is.error(Var_medida) | is.na(Var_medida)) {
            stop(paste("La variable de medida ", Var_medida, " debe ser num?rica"))
        }
    }
    muestra_tb <- sample(1:nrow(df), min(5000, nrow(df)))
    muestra_server <- df[, c(Var_medida, Var_clasif, Var_date)]
    muestra_server$Semana <- muestra_server[[Var_date]] - as.numeric(muestra_server[[Var_date]])%%7
    muestra_server <- muestra_server[muestra_server$Semana > (min(df[[Var_date]]) - 1), ]
    Semana_server <- purrr::map(c(mean, max), ~dcast(muestra_server, formula(paste0("Semana~", Var_date)), .x, value.var = Var_medida))
    Semana_server_div <- Semana_server[[2]][, -1]/Semana_server[[1]][, -1]
    Semana_server_div$Semana <- Semana_server[[1]][, 1]
    system.time({
        Semana_server_div[is.na(Semana_server_div)] <- 1
    })
    Semana_server_div <- drop.df(Semana_server_div)
    QM95 <- quantile(as.matrix(Semana_server_div[, -ncol(Semana_server_div)]), 0.95)
    Semana_server_out <- Semana_server_div[, map_lgl(Semana_server_div, ~any(.x > QM95))]
    Semana_server_out_cont <- rbind(Semana_server_out[, -ncol(Semana_server_out)], as.data.frame(t(data.frame(colSums(Semana_server_out[, -ncol(Semana_server_out)])))))
    Semana_server_out_cont <- Semana_server_out_cont[, order(-Semana_server_out_cont[nrow(Semana_server_out_cont), ])]
    Score_Max <- dft(Semana_server_out_cont[nrow(Semana_server_out_cont), ]/(nrow(Semana_server_out_cont) - 1))
    Cortes_8 <- table(cut(Score_Max[[1]], 8))
    Porcent_aumento <- sum(Cortes_8/sum(Cortes_8) * (0:7))
    ifelse(Porcent_aumento < (0.85), Alerta <- list("A2N", "yellow", "exclamation-circle"), ifelse(Porcent_aumento < (1), Alerta <- list("A1N", "yellow", "check-circle"), ifelse(Porcent_aumento < 1.15, Alerta <- list("A0", "green", "check-circle"), ifelse(Porcent_aumento < 1.3, Alerta <- list("A1", "light-blue", "check-circle"), ifelse(Porcent_aumento < 1.45, Alerta <- list("A2", "yellow", "exclamation-circle"), ifelse(Porcent_aumento < 1.6, Alerta <- list("A3", "red", "times-circle"), ifelse(Porcent_aumento < 
        1.75, Alerta <- list("A4", "black", "times-circle"), Alerta <- list("A3N", "red", "times-circle"))))))))
    Alerta[[4]] <- round(Porcent_aumento, 3)
    Alerta
}
