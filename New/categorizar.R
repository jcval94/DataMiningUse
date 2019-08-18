categorizar <- function(df1, vect = names(df1), porcent = 0.015, ALL = T, valor_disc = 85) {
    df1 <- as.data.frame(df1)
    nombrs_1 <- names(df1)
    for (vector in vect) {
        print(vector)
        variable <- df1[, vector]
        if (length(unique(variable)) > valor_disc) {
            (next)()
        }
        uvp <- unique(variable)
        if (length(uvp) == 2) {
            posicion <- as.character(variable[1])
            df1[, paste0(vector, "_", posicion, " & ", uvp[uvp != posicion], "")] <- ifelse(as.character(df1[, vector]) == posicion[1], 1, 0)
        }
        else {
            nombres_po <- as.character(unique(variable))
            prp <- names(table(variable))[table(variable)/length(variable) > porcent]
            nombres <- nombres_po[nombres_po %in% prp]
            nomb1 <- paste0(vector, "_", nombres)
            nombres_Otros <- nombres_po[!nombres_po %in% prp]
            for (y in 1:length(nombres)) {
                df1[, nomb1[y]] <- ifelse(as.character(df1[, vector]) == nombres[y], 1, 0)
            }
            if (length(nombres_Otros) != 0) {
                df1[, paste0(vector, "_Otros")] <- 0
                for (y in 1:length(nombres_Otros)) {
                  df1[, paste0(vector, "_Otros")] <- c(df1[, paste0(vector, "_Otros")] + ifelse(as.character(df1[, vector]) == nombres_Otros[y], 1, 0))
                }
            }
        }
    }
    if (ALL) {
        return(df1)
    }
    else {
        return(df1[, !names(df1) %in% nombrs_1])
    }
}
