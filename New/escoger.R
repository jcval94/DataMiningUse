escoger <- function(persona1, valores, cortes) {
    a <- c()
    for (i in 1:(length(cortes) + 1)) {
        a <- ifelse(persona1 < cortes[i + 1] & persona1 >= cortes[i], valores[i], NaN)
        ifelse(is.na(a), a <- ifelse(persona1 < cortes[i + 1] & persona1 >= cortes[i], valores[i], NaN), (break)())
    }
    return(a)
}
proporciones <- c(4, 2)
valores <- c(4, 8)
cortes <- c(1, 5, 10)
escoger(1, valores, cortes)
