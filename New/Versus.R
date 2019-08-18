library(ggplot2)
Versus <- function(df, var1 = names(df)[1], var2 = names(df)[2], minmax = NULL, plot = TRUE, disc = TRUE, min.disc = 20, max.cont = 500) {
    NV1 <- is.numeric(var1)
    NV2 <- is.numeric(var2)
    FV1 <- is.factor(var1)
    FV2 <- is.factor(var2)
    if (length(unique(df[, var1])) > min.disc) {
        return(invisible())
    }
    a <- df[, var2]
    if (is.null(minmax)) {
        minimo <- min(a)
        maximo <- max(a)
    }
    else {
        minimo <- minmax[1]
        maximo <- minmax[2]
    }
    if (length(unique(df[, var2])) > max.cont) {
        v <- as.data.frame(table(cut(df[, var2], seq(minimo, maximo, length.out = 100), include.lowest = T), df[, var1]))
    }
    else {
        v <- as.data.frame(table(df[, var2], df[, var1]))
    }
    if (length(unique(v$Var1)) <= length(unique(v$Var2))) {
        xt <- unique(v$Var1)
        var <- "Var1"
        vir <- "Var2"
    }
    else {
        xt <- unique(v$Var2)
        var <- "Var2"
        vir <- "Var1"
    }
    list <- list()
    for (i in 1:length(xt)) {
        list[[i]] <- v[v[, var] == xt[i], ]
    }
    J <- list[[1]]
    for (i in 1:(length(list) - 1)) {
        J <- dplyr::left_join(J, list[[i + 1]], by = vir)
    }
    sumas <- rowSums(J[, startsWith(names(J), "Freq")])
    porcent <- J
    proporciones <- J[, (startsWith(names(J), "Freq"))]/sumas
    for (y in 1:ncol(proporciones)) {
        names(proporciones)[y] <- paste(var1, "=", unique(v$Var2)[y])
    }
    porcent <- cbind(porcent, proporciones)
    names(porcent)[1] <- paste("Intervalo", var2, "")
    if (plot) {
        names(df)[1] <- "var1"
        names(df)[2] <- "var2"
        plot_2 <- ggplot(df, aes(var1, var2)) + geom_violin(aes(fill = var1)) + geom_boxplot(width = 0.2, alpha = 0.2, color = "red")
    }
    return(list(porcent, plot_2))
}
data(iris)
VS <- Versus(df = iris, var1 = "Species", var2 = "Sepal.Length", plot = T)
