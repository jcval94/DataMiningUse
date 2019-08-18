estad <- function(b, fit = NULL, p.value = NULL, anch = 100) {
    n1 <- length(b)
    b <- na.omit(b)
    if (!is.numeric(b)) {
        b <- as.character(b)
        bb <- as.numeric(gsub(",", "", b))
        b <- bb
    }
    if (length(b) < anch) {
        fit <- "No hay suficientes datos o muchos NAs"
        p.value <- "No hay suficientes datos o muchos NAs"
    }
    if (n1 > 0) {
        if (((n1 - length(b))/n1) > 0.15) {
            fit <- "No hay suficientes datos o muchos NAs"
            p.value <- "No hay suficientes datos o muchos NAs"
        }
    }
    if (is.null(fit) | is.null(p.value)) {
        fit = ajustar(b, mus = 1)[[1]][1]
        p.value = ajustar(b, mus = 1)[[2]][1]
    }
    if (length(b) == 0) {
        b <- NA
    }
    data.frame(min = min(b, na.rm = T), q1 = quantile(b, 0.25, na.rm = TRUE), q2 = quantile(b, 0.5, na.rm = TRUE), q3 = quantile(b, 0.75, na.rm = TRUE), max = max(b, na.rm = T), mean = mean(b, na.rm = T), sd = sd(b, na.rm = T), Len_Data = length(b), Len_NA = n1 - length(b), fit = as.character(fit), p.value = as.character(p.value))
}
