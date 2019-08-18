RBIND <- function(df1, df2) {
    nd1 <- ncol(df1)
    nd2 <- ncol(df2)
    if (nd1 == nd2 && all(names(df1) == names(df2))) {
        return(rbind(df1, df2))
    }
    else {
        mxm <- c(nd1, nd2) != max(c(nd1, nd2))
        if (all(mxm == F)) {
            mxm <- c(TRUE, FALSE)
        }
        dmx <- list(df1, df2)[mxm][[1]]
        dmn <- list(df1, df2)[!mxm][[1]]
        min_ex <- names(dmn)[!names(dmn) %in% names(dmx)]
        max_ex <- names(dmx)[!names(dmx) %in% names(dmn)]
        if (length(min_ex) + length(max_ex) == 0) {
            dmn <- cbind(dmn, tibble::as_tibble(matrix(NA, nrow(dmn), c(nd1, nd2)[mxm] - c(nd1, nd2)[!mxm], dimnames = list(1:nrow(dmn), max_ex))))
        }
        else {
            dmn <- cbind(dmn, tibble::as_tibble(matrix(NA, nrow(dmn), length(max_ex), dimnames = list(1:nrow(dmn), max_ex))))
            dmx <- cbind(dmx, tibble::as_tibble(matrix(NA, nrow(dmx), length(min_ex), dimnames = list(1:nrow(dmx), min_ex))))
        }
        if (nd1 < nd2 | mxm[1]) 
            return(rbind(dmx, dmn))
        else return(rbind(dmn, dmx))
    }
}
data(iris)
d_1 <- iris[1, ]
d_1$Species <- as.character(d_1$Species)
d_1[1, 5] <- "Rse"
d_1[["Nueva"]] <- c("U")
ir <- iris
ir$W <- "rr"
tail(RBIND(df1 = ir, df2 = d_1))
head(RBIND(df1 = d_1, df2 = ir))
library(data)
RBIND(iris, esoph)
