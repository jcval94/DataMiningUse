periodicidad <- function(ts, place = 10) {
    ddT <- data.frame(freq = c(), spec = c(), orden = c())
    ords <- floor(length(ts) * 0.7):length(ts)
    for (lu in ords) {
        p <- TSA::periodogram(ts[1:lu], plot = F)
        dds <- data.frame(freq = 1/p$freq, spec = p$spec, orden = 1:length(p$spec))
        dds <- head(dds[order(-dds$spec), ], place)
        ddT <- rbind(ddT, dds)
    }
    ddT <- ddT[order(-ddT$spec), ]
    Maxi <- max(ddT$spec)
    ddT <- head(ddT[ddT$orden > 2, ], 15)
    ddT$Freq_Orden <- paste0(ddT$freq, "_", ddT$orden)
    ddT <- suppressWarnings(reshape2::dcast(ddT, Freq_Orden ~ ., max, value.var = "spec"))
    ddT$. <- ddT$./Maxi
    ddT <- ddT[order(-ddT$.), ]
    return(list(unique(as.numeric(do.call("rbind", strsplit(ddT$Freq_Orden, "_"))[, 1])), ddT))
}
library(timeSeries)
data(LPP2005REC)
df <- LPP2005REC
periodicidad(df, place = 10)
