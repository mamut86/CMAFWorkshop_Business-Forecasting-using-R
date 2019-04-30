library(MAPA)

load("PromoData.Rdata")
PromoDataTrain <- head(PromoData,48)
PromoDataTest <- tail(PromoData,12)

PromoDataTrain$y <- ts(PromoDataTrain$y, frequency=12, start=start(PromoData$y))
PromoDataTest$y <- ts(PromoDataTest$y, frequency=12)


# Select the model, keeping last 12 observations for the holdout
mapaModel1 <- mapa(PromoDataTrain$y, fh=12, outplot=1)
graphmaker(PromoDataTrain$y, mapaModel1$outfor, parReset=FALSE)
lines(PromoData$y)
dev.off()

mapa(PromoDataTrain$y, fh=12, outplot=1, type="es", xreg=as.matrix(PromoDataTrain[,-1]))

test <- mapa(ourData$y[1:140], ppy=12, fh=10, outplot=1, type="es", xreg=as.matrix(ourData[,2:3]))

