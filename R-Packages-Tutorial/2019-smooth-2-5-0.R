library(smooth)

load("PromoData.Rdata")

#### ETS example ####
# Plot the data
plot(PromoData$y)
abline(v=time(PromoData$y)[which(PromoData$Promo1!=0)], col="darkred", lwd=2)
abline(v=time(PromoData$y)[which(PromoData$Promo2!=0)], col="darkblue", lwd=1, lty=2)

# Plot our sales vs competitor sales
plot(as.matrix(PromoData[,c(1,4)]))

# Select the model, keeping last 12 observations for the holdout
esModel1 <- es(PromoData$y, h=12, holdout=T, intervals="p", silent=F)
# Use both promotions
esModel2 <- es(PromoData$y, h=12, holdout=T, intervals="p", silent=F, xreg=PromoData[,-1])
# Use both promotions
esModel3 <- es(PromoData$y, h=12, holdout=T, intervals="p", silent=F, xreg=PromoData[,-1], xregDo="select")

# Produce combinations of forecasts
esModel4 <- es(PromoData$y, "CCC", h=12, holdout=T, intervals="p", silent=F, xreg=PromoData[,c(2,3)])
round(esModel4$ICw,3)


#### ARIMA examples ####
# Order selection in ssarima
ssarimaModel1 <- auto.ssarima(PromoData$y, h=12, holdout=T, intervals="p", silent=F, xreg=PromoData[,-1])
# Order selection in msarima
msarimaModel1 <- auto.msarima(PromoData$y, h=12, holdout=T, intervals="p", silent=F, xreg=PromoData[,-1])


#### An example with multiple seasonalities ####
# taylorMSARIMA <- auto.msarima(forecast::taylor, orders=list(ar=c(3,3,3),i=c(2,1,1),ma=c(3,3,3)), lags=c(1,48,336),
#                               h=48, holdout=TRUE, silent=F)
load("taylorMSARIMA.Rdata")
# 169 models tested
taylorMSARIMA
msarima(forecast::taylor, model=taylorMSARIMA, h=48*7, holdout=T, silent=F)
