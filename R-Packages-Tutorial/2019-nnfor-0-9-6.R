library(nnfor)

# Multilayer perceptron
mlpModel1 <- mlp(ts(ourData$y[1:140]), lags=c(1:7))
plot(mlpModel1)

mlpModelForecast1 <- forecast(mlpModel1, h=10)
plot(mlpModelForecast1)
greybox::graphmaker(ourData$y, mlpModelForecast1$mean, fitted(mlpModel1), main="MLP")

# Multilayer perceptron with explanatory variables
mlpModel2 <- mlp(ts(ourData$y[1:140]), lags=c(1:7), xreg=ourData[1:140,2,drop=F])
plot(mlpModel2)

mlpModelForecast2 <- forecast(mlpModel2, h=10, xreg=ourData[,2,drop=F])
plot(mlpModelForecast2)
graphmaker(ourData$y, mlpModelForecast2$mean, fitted(mlpModel2), main="MLPX")


# Extreme Learning Machine with explanatory variables
elmModel <- elm(ts(ourData$y[1:140]), lags=c(1:7), xreg=ourData[1:140,2,drop=F])
plot(elmModel)

elmModelForecast <- forecast(elmModel, h=10, xreg=ourData[,2,drop=F])
plot(elmModelForecast)
graphmaker(ourData$y, elmModelForecast$mean, fitted(mlpModel2), main="ELMX")


# Performance of the three ANNs
BJEMeasuresNNFOR <- cbind(measures(ourData$y[141:150], mlpModelForecast1$mean, ourData$y),
                          measures(ourData$y[141:150], mlpModelForecast2$mean, ourData$y),
                          measures(ourData$y[141:150], elmModelForecast$mean, ourData$y))
colnames(BJEMeasuresNNFOR) <- c("MLP", "MLPX", "ELMX")
round(BJEMeasuresNNFOR,3)
