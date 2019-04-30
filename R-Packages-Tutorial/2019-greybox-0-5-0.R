library(greybox)

# Prepare the data for the example
ourData <- vector("list")
ourData$y <- BJsales
ourData <- cbind(ourData,as.data.frame(xregExpander(BJsales.lead,c(-7:7))))
View(ourData)

# Model 1 with normal distribution, one variable and I=1
BJalmModel1 <- alm(y~x, ourData[1:140,], distribution="dnorm", ar=0, i=1)
summary(BJalmModel1)
BJalmForecast1 <- forecast(BJalmModel1, ourData[141:150,], interval="p")
plot(BJalmForecast1)

# Model 1 with Laplace distribution, all lags and leads and I=1
BJalmModel2 <- alm(y~., ourData[1:140,], distribution="dlaplace", ar=0, i=1)
summary(BJalmModel2)
BJalmForecast2 <- forecast(BJalmModel2, ourData[141:150,], interval="p")
plot(BJalmForecast2)

# Stepwise with normal distribution and no ARI elements
BJStepwise <- stepwise(ourData[1:140,])
summary(BJStepwise)
BJalmForecast3 <- forecast(BJStepwise, ourData[141:150,], interval="p")
plot(BJalmForecast3)

# Combination of models based on AICc
startTime <- Sys.time()
BJCombined <- lmCombine(ourData[1:140,], bruteForce=T)
Sys.time()-startTime
summary(BJCombined)
BJalmForecast4 <- forecast(BJCombined, ourData[141:150,], interval="p")
plot(BJalmForecast4)

# Measure the performance of the models
BJEMeasures <- cbind(measures(ourData$y[141:150], BJalmForecast1$mean, ourData$y),
                     measures(ourData$y[141:150], BJalmForecast2$mean, ourData$y),
                     measures(ourData$y[141:150], BJalmForecast3$mean, ourData$y),
                     measures(ourData$y[141:150], BJalmForecast4$mean, ourData$y))
colnames(BJEMeasures) <- c("Model 1", "Model 2", "Stepwise", "Combination")
round(BJEMeasures,3)



#### Rolling Origin example ####
### Model 2
ourCall <- "predict(alm(y~., data=ourData[counti,], i=1), newdata=ourData[counto,])"
ourValue <- "mean"
roResults1 <- ro(ourData$y, h=10, origins=20, ourCall, ourValue, co=TRUE, silent=FALSE)

# Produce a plot
plot(roResults1)

# Calculate sMAE
mean(abs(roResults1$holdout - roResults1$mean)) / mean(roResults1$actuals) *100


### Stepwise
ourCallStepwise <- "predict(stepwise(data=ourData[counti,]), newdata=ourData[counto,])"
ourValueStepwise <- "mean"
roResultsStepwise <- ro(ourData$y, h=10, origins=20, ourCallStepwise, ourValueStepwise, co=TRUE, silent=FALSE)

# Produce a plot
plot(roResultsStepwise)

# Calculate sMAE
mean(abs(roResultsStepwise$holdout - roResultsStepwise$mean)) / mean(roResultsStepwise$actuals) *100
