library(tsutils)

# Theta method example on promotional data
thetaModel1 <- thetaf(window(PromoData$y, start(PromoData$y), c(5,0)), h=12)
plot(thetaModel1)
points(PromoData$y)

# Seasonal plots
seasplot(PromoData$y)
seasplot(PromoData$y, outplot=4)

# ts decomposition
PromoDecomp <- decomp(PromoData$y, outplot=T)

# abc analysis
x <- abs(matrix(cumsum(rnorm(5400,0,1)),36,150))
xabc <- abc(x)
plot(xabc)

# xyz analysis
x <- abs(matrix(cumsum(rnorm(5400,0,1)),36,150))
xxyz <- xyz(x, m=12, type="cv")
plot(xxyz)

# abc-xyz analysis
abcxyz(xabc, xxyz)
