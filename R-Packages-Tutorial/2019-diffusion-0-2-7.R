library(diffusion)

# Fit the Bass model
fitbass <- diffusion(tsSafari[, 2], type = "bass")
plot(fitbass)

# Produce predictions
plot(predict(fitbass, h=5))
