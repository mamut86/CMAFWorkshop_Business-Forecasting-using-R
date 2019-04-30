library(tsintermittent)

# Croston's method with MAR as a cost
croston1 <- crost(ts.data1, h=10, outplot=TRUE)
# Croston's method with MSE as a cost
croston2 <- crost(ts.data1, h=10, outplot=TRUE, cost="mse")

# TSB with the default cost
tsb1 <- tsb(ts.data1, h=10, outplot=TRUE)

# iMAPA
imapa1 <- imapa(ts.data1, h=10, outplot=TRUE)
