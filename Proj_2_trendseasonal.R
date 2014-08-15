# MODEL WITH TREND AND SEASONALITY SIMULTANEOUSLY
rm(list=ls())
library(astsa)
library(forecast)

data = read.table("~/Google Drive/STA 137/Project_2/petroleum.txt")
data = data$V1
time = 1:348
data.ts = ts(data)


### PART 1 ###

## diagnostics and transformations
# data indicates trend, periodogram indicates seasonality
plot(data.ts, main = "Original Data")

data.ln = log(data.ts)
plot(data.ln, main = "Log Data")

data.sqrt = sqrt(data.ts)
plot(data.sqrt, main = "Square Root Data")

data.inv = 1 / data.ts
plot(data.inv, main = "Inverse Data")

data.invsqrt = 1 / sqrt(data.ts)
plot(data.invsqrt, main = "Inverse Square Root Data")

kernal.1=kernel("modified.daniell",c(3,3))
kernal.2=kernel("modified.daniell",c(7,7))
kernal.3=kernel("modified.daniell",c(11,11), par(mfrow=c(2,2)))
# k = 3 is good
rec.ave=spec.pgram(data,taper=0, fast=FALSE, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed1=spec.pgram(data,kernal.1,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed2=spec.pgram(data,kernal.2,taper=0, detrend=FALSE, demean=TRUE, log="no")


### PART 2 ###

## remove trend and seasonality 

X1 = c(rep(c(1, rep(0, 10), -1), 29))
X2 = c(rep(c(0, 1, rep(0, 9), -1) , 29))
X3 = c(rep(c(0, 0, 1, c(rep(0, 8), -1)), 29))
X4 = c(rep(c(rep(0, 3), 1, c(rep(0, 7)),  -1), 29))
X5 = c(rep(c(rep(0, 4), 1, c(rep(0, 6)),  -1), 29))
X6 = c(rep(c(rep(0, 5), 1, c(rep(0, 5)),  -1), 29))
X7 = c(rep(c(rep(0, 6), 1, c(rep(0, 4)),  -1), 29))
X8 = c(rep(c(rep(0, 7), 1, c(rep(0, 3)),  -1), 29))
X9 = c(rep(c(rep(0, 8), 1, c(rep(0, 2)),  -1), 29))
X10 = c(rep(c(rep(0, 9), 1, c(rep(0, 1)),  -1), 29))
X11 = c(rep(c(rep(0, 10), 1, -1), 29))

# decide poly order
fit.1 = lm(data.ln ~ time + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11)
AIC(fit.1)
fit.2 = lm(data.ln ~ time + I(time^2) + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11)
AIC(fit.2)
fit.3 = lm(data.ln ~ time + I(time^2) + I(time^3) + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11)
AIC(fit.3)

fit = lm(data.ln ~ time + I(time^2) + I(time^3) + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11)
coef = fit$coef[5:15]
plot(fit$fitted, type = "l", main = "Plot of Fitted Values")
plot(fit$residual, main = "Plot of residuals")
plot(coef, type = "l", main = "Plot of seasonal coefficients")


### PART 3 ###

# preliminary model
rough = fit$residuals
acf(rough, lag = 10, main = "acf of rough")
pacf(rough, lag = 10, main = "pacf of rough")

kernal.1=kernel("modified.daniell",c(3,3))
kernal.2=kernel("modified.daniell",c(7,7))
kernal.3=kernel("modified.daniell",c(11,11), par(mfrow=c(2,2))) 
rec.ave=spec.pgram(rough,taper=0, fast=FALSE, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed1=spec.pgram(rough,kernal.1,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed2=spec.pgram(rough,kernal.2,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed3=spec.pgram(rough,kernal.3,taper=0, detrend=FALSE, demean=TRUE, log="no")

# final rough model
arima = auto.arima(rough, max.p = 10, max.q = 10)
arima
model = sarima(rough, p = 2, d = 0, q = 1, details = F, no.constant = T)
rough.estimate = arima(rough, order = c(2, 0, 1))
pacf(arima$residuals)

spec = spectrum(rough.estimate$residuals)
kernal.1=kernel("modified.daniell",c(3,3))
kernal.2=kernel("modified.daniell",c(7,7))
kernal.3=kernel("modified.daniell",c(11,11), par(mfrow=c(2,2))) 
rec.ave=spec.pgram(rough.estimate$residuals,taper=0, fast=FALSE, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed1=spec.pgram(rough.estimate$residuals,kernal.1,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed2=spec.pgram(rough.estimate$residuals,kernal.2,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed3=spec.pgram(rough.estimate$residuals,kernal.3,taper=0, detrend=FALSE, demean=TRUE, log="no")


# final model
estimated.ln = fit$fitted + rough.estimate$residuals
estimated = exp(estimated.ln)

plot(time, estimated, type = "l", main = "Plot of Final Model with Trend, Seasonality, and Rough")
lines(time, data, col = "blue")

acf(fit$residuals)
pacf(fit$residuals)

#spectral density and smoothed periodogram of final model


# residual analysis of final model
resids = estimated - data
acf(resids)
pacf(resids)
kernal.1=kernel("modified.daniell",c(3,3))
kernal.2=kernel("modified.daniell",c(7,7))
kernal.3=kernel("modified.daniell",c(11,11), par(mfrow=c(2,2))) 
rec.ave=spec.pgram(resids,taper=0, fast=FALSE, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed1=spec.pgram(resids,kernal.1,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed2=spec.pgram(resids,kernal.2,taper=0, detrend=FALSE, demean=TRUE, log="no")
rec.smoothed3=spec.pgram(resids,kernal.3,taper=0, detrend=FALSE, demean=TRUE, log="no")


### PART 4 ###

## PREDICTION TIME
newdata.ln = data.ln[1:336]
newtime = 1:336
newrough = rough[1:336]
bar = data.frame(newdata.ln)

nX1 = c(rep(c(1, rep(0, 10), -1), 28))
nX2 = c(rep(c(0, 1, rep(0, 9), -1) , 28))
nX3 = c(rep(c(0, 0, 1, c(rep(0, 8), -1)), 28))
nX4 = c(rep(c(rep(0, 3), 1, c(rep(0, 7)),  -1), 28))
nX5 = c(rep(c(rep(0, 4), 1, c(rep(0, 6)),  -1), 28))
nX6 = c(rep(c(rep(0, 5), 1, c(rep(0, 5)),  -1), 28))
nX7 = c(rep(c(rep(0, 6), 1, c(rep(0, 4)),  -1), 28))
nX8 = c(rep(c(rep(0, 7), 1, c(rep(0, 3)),  -1), 28))
nX9 = c(rep(c(rep(0, 8), 1, c(rep(0, 2)),  -1), 28))
nX10 = c(rep(c(rep(0, 9), 1, c(rep(0, 1)),  -1), 28))
nX11 = c(rep(c(rep(0, 10), 1, -1), 28))

newfit = lm(newdata.ln ~ newtime + I(newtime^2) + I(newtime^3) + nX1 + nX2 + nX3 + nX4 + nX5 + nX6 + nX7 + nX8 + nX9 + nX10 + nX11)

# extrapolate trend
trend.pred =  rep(1, 12) * newfit$coef[1] + newfit$coef[2] * (seq(337, 348, 1)) + newfit$coef[3] * (seq(337, 348, 1))^2 + newfit$coef[4] * (seq(337, 348, 1))^3

# seasonal coefficients
seasonal.pred = c(newfit$coef[5:15], 0)

#prediction rough
newmodel = arima(newrough, order = c(2, 0, 1))
sarima = sarima(newrough, p = 2, d = 0, q = 1, details = F)
model.pred = predict(newmodel, 12) 

prediction.ln = trend.pred + seasonal.pred + model.pred$pred[1:12]
prediction = exp(prediction.ln)

plot(1:12, data[337:348], ylim = c(60, 130), type = "l", main = "Predicted vs Actual",
     xlab = "Months January to December of 2012", ylab = "Petroleum")
lines(1:12, prediction, col = "blue")
legend("topright", c("Actual Values", "Estimated Values"), fill = c("black", "blue"))

