library("astsa")
library("AICcmodavg")
rm (list = ls ())

data = read.table ("~/Google Drive/STA 137/Midterm_Project/chicago.txt")
data.ts = ts(data[,1], frequency = 7)
t = 1:106
day = c("Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday",
        "Thursday")

# transform data.ts
trans1 = (data.ts) ^ .5
# Multiple R-squared: 0.5854,  Adjusted R-squared: 0.5814 
trans2 = log (data.ts) 
# Multiple R-squared: 0.6411,  Adjusted R-squared: 0.6377 
trans3 = 1 / (data.ts ^ .5)
# Multiple R-squared: 0.6085,  Adjusted R-squared: 0.6047
trans4 = 1 / (data.ts)
# Multiple R-squared: 0.5299,  Adjusted R-squared: 0.5254 

# regression of transformation on time
fit.data = lm(data.ts ~ t)
fit.trans1 = lm(trans1 ~ t)
fit.trans2 = lm(trans2 ~ t)
fit.trans3 = lm(trans3 ~ t)
fit.trans4 = lm(trans4 ~ t)
summary(fit.data)
summary(fit.trans1)
summary(fit.trans2)
summary(fit.trans3)
summary(fit.trans4)

## plots of data and transformations
plot(t, data.ts, ylab = "ticket sales", 
     main = "Ticket Sales for Chicago Against Time", type = "l")
plot(t, trans1, ylab = "Y ^ 0.5", main = "Y ^ 0.5 Transfromation Against Time",
     type = "l")
plot(t, trans2, ylab = "ln(Y)", main = "ln(Y) Transfromation Against Time",
     type = "l")
plot(t, trans3, ylab = "1 / (Y ^ 0.5)", main = "1 / (Y ^ 0.5) Transfromation Against Time", 
     type = "l") # roughly constant fluctuations 
plot(t, trans4, ylab = "1 / Y", main = "1 / Y Transfromation Against Time", type = "l")

data.ln = log(data.ts)
plot(data.ln, ylab = "log (tickets sales)", 
     main = "Log Ticket Sales for 'Chicago'")

# remove trend through polynomial regression
fit.1 = lm(data.ln ~ t)
fit.2 = lm(data.ln ~ t + I(t ^ 2))
fit.3 = lm(data.ln ~ t + I(t ^ 2) + I(t ^ 3))
plot(t, fit.1$fitted, type = "l", main = "fit.1")
lines(t, data.ln, col = "blue")
plot(t, fit.2$fitted, type = "l", main = "fit.2")
lines(t, data.ln, col = "blue")
plot(t, fit.3$fitted, type = "l", main = "fit.3")
lines(t, data.ln, col = "blue")
print(AICc(fit.1))
print(AICc(fit.2)) # lowest
print(AICc(fit.3))
hist(fit.2$residuals)
qqnorm(fit.2$residuals)
print("Correlation of Residuals and Time")
print(cor(t, fit.2$residuals))

plot(t, data.ln, ylab = "log (tickets sales)", main = "Log Ticket Sales for 'Chicago'")
lines(t, data.ln)
lines(t, fit.1$fitted, col = "red")

# remove seasonality

fit.error = ts(fit.2$residuals, frequency = 7)

X1 = c(rep(c(1, rep(0, 5), -1), 15), 1)
X2 = c(rep(c(0, 1, rep(0, 4), -1) , 15), 0)
X3 = c(rep(c(0, 0, 1,rep(0, 3), -1), 15), 0)
X4 = c(rep(c(rep(0, 3), 1, 0, 0, -1), 15), 0)
X5 = c(rep(c(rep(0, 4), 1, 0, -1), 15), 0)
X6 = c(rep(c(rep(0, 5), 1, -1), 15), 0)

fit.seasonal = lm(fit.error ~ 0 + X1 + X2 + X3 + X4 + X5 + X6)
fit.seasonal$coef[7] = -sum(fit.seasonal$coef[1:6])
seasonal = fit.seasonal$coef
rough = fit.seasonal$residuals
acf(fit.seasonal$residuals)
pacf(fit.seasonal$residuals)

plot(seasonal, type = "l", ylab = "Seasonal Coefficient", xlab = "Day",
     main = "Plot of Seasonality against Day of Week")

plot(t, rough, ylab = "Rough", xlab = "Time", type = "l", 
     main = "Plot of Rough from Transformed Chicago Model")

# diagnostics on rough
hist(rough, main = "Histogram of Rough")
acf(rough)
pacf(rough)

# use the first 99 values to fit and predict
newrough=rough[1:99]
model.ar0 = sarima(newrough, 0, d = 0, 0, no.constant = TRUE)
model.ar1 = sarima(newrough, 1, d = 0, 0, no.constant = TRUE)
model.ar2 = sarima(newrough, 2, d = 0, 0, no.constant = TRUE)
model.ar3 = sarima(newrough, 3, d = 0, 0, no.constant = TRUE)
model.ar4 = sarima(newrough, 4, d = 0, 0, no.constant = TRUE)
model.ar5 = sarima(newrough, 5, d = 0, 0, no.constant = TRUE)
print(model.ar0$AICc)
print(model.ar1$AICc)
print(model.ar2$AICc)
print(model.ar3$AICc) # lowest AICc
print(model.ar4$AICc)
print(model.ar5$AICc)

rough.model.ar3 = arima(newrough, order = c(3, 0, 0))
rough.model.pred = predict(rough.model.ar3, 7)

estimated.sales.ln <- (fit.2$fitted[100:106]) + (fit.seasonal$fitted[100:106]) + (rough.model.pred$pred[1:7])
estimated.sales = exp(estimated.sales.ln)
print(estimated.sales)
print(data[100:106, 'V1'])
plot(data[100:106, c('V1')], ylab = "Ticket Sales", type = "l", main = "Predicted vs Actual Sales")
lines(estimated.sales, col = "blue")
legend("topright", c("estimated recepients", "actual recepients"), fill = c("blue", "black"))
     
     
     
