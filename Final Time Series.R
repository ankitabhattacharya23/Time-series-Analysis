install.packages("forecast")
library(readxl)
library(forecast)

tdata <- read_excel("Dataset.xls")

tsdata <- ts(tdata$Total,
             start = c(1996,1),end = c(2005,4), frequency = 4)

plot(tsdata)

plot(decompose(tsdata, type = c("multiplicative")))

?stl

fit <- stl(tsdata, s.window = "period")
plot(fit)

ls(fit)
print(fit$time.series)
fit$win

#Exponential Models
#simple model level
fit1 <- HoltWinters(tsdata, beta=FALSE, gamma = FALSE)
ls(fit1)
accuracy(fit1$fitted, tsdata)

#double model level
fit2 <- HoltWinters(tsdata, gamma = FALSE)
ls(fit2)
accuracy(fit2$fitted, tsdata)

#triple model level
fit3 <- HoltWinters(tsdata)
ls(fit3)
accuracy(fit3$fitted, tsdata)

forecast(fit3, 4)

#Using ETS method (    BEST RESULTS   ) 

efit <- ets(tsdata)
accuracy(efit$fitted, tsdata)
summary(efit)

#Predict next 4 values
plot(forecast(efit,4))


#Arima Models

# fit an ARIMA model of order P, D, Q
afit <- arima(tsdata, order=c(1, 0, 1))
summary(afit)

afit <- auto.arima(tsdata)
afit$model
summary(afit)

# predict next 4 observations
forecast(afit, 4)
plot(forecast(afit, 4))

