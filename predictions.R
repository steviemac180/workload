install.packages("tseries")
install.packages("lmtest")
install.packages("forecast")
library(tseries)
library(lmtest)
library(forecast)

# haemophilia forecasting
f.haem.pred <- work$Haemophilia
f.haem.pred <- f.haem.pred[19:88]

df.haem.pred <- ts(data=f.haem.pred, start = c(2011,07), frequency = 12)
adf.test(df.haem.pred) # not staionary
plot(decompose((df.haem.pred)))

length(df.haem.pred)
dwtest(df.haem.pred[-70]~df.haem.pred[-1])

acf(df.haem.pred, lag.max = 20)
pacf(df.haem.pred, lag.max = 20)

dec.haem.pred <- decompose(df.haem.pred)
adj.haem.pred <- df.haem.pred - dec.haem.pred$trend
plot(adj.haem.pred)

auto.arima(adj.haem.pred, ic = "aic", trace=T)
auto.arima(f.haem.pred, ic = "aic", trace=T)

myarima = arima(f.haem.pred, c(0,1,1), include.mean = T)
forecast(myarima, h=3)
plot(forecast(myarima, h=12), xlab="Month", ylab="Montly Workload",main = "Projected workload for next 12 months - Haemophilia")

# mulitplate forecasting
f.plts.pred <- work$`Multiplate aggregation`
f.plts.pred <- f.plts.pred[59:88]

df.plts.pred <- ts(data=f.plts.pred, start = c(2014,11), frequency = 12)
adf.test(df.plts.pred) # not staionary
plot(decompose((df.plts.pred)))

length(df.plts.pred)
dwtest(df.plts.pred[-30]~df.plts.pred[-1])

acf(df.plts.pred, lag.max = 20)
pacf(df.plts.pred, lag.max = 20)

dec.plts.pred <- decompose(df.plts.pred)
adj.plts.pred <- df.plts.pred - dec.plts.pred$trend
plot(adj.plts.pred)


auto.arima(f.plts.pred, ic = "aic", trace=T)

myarima = arima(f.plts.pred, c(0,1,0), include.mean = T)
forecast(myarima, h=12)
plot(forecast(myarima, h=12), xlab="Month", ylab="Monthly Workload",main = "Projected workload for next 12 months - Mulitplate")

# mFVII forecasting
f.fvii.pred <- work$FVII
f.fvii.pred <- f.fvii.pred[30:87]

df.fvii.pred <- ts(data=f.fvii.pred, start = c(2012,06), frequency = 12)
adf.test(df.fvii.pred) # not staionary
plot(decompose((df.fvii.pred)))

length(df.fvii.pred)
dwtest(df.fvii.pred[-58]~df.fvii.pred[-1])

acf(df.fvii.pred, lag.max = 20)
pacf(df.fvii.pred, lag.max = 20)

dec.fvii.pred <- decompose(df.fvii.pred)
adj.fvii.pred <- df.fvii.pred - dec.fvii.pred$trend
plot(adj.fvii.pred)


auto.arima(f.fvii.pred, ic = "aic", trace=T)

myarima = arima(f.fvii.pred, c(0,1,1), include.mean = T)
forecast(myarima, h=12)
plot(forecast(myarima, h=12), xlab="Month", ylab="Monthly Workload",main = "Projected workload for next 12 months - Factor VII")

