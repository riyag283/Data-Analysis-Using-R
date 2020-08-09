library(forecast)
library(timeSeries)
library(MASS)

data("drivers")      
class(drivers)
plot(drivers)

frequency(drivers)

cycle(drivers)
time(drivers)

m <- lm(drivers~time(drivers))
plot(m)

plot(drivers)
abline(m)

plot(aggregate(drivers, FUN=mean))

boxplot(drivers ~ cycle(drivers))

d <- stl(drivers, s.window = 12)
plot(d)

Drivers <- window(drivers,start=1970,end=1980)
hw <- HoltWinters(Drivers, alpha='0.5', beta=NULL, seasonal='additive')
plot(hw)

predict(hw, n.ahead=48)

p<-predict(hw, n.ahead=48)
plot(hw,p)

Drivers_test <- window(drivers, start=1980)
rms_err <- function(m,o){
  sqrt(mean(m-o)^2)
}
rms_err(Drivers_test,p)

hw <- HoltWinters(drivers,alpha='0.8',beta=NULL, gamma=NULL, seasonal='additive')
plot(hw)

auto.arima(drivers)
plot(auto.arima(drivers))

data1 <- arima(log(drivers),c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
prediction <- predict(data1, n.ahead=12*10)
ts.plot(drivers, 2.718^prediction$pred, log='y', lty=c(1,3), main='Prediction')

data2 <- arima(log(Drivers),c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
prediction2 <- predict(data2, n.ahead=48)
ts.plot(Drivers, 2.718^prediction2$pred, log='y', lty=c(1,3), main='Prediction')

rms_err(Drivers_test,prediction2$pred)

data2 <- arima(log(Drivers),c(0,1,1),seasonal=list(order=c(0.5,0.5,1),period=12))
prediction2 <- predict(data2, n.ahead=48)
ts.plot(Drivers, 2.718^prediction2$pred, log='y', lty=c(1,3), main='Prediction')

rms_err(Drivers_test,prediction2$pred)

