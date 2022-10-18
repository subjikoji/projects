library(forecast)
library(tseries)
library(ggplot2)
library(openxlsx)


setwd("C:\\Users\\pmhk\\Desktop\\eco374\\eco374 essay")
datter <- read.csv("essay data.csv", h=T)
dat <- datter[1:180,]
plot.ts(dat$y, main="Plot of Original Series", ylab="Y")
adf.test(dat$y, nlag=24)

acf(dat$y, lag.max = 24, main="ACF of Original Series")
n <- length(dat$y)

###maybe use stochastic seasonality??
dy12 <- lag(dat$y, 12)
dat12  <- data.frame(dat, dy12)[13:180,]
y <- ts(dat12$y,start=c(2,1), freq=12)
t= dat12$t
t2 = t^2
t3=t^3
t4=t^4
t5=t^5
t6=t^6
t7=t^7
t8=t^8
t9=t^9
t10=t^10
mod1 <- lm(y~t+dy12, data=dat12)
mod2 <- lm(y~t+t2+dy12, data=dat12)
mod3 <- lm(y~t+t2+t3+dy12, data=dat12)
mod4 <- lm(y~t+t2+t3+t4+dy12, data=dat12)
mod5 <- lm(y~t+t2+t3+t4+t5+dy12, data=dat12)
mod6 <- lm(y~t+t2+t3+t4+t5+t6+dy12, data=dat12)
mod7 <- lm(y~t+t2+t3+t4+t5+t6+t7+dy12, data=dat12)
mod8 <- lm(y~t+t2+t3+t4+t5+t6+t7+t8+dy12, data=dat12)
mod9 <- lm(y~t+t2+t3+t4+t5+t6+t7+t8+t9+dy12, data=dat12)
mod10 <- lm(y~t+t2+t3+t4+t5+t6+t7+t8+t9+t10+dy12,data=dat12)

plot(c(AIC(mod1), AIC(mod2), AIC(mod3), AIC(mod4), 
       AIC(mod5), AIC(mod6), AIC(mod7), AIC(mod8), 
       AIC(mod9), AIC(mod10)), type="l",
     ylab="AIC", xlab="Order", main="AIC vs Order of Time Polynomial")

fit6 <- ts(mod6$fitted.values, start=c(2,1), freq=12)
ts.plot(fit6, y, col=c('red', 'black'), main="Fitted Values and Data")
legend(x="bottomright",
       legend=c("Fitted Values", "Data"),col=c("red","black"),lty=1,cex=1)

resid6 <- ts(mod6$residuals, start=c(2,1), freq=12)
dresid6 <- diff(resid6)

i=1
while(i < 13){
  print(Box.test(dresid6, lag=i, type='Ljung-Box'))
  i = i + 1
}
#acf, adf
adf.test(resid6)
adf.test(dresid6)

par(mfrow=c(1,3))
acf(resid6, main="Residuals")
acf(dresid6, main="1st Diff Residuals")
pacf(dresid6, main= "PCAF 1st Diff")

summary(auto.arima(dresid6))


#arima model
ts_mod3 <- auto.arima(dresid6)
summary(ts_mod3)

pred3 <- predict(ts_mod3, n.ahead=12)
pred3_se <- ts(pred3$se, start=c(16,1), freq=12)

res_f <-diffinv(pred3$pred, xi=resid6[168])[2:13]
res_f <- ts(res_f, start=c(16,1), freq=12)

resid6[157:168]

#residual series prediction 
par(mfrow=c(1,1))
ts.plot(res_f, resid6, col=c('red', 'black'), main="12-step Ahead Residual Prediction")
legend(x="bottomright",
       legend=c("Fitted Values", "Data"),col=c("red","black"),lty=1,cex=1)

#determinstic component prediction
N=12
t=seq(N+1, N+12,1)
t2 = t^2
t3=t^3
t4=t^4
t5=t^5
t6=t^6
t7=t^7

#last observed 12 months)
dy12 <- dat$y[169:180]
time2 <- data.frame(t, t2, t3, t4, t5, t6, t7, dy12)
m6_f <- predict(mod6, time2)
m6_f <- ts(m6_f, start=c(16,1), freq=12)
ts.plot(m6_f, ts(mod6$fitted.values, start=c(2,1), freq=12), col=c("red", "black"))
#add them

y_f2 <- m6_f + ts(pred3$pred, start=c(16,1), freq=12)
upper <- y_f2 + 1.96*pred3_se
lower <- y_f2 - 1.96*pred3_se


p_band <- ts(data.frame(upper, lower), start=c(16,1), freq=12)

ts.plot(y_f, ts(dat$y, start=c(1,1), freq=12), p_band, 
        col=c("red", "black", "blue", "blue"))

par(mfrow=c(1,1))
ts.plot(y_f2, ts(dat$y, start=c(1,1), freq=12), 
        col=c("red", "black"), main="12-Step Ahead Forecast of Original Series")
ts.plot(y_f2, p_band, col=c("red", "blue", "blue"), lty=c(1, 2, 2), main="12-Step Ahead Forecast")
legend(x="bottomright",
       legend=c("Forecast", "95% Prediction Interval"),col=c("red","blue"),lty=c(1,2),cex=1)

write.xlsx(y_f, file = "Min_Ho_Kim_1003056266.xlsx", col.names=FALSE)





##########
#Below was just some rough work that wasn't directly used in the essay

###########


#monthly dummies 
m1 <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0),n/12)
m2 <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0),n/12)
m3 <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0),n/12)
m4 <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0),n/12)
m5 <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0),n/12)
m6 <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0),n/12)
m7 <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0),n/12)
m8 <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0),n/12)
m9 <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0),n/12)
m10 <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0),n/12)
m11 <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0),n/12)

t= dat$t
t2 = t^2
t3=t^3
t4=t^4
t5=t^5
t6=t^6
t7=t^7
t8=t^8
t9=t^9
t10=t^10

y_ts <- ts(dat$y, freq=12)

mod1 <- lm(y_ts~t+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod2 <- lm(y_ts~t+t2+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod3 <- lm(y_ts~t+t2+t3+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod4 <- lm(y_ts~t+t2+t3+t4+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod5 <- lm(y_ts~t+t2+t3+t4+t5+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod6 <- lm(y_ts~t+t2+t3+t4+t5+t6+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod7 <- lm(y_ts~t+t2+t3+t4+t5+t6+t7+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod8 <- lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod9 <- lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)
mod10 <- lm(y_ts~t+t2+t3+t4+t5+t6+t7+t8+t9+t10+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11)


plot(c(AIC(mod1), AIC(mod2), AIC(mod3), AIC(mod4), 
       AIC(mod5), AIC(mod6), AIC(mod7), AIC(mod8), 
       AIC(mod9), AIC(mod10), AIC(mod11)), type="l",
     ylab="AIC", xlab="Order", main="AIC vs Order of Time Polynomial")

#select mod7 since marginal gains (of lower AIC) become negligible after t^7. 
fitted7 <- ts(mod7$fitted.values, freq=12)
ts.plot(fitted7, y_ts, main="Fitted Trends vs Data", col=c("red", "blue"), xlab="Time (in Years)")
legend(x="topleft",legend=c("Fitted Values", "Data"),col=c("red","blue"),lty=1,cex=1)

resids <- ts(mod7$residuals, freq=12)
ts.plot(resids, main="Residual Series")

#ljung box to test for autocorrelation
i=1
while(i < 13){
  print(Box.test(resids, lag=i, type='Ljung-Box'))
  i = i + 1
}
#will have to model the autocorrelations!

#adf
adf.test(resids)
#series is stationary, since p-value less than 0.05, so we reject the null (presenece of unit root)

par(mfrow=c(1,2))
#check acf and pacf
acf(resids); pacf(resids)

ts_mod1 <- auto.arima(resids)
summary(ts_mod1)

pred1 <- predict(ts_mod1, n.ahead=12)

#residual series prediction
par(mfrow=c(1,1))
ts.plot(resids, pred1$pred)

#determinstic component forecast
t=seq(n+1, n+12,1)
t2 = t^2
t3=t^3
t4=t^4
t5=t^5
t6=t^6
t7=t^7
t8=t^8
t9=t^9
t10=t^10
m1 <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0),1)
m2 <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0),1)
m3 <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0),1)
m4 <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0),1)
m5 <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0),1)
m6 <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0),1)
m7 <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0),1)
m8 <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0),1)
m9 <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0),1)
m10 <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0),1)
m11 <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0),1)

time <- data.frame(t, t2, t3, t4, t5, t6, t7, m1, m2,m3,
                   m4, m5, m6, m7, m8, m9, m10, m11)

m7_f <- predict(mod7, time)
pred7 <- ts(m7_f, freq=12)
ts.plot(pred7, y_ts)












