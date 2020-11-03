# Group Project

# Step 1 - Import the data
library(forecast)
library(urca)
a_temp.data <- read.csv("abnormal_temp.csv")
a_temp.ts <- ts(a_temp.data$a_temp,start=c(1880,1),freq=12)


# Step 2 - Data Visualization & Train - Validation Split
plot(a_temp.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1880-2019")
# There is a clear trend in the data
# Another intertesting but also warning fact: The data seems to has THREE trends
# First period: 1880s-1920s   Flat
# Second period: 1920s-1950s  Rise quickly, and fall a little bit (Probably because of WWII?)
# Third period: 1950s-2019    Rise steadily
# We should see that until most 1940s, most of our data are below zero - don't know how it is calculated, so interpretation remains confusing

# To make model more useful for forecasting, ONLY use data after 1950
# Now, drop data before 1950
a_temp.ts<- window(a_temp.ts,start=c(1950,1))

  # 2.1 Train - Validation Split
train.ts <- window(a_temp.ts,start=c(1950,1),end=c(1999,12))
valid.ts <- window(a_temp.ts,start=c(2000,1),end=c(2019,10))
saveTrain.ts <- train.ts
saveValid.ts <- valid.ts
nTrain <- length(saveTrain.ts)
nValid <- length(saveValid.ts)
T1 <- length(saveTrain.ts)
T2 <- length(saveValid.ts)

  # 2.2 Trend of the data
a_temp_l.lm <- tslm(saveTrain.ts ~ trend)
a_temp_q.lm <- tslm(saveTrain.ts ~ trend + I(trend^2))

# Plot the data (Training Period)
plot(saveTrain.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1950-1999")
lines(a_temp_l.lm$fitted,lwd = 2,col="red")
lines(a_temp_q.lm$fitted,lwd = 2,col="blue")
abline(a = 0, b = 0, lwd=2, lty=2)
legend(1950,0.8,c("Linear Trend","Quadratic Trend"),col=c("red","blue"),lwd=c(2,2))
grid()

# Quick comparasion: Linear or Quadratic trend?
a_temp_l.forecast <- forecast(a_temp_l.lm, h=T2,level=0)
a_temp_q.forecast <- forecast(a_temp_q.lm, h=T2,level=0)

print("Diebold/Mariano Linear versus Quadratic")
print(dm.test(a_temp_l.forecast$mean-saveValid.ts,a_temp_q.forecast$mean-saveValid.ts))
# Quadratic trend model fits the data statiscally better than linear model
# Yet, the difference is not very big (DM = 6.12)

# Now compare the two trend theories with R2 (validation period)

err1.linear <- saveValid.ts - a_temp_l.forecast$mean
err2 <- saveValid.ts - mean(saveValid.ts)
r2valid.linear <- 1 - sum(err1.linear^2)/sum(err2^2)

err1.quadratic <- saveValid.ts - a_temp_q.forecast$mean
r2valid.quadratic <- 1 - sum(err1.quadratic^2)/sum(err2^2)

print(r2valid.linear)
print(r2valid.quadratic)

# The R2 is werid for linear trend in validation period (-0.58)
# For quadratic trend, R2 = 0.32

# From the plot, both linear and quadratic trend captures the training period well
# Use quadratic trend may improve the model, but it also makes it more complicated
# Linear trend may be a good choice in terms of model simplicity, at a cost of less accuarcy
# Decided to use linear trend

  #2.3 Seasonality
plot(window(train.ts,start=c(1950,1),end=c(1959,12)),xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1950-1959")
abline(a = 0, b = 0, lwd=2, lty=2)
grid()

plot(window(a_temp.ts,start=c(1970,1),end=c(1979,12)),xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1970-1979")
abline(a = 0, b = 0, lwd=2, lty=2)
grid()

plot(window(a_temp.ts,start=c(1990,1),end=c(1999,12)),xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1990-1999")
abline(a = 0, b = 0, lwd=2, lty=2)
grid()

# Hard to say whether seasonality exists

# Plot the data and summarize the findings by now

plot(a_temp.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1950-2019")
lines(a_temp_l.lm$fitted,lwd = 2,col="red")
lines(c(2000, 2000), c(-2, 2),lwd=2,col="purple") 
text(1990, -0.25, "Training",cex=1.25)
text(2010, -0.25, "Validation",cex=1.25)
abline(a = 0, b = 0, lwd=2, lty=2)
legend(1950,1.4,c("Linear Trend"),col=c("red"),lwd=c(2,2))
grid()


# Step 3 Model Selection

  # 3.1 Exponential Flitterling
  # It is hard to predict what Exponential Flitterling parameters we should choose due to the difficultities in data interpretation
  # So compare two models: AAN & Automatic Selected Model (By R)
  # Also, let R to decide the value of alpha
ses.aan <- ets(saveTrain.ts, model="AAN")
ses.zzz<- ets(saveTrain.ts, model="ZZZ")
summary(ses.zzz)
  # R believe ANN is the best model - Surprise! It ignores the trend

  # Now, need to choose between AAN and ANN by DM-test and comparing accuarcy of the 12-month (One year) head forecast
  # The code forrecursive modeling for these two models are names as "aan vs ann.R"

# Recursive forecast
flength <- nValid
fvec.aan <- rep(0,flength)
fvec.ann <- rep(0,flength)
ferr.aan <- rep(0,flength)
ferr.ann <- rep(0,flength)

# move into future
for (t in 0:(flength-12)) {
  train.ts <- window(a_temp.ts,end=c(1999,12+t))
  valid.ts <- window(a_temp.ts,start=c(2000,1+t))
  T <- length(train.ts)
  # refit models
  # AAN
  ses.aan.rec <- ets(train.ts, model="AAN")
  # Forecast 12 periods out : 1 year
  ftest.aan <- forecast(ses.aan.rec, h=12)
  fvec.aan[t+12] <- ftest.aan$mean[12]
  ferr.aan[t+12] <- valid.ts[12]-fvec.aan[t+12]
  
  # ANN
  ses.ann.rec <- ets(train.ts, model="ANN")
  # Forecast 12 periods out : 1 year
  ftest.ann <- forecast(ses.ann.rec, h=12)
  fvec.ann[t+12] <- ftest.ann$mean[12]
  ferr.ann[t+12] <- valid.ts[12]-fvec.ann[t+12]
}

# assign dates:
fvec.aan.ts <- ts(fvec.aan,start=c(2000,1),freq=12)
fvec.ann.ts <- ts(fvec.ann,start=c(2000,1),freq=12)
ferr.aan.ts <- ts(ferr.aan,start=c(2000,1),freq=12)
ferr.ann.ts <- ts(ferr.ann,start=c(2000,1),freq=12)

# set windows to kick out first 11 months (these are zeros)
fvec.aan.ts <- window(fvec.aan.ts,start=c(2000,12))
fvec.ann.ts <- window(fvec.ann.ts,start=c(2000,12))
ferr.aan.ts <- window(ferr.aan.ts,start=c(2000,12))
ferr.ann.ts <- window(ferr.ann.ts,start=c(2000,12))

# plot time series
plot(saveValid.ts,xlab="Year",ylab="Global Abnormal Temp",main = "Model Selection: AAN v.s. ANN")
lines(fvec.aan.ts,col='blue',lwd=2)
lines(fvec.ann.ts,col='red',lwd=2)
legend(2000,1.35,c("One year ahead forecast: AAN","One year ahead forecast: ANN"),col=c("blue","red"),lwd=c(2,2))
grid()

# Compare accuracy
print("Accuracy: AAN")
print(accuracy(saveValid.ts,fvec.aan.ts))
print("Accuracy: ANN")
print(accuracy(saveValid.ts,fvec.ann.ts))

print("Diebold/Mariano")
print(dm.test(ferr.aan,ferr.ann))

# AAN does better in both tests
# Keep AAN model ONLY

  # 3.2 ARIMA Model
  # 3.2.1 Unit root test (DF test) - Is the data stationary?
  # AN intertesting question: Which DF test to use ("none" or "trend")
  # Try both
df.test.none <- ur.df(train.ts,type="none",selectlags="BIC")
df.test.trend <- ur.df(train.ts,type="trend",selectlags="BIC")

print("ADF test with NO trend (None)")
print(summary(df.test.none))
# reject the null
# A stationary process

print("trend ADF test")
print(summary(df.test.trend))
# Strongly reject the null
# A stationary process with trend

# Both ADF test concludes that the data is a stationary process (No Unit Root)
# By considering evidence from the plot, we believe our data has a trend
# Therefore, only consider models that deal with trend

  # 3.2.2 AR, MA, or ARMA
acf(saveTrain.ts)
pacf(saveTrain.ts)
  # It seems the data fits a AR(2) process
  # Compare too models: AR(2) and R-selected ARMA
  # Also, try models with trend ONLY
  # And for auto arima, consider model that include or exclude seasonality

  # AR(2) with trend
ar2.trend.mod <- Arima(saveTrain.ts,order=c(2,0,0),xreg = (1:T1))

  # Auto Arima with seasonality
arma.auto.sea.mod <- auto.arima(saveTrain.ts,d=0,ic="bic",xreg = (1:T1),seasonal=TRUE)
  # ARIMA(1,0,1) 
  
  # Auto Arima without seasonality
arma.auto.unsea.mod <- auto.arima(saveTrain.ts,d=0,ic="bic",xreg = (1:T1),seasonal=FALSE)
  # ARIMA(1,0,1)

  # R exclude seasonality in auto.arima even if allow seasonal=TRUE
  # Therefore, the auto selected model of ARIMA is ARMA(1,1), with trend

arma.auto.mod <- Arima(saveTrain.ts,order=c(1,0,1),xreg = (1:T1))

  # Now need to compared the two models: AR(2) with trend and ARMA(1,1) with trend

# Recursive forecast
flength <- nValid
fvec.ar2 <- rep(0,flength)
fvec.arma11 <- rep(0,flength)
ferr.ar2 <- rep(0,flength)
ferr.arma11 <- rep(0,flength)

# move into future
for (t in 0:(flength-12)) {
  train.ts <- window(a_temp.ts,end=c(1999,12+t))
  valid.ts <- window(a_temp.ts,start=c(2000,1+t))
  T <- length(train.ts)
  # refit models
  # AR(2) with trend
  ar2.trend.mod.rec <- Arima(train.ts,order=c(2,0,0),xreg = (1:T))
  # Forecast 12 periods out : 1 year
  ftest.ar2 <- forecast(ar2.trend.mod.rec,xreg=(T+1:(T+13)))
  fvec.ar2[t+12] <- ftest.ar2$mean[12]
  ferr.ar2[t+12] <- valid.ts[12]-fvec.ar2[t+12]
  
  # arma11
  arma.auto.trend.mod.rec <- Arima(train.ts,order=c(1,0,1),xreg = (1:T))
  # Forecast 12 periods out : 1 year
  ftest.arma11 <- forecast(arma.auto.trend.mod.rec,xreg=(T+1:(T+13)))
  fvec.arma11[t+12] <- ftest.arma11$mean[12]
  ferr.arma11[t+12] <- valid.ts[12]-fvec.arma11[t+12]
}

# assign dates:
fvec.ar2.ts <- ts(fvec.ar2,start=c(2000,1),freq=12)
fvec.arma11.ts <- ts(fvec.arma11,start=c(2000,1),freq=12)
ferr.ar2.ts <- ts(ferr.ar2,start=c(2000,1),freq=12)
ferr.arma11.ts <- ts(ferr.arma11,start=c(2000,1),freq=12)

# set windows to kick out first 11 months (these are zeros)
fvec.ar2.ts <- window(fvec.ar2.ts,start=c(2000,12))
fvec.arma11.ts <- window(fvec.arma11.ts,start=c(2000,12))
ferr.ar2.ts <- window(ferr.ar2.ts,start=c(2000,12))
ferr.arma11.ts <- window(ferr.arma11.ts,start=c(2000,12))

# plot time series
plot(saveValid.ts,xlab="Year",ylab="Global Abnormal Temp",main = "Model Selection: ar2 v.s. arma11")
lines(fvec.ar2.ts,col='blue',lwd=2)
lines(fvec.arma11.ts,col='red',lwd=2)
legend(2000,1.35,c("One year ahead forecast: ar2","One year ahead forecast: arma11"),col=c("blue","red"),lwd=c(2,2))
grid()

# Compare accuracy
print("Accuracy: ar2")
print(accuracy(saveValid.ts,fvec.ar2.ts))
print("Accuracy: arma11")
print(accuracy(saveValid.ts,fvec.arma11.ts))

print("Diebold/Mariano")
print(dm.test(ferr.ar2,ferr.arma11))

  # ARMA (1,1) with trend is significantly better than AR(2) with trend
  # Therefore, keep ARMA (1,1) with trend ONLY
  
  # ARIMA(1,0,1) with trend is better than AR(2) with trend
  # Keep ARIMA(1,0,1) with trend

  # 3.2.3 Naive forecasts
#naive 
temp.naive.meanf  <- meanf(saveTrain.ts, h = T2)
temp.naive.last   <- rwf(saveTrain.ts, h = T2, level=95)
temp.snaive       <- snaive(saveTrain.ts, h = T2,drift=FALSE,level=95)
temp.naive.drift  <- rwf(saveTrain.ts, h = T2,drift=TRUE, level=95)
temp.snaive.drift <- snaive(saveTrain.ts, h = T2,drift=TRUE,level=95)

plot(saveValid.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 2000-2019(Validation)", ylim=c(0,1.5))
lines(temp.naive.meanf$mean,col="orange",lwd=2) #can be deleted
lines(temp.naive.last$mean,col="green",lwd=2)
lines(temp.snaive$mean,col="blue",lwd=2)
lines(temp.naive.drift$mean,col="red",lwd=2)
lines(temp.snaive.drift$mean,col="purple",lwd=2)
legend(2000,1.35,c("mean","naive","seasonal naive","naive with drift","seasonal naive with drift"),col=c("orange","green","blue","red","purple"),lwd=c(2,2))
grid()
# turns out seasonal naive and seasonal naive with drift are the same forecast

# Keep naive forecast with drift becasue from the graph, it apparently does better than all other navie models

# 4 Final Model Selection
# Quadratic, AAN, ARMA (1,1) with trend, and naive forecast with drift

# Recursive forecast
flength <- nValid
fvec.naive.drift <- rep(0,flength)
ferr.naive.drift <- rep(0,flength)

# move into future
for (t in 0:(flength-12)) {
  train.ts <- window(a_temp.ts,end=c(1999,12+t))
  valid.ts <- window(a_temp.ts,start=c(2000,1+t))
  T <- length(train.ts)
  # refit models
  # naive.trend
  temp.naive.drift.rec <- rwf(train.ts,h=12,drift=TRUE,level=95)
  # Forecast 12 periods out : 1 year
  ftest.naive.drift <- temp.naive.drift.rec
  fvec.naive.drift[t+12] <- ftest.naive.drift$mean[12]
  ferr.naive.drift[t+12] <- valid.ts[12]-fvec.naive.drift[t+12]
}

# assign dates:
fvec.naive.drift.ts <- ts(fvec.naive.drift,start=c(2000,1),freq=12)
ferr.naive.drift.ts <- ts(ferr.naive.drift,start=c(2000,1),freq=12)

# set windows to kick out first 11 months (these are zeros)
fvec.naive.drift.ts <- window(fvec.naive.drift.ts,start=c(2000,12))
ferr.naive.drift.ts <- window(ferr.naive.drift.ts,start=c(2000,12))

# plot time series
plot(saveValid.ts,xlab="Year",ylab="Global Abnormal Temp",main = "Final Model Selection")
lines(fvec.aan.ts,col='blue',lwd=2)
lines(fvec.arma11.ts,col='red',lwd=2)
lines(fvec.naive.drift.ts,col='orange',lwd=2)
legend(2000,1.35,c("AAN","ARMA(1,1) with trend","Naive with drift"),col=c("blue","red","orange"),lwd=c(2,2))
grid()

# Compare accuracy
print("Accuracy: AAN")
print(accuracy(saveValid.ts,fvec.aan.ts))
print("Accuracy: ARMA(1,1)")
print(accuracy(saveValid.ts,fvec.arma11.ts))
print("Accuracy: Naive with Drift")
print(accuracy(saveValid.ts,fvec.naive.drift.ts))
# Accuracy: AAN > ARMA(1,1) >> Naive with drift

# DM test
print("Diebold/Mariano: AAN vs ARMA(1,1)")
print(dm.test(ferr.aan,ferr.arma11))
# No decisive conclusion

print("Diebold/Mariano: AAN vs Naive with drift")
print(dm.test(ferr.aan,ferr.naive.drift))
# AAN is better than naive with drift

print("Diebold/Mariano: ARMA(1,1) vs Naive with drift")
print(dm.test(ferr.arma11,ferr.naive.drift))
# No decisive conclusion

# Final conclusion: AAN is best models
# But ARMA(1,1) with trend is still a good candidate
# RMSE: AAN < ARMA(1,1)
# DM-test: No decisive conclusion
# Depends on the purpose of the research
# ANN is more responsive to change
# ARMA is more stable, and less sensitive to change

# Look at the graph one more time
plot(saveValid.ts,xlab="Year",ylab="Global Abnormal Temp",main = "Final Model Selection")
lines(fvec.aan.ts,col='blue',lwd=2)
lines(fvec.arma11.ts,col='red',lwd=2)
legend(2000,1.35,c("AAN","ARMA(1,1) with trend"),col=c("blue","red"),lwd=c(2,2))
grid()

# The final graph:
plot(a_temp.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Global Abnormal Temp 1950-2019")
lines(fvec.aan.ts,col='blue',lwd=1)
lines(fvec.arma11.ts,col='red',lwd=1)
lines(c(2000, 2000), c(-2, 2),lwd=2,col="purple") 
text(1990, -0.25, "Training",cex=1.25)
text(2010, -0.25, "Validation",cex=1.25)
abline(a = 0, b = 0, lwd=2, lty=2)
legend(1950,1.4,c("AAN","ARMA(1,1) with trend"),col=c("blue","red"),lwd=c(2,2))
grid()

# See how quadratic trend may fit in the model

#quaratic trend recursive
flength <- nValid
fvec.q <- rep(0,flength)
ferr.q <- rep(0,flength)

# move into future
for (t in 0:(flength-12)) {
  train.ts <- window(a_temp.ts,end=c(1999,12+t))
  valid.ts <- window(a_temp.ts,start=c(2000,1+t))
  T <- length(train.ts)
  # refit models
  # Quadratic trend model
  temp.qtrend <- tslm(train.ts ~ trend + I(trend^2))
  # Forecast 12 periods out : 1 year
  ftest.q <- forecast(temp.qtrend,h = 12)
  fvec.q[t+12] <- ftest.q$mean[12]
  ferr.q[t+12] <- valid.ts[12]-fvec.q[t+12]
} 
# assign dates:
fvec.q.ts<- ts( fvec.q,start=c(2000,1),freq=12)
ferr.q.ts<- ts(ferr.q,start=c(2000,1),freq=12)

# set windows to kick out first 11 months (these are zeros)
fvec.q.ts <- window(fvec.q.ts,start=c(2000,12))
ferr.q.ts <- window(ferr.q.ts,start=c(2000,12))

print(accuracy(saveValid.ts,ferr.q.ts))
print(dm.test(ferr.q.ts,ferr.arma11.ts))
print(dm.test(ferr.q.ts,ferr.aan.ts))

# Surprise: Quadratic trend model may be the best model

# Now plot the data
plot(a_temp.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Model Selection (1950-2019)")
lines(a_temp_q.lm$fitted,lwd = 1.8,col="darkgreen")
lines(a_temp_q.forecast$mean, lwd = 1.8, col="darkgreen")
lines(fvec.aan.ts, lwd = 1.8, col="blue")
lines(fvec.arma11.ts, lwd = 1.8, col="red")
lines(fvec.naive.drift.ts,col='orange',lwd=1.5)
lines(c(2000, 2000), c(-2, 2),lwd=2,col="purple") 
text(1990, -0.25, "Training",cex=1.25)
text(2010, -0.25, "Validation",cex=1.25)
legend(1950,1.4,c("Actual","AAN","ARMA(1,1) with trend","Naive with drift","Quadratic"),
       col=c("black","blue","red","orange","darkgreen"),lwd=c(2,2))
grid()

# Or Zoom In to Validation Period
plot(saveValid.ts,xlab="Year",ylab="Global Abnormal Temp",bty="l",main="Model Selection (2000-2019), Validation",lwd=1)
lines(a_temp_q.forecast$mean, lwd = 1.8, col="darkgreen")
lines(fvec.aan.ts, lwd = 1.8, col="blue")
lines(fvec.arma11.ts, lwd = 1.8, col="red")
lines(fvec.naive.drift.ts,col='orange',lwd=1.5)
legend(2000,1.4,c("Actual","AAN","ARMA(1,1) with trend","Naive with drift","Quadratic"),
       col=c("black","blue","red","orange","darkgreen"),lwd=c(2,2))
grid()

