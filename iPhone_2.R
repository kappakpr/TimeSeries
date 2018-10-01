rm(list=ls())
setwd("C:\\RProgs")

library(TSA) 
library(data.table) 
data=read.csv("iPhone_data.csv",header=T)
temp=ts(data[,"iPhone_sales"],start=2008, freq=4)

## Process Dates
year = data$Year
class(year)
month = data$Quarter * 3
day = as.list(rep(15, 38))
datemat = cbind(as.character(day),as.character(month),as.character(year))
paste.dates = function(date){
  day = date[1]; month=date[2]; year = date[3]
  return(paste(day,month,year,sep="/"))
}
dates = apply(datemat,1,paste.dates)
dates = as.Date(dates, format="%d/%m/%Y")
data = cbind(dates,data)

attach(data)

par(mfrow=c(2,2))
plot(temp,main="iphone data")
acf(temp,main="iphone data acf")
pacf(temp,main="iphone data pacf")

#difference first order
xts = diff(temp, lag=1,differences = 1)
ts.plot(xts,ylab="iphone data Differenced")
acf(xts,main="iphone data Differenced acf")
pacf(xts,main="iphone data Differenced pacf")

#log transform
temp.tr = log(temp)
#temp.tr = log(xts) # In log(xts) : NaNs produced
par(mfrow=c(2,2))
plot(temp.tr,main="log iphone data")
acf(temp.tr,main="log iphone data acf")
pacf(temp.tr,main="log iphone data pacf")

# Differencing to Remove Trend
diff.temp.tr = diff(temp.tr)
par(mfrow=c(2,2))
plot(diff.temp.tr,main="one diff")
acf(as.vector(diff.temp.tr),main="ACF: One-Lag Difference Iphone sales")
pacf(diff.temp.tr,main="PACF: One-Lag Difference Iphone sales")

# Differencing to Remove Trend - 2nd order
diff2.temp.tr = diff(temp.tr, lag=1,differences = 1)
par(mfrow=c(2,2))
plot(diff2.temp.tr,main="one diff")
acf(as.vector(diff2.temp.tr),main="ACF: One-Lag Difference Iphone sales")
pacf(diff2.temp.tr,main="PACF: One-Lag Difference Iphone sales")


## Apply ARIMA

## Order selection -- AIC 
n = length(temp.tr)
norder = 6
p = c(1:norder)-1; q = c(1:norder)-1
aic = matrix(0,norder,norder)
for(i in 1:norder){
  for(j in 1:norder){
    modij = arima(temp.tr,order = c(p[i],1,q[j]), method='ML')
    #modij = arima(diff.temp.tr,order = c(p[i],1,q[j]), method='ML')
    #Warning messages:
    #  1: In log(s2) : NaNs produced
    #2: In stats::arima(x = x, order = order, seasonal = seasonal, xreg = xreg,  : possible convergence problem: optim gave code = 1
    aic[i,j] = modij$aic-2*(p[i]+q[j]+1)+2*(p[i]+q[j]+1)*n/(n-p[i]-q[j]-2)
  }  
}

aicv = as.vector(aic)  
par(mfrow=c(1,1))
plot(aicv,ylab="AIC values")
indexp = rep(c(1:norder),norder)
indexq = rep(c(1:norder),each=norder)
indexaic = which(aicv == min(aicv))
porder = indexp[indexaic]-1
qorder = indexq[indexaic]-1

final_model = arima(temp.tr, order = c(porder,1,qorder), method = "ML")
summary(final_model)
final_model$coef

## GOF: residual analysis
par(mfrow=c(2,2))
plot(resid(final_model), ylab='Residuals',type='o',main="Residual Plot")
abline(h=0)
acf(resid(final_model),main="ACF: Residuals")
hist(resid(final_model),xlab='Residuals',main='Histogram: Residuals')
qqnorm(resid(final_model),ylab="Sample Q",xlab="Theoretical Q")
qqline(resid(final_model))

Box.test(final_model$resid, lag = (porder+qorder+1), type = "Box-Pierce", fitdf = (porder+qorder))
Box.test(final_model$resid, lag = (porder+qorder+1), type = "Ljung-Box", fitdf = (porder+qorder))

par(mfrow=c(1,1))

## Forecasting with ARIMA 
## 6 last point: 
n = length(temp.tr)
nfit = n-6
outprice = arima(temp.tr[1:nfit], order = c(porder,1,qorder),method = "ML")
outpred = predict(outprice,n.ahead=6)
ubound = outpred$pred+1.96*outpred$se
lbound = outpred$pred-1.96*outpred$se
ymin = min(exp(lbound))
ymax = max(exp(ubound))
plot(dates[(n-10):n], exp(temp.tr)[(n-10):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Iphone sales")
points(dates[(nfit+1):n],exp(outpred$pred),col="red")
lines(dates[(nfit+1):n],exp(ubound),lty=3,lwd= 2, col="blue")
lines(dates[(nfit+1):n],exp(lbound),lty=3,lwd= 2, col="blue")


## Compute Accuracy Measures
obsprice = exp(temp.tr[(nfit+1):n]) 
predprice = exp(outpred$pred)
### Mean Squared Prediction Error (MSPE)
mean((predprice-obsprice)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(predprice-obsprice))
### Mean Absolute Percentage Error (MAPE)
mean(abs(predprice-obsprice)/obsprice)
### Precision Measure (PM)
sum((predprice-obsprice)^2)/sum((obsprice-mean(obsprice))^2)

### Does the observed data fall outside the prediction intervals?
sum(obsprice<exp(lbound))+sum(obsprice>exp(ubound))

## Daily Prediction over a period of 6 observations 
outpred.10 = NULL
ubound.10 = NULL
lbound.10 = NULL
n = length(temp.tr)
for(i in 1:6){
  nfit = n-(6-i-1)
  outprice = arima(temp.tr[1:nfit], order = c(porder,1,qorder),method = "ML")
  pred.1 = predict(outprice,n.ahead=1)
  outpred.10 = c(outpred.10,pred.1$pred)
  ubound.10 = c(ubound.10, pred.1$pred+1.96*pred.1$se)
  lbound.10 = c(lbound.10, pred.1$pred-1.96*pred.1$se)
}

## Compute Accuracy Measures 
predprice.10 = exp(outpred.10)
### Mean Squared Prediction Error (MSPE)
mean((predprice.10-obsprice)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(predprice.10-obsprice))
### Mean Absolute Percentage Error (MAPE)
mean(abs(predprice.10-obsprice)/obsprice)
### Precision Measure (PM)
sum((predprice.10-obsprice)^2)/sum((obsprice-mean(obsprice))^2)

### Does the observed data fall outside the prediction intervals?
sum(obsprice<exp(lbound.10))+sum(obsprice>exp(ubound.10))

nfit = n-6
ymin = min(exp(c(lbound,lbound.10)))
ymax = max(exp(c(ubound,ubound.10)))

plot(dates[(n-10):n], exp(temp.tr)[(n-10):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Iphone sales")
points(dates[(nfit+1):n],exp(outpred$pred),col="red")
lines(dates[(nfit+1):n],exp(ubound),lty=3,lwd= 2, col="blue")
lines(dates[(nfit+1):n],exp(lbound),lty=3,lwd= 2, col="blue")
points(dates[(nfit+1):n],exp(outpred.10),col="green")
lines(dates[(nfit+1):n],exp(ubound.10),lty=3,lwd= 2, col="brown")
lines(dates[(nfit+1):n],exp(lbound.10),lty=3,lwd= 2, col="brown")
legend(x="topleft",cex = 0.55,dates[n-10],legend=c("1 Lag Ahead Prediction", "1 Lag Ahead Prediction Interval",
                                        "6 Lags Ahead Prediction","6 Lags Ahead Prediction Interval" ),col=c("green","brown","red","blue"),pch=1,lty = c(0,3,0,3))
