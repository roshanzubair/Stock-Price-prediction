library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

start <- as.Date("2010-01-01")
end <- as.Date("2018-04-13")

getSymbols("AMZN", src = "yahoo", from = start, to = end)
AMZN<-data.frame(as.matrix(AMZN))

library(data.table)
AMZN<-setDT(AMZN, keep.rownames = TRUE)[]
AMZN$Date<-as.Date(AMZN$rn)
AMZN$rn<-NULL

library(Hmisc)
AMZN$lag_year1 <- Lag(AMZN$AMZN.Close, 252)
months<-format(AMZN$Date,format="%B")

class.ind <- function(cl)
{
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)) )
  x[(1:n) + n*(unclass(cl)-1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}

AMZN<-cbind(AMZN,class.ind(months))
AMZN<-cbind(AMZN,class.ind(weekdays(AMZN$Date)))
colnames(AMZN)[colnames(AMZN)=="AMZN.Close"]<-"close_price"

AMZN$log_price = diff(log(AMZN$close_price),lag=1)
AMZN$lag1 <- Lag(AMZN$log_price, 1)
AMZN$lag2 <- Lag(AMZN$log_price, 2)
AMZN$lag3 <- Lag(AMZN$log_price, 3)
AMZN$lag4 <- Lag(AMZN$log_price, 4)
AMZN$lag5 <- Lag(AMZN$log_price, 5)
AMZN$lag6 <- Lag(AMZN$log_price, 6)
AMZN$lag7 <- Lag(AMZN$log_price, 7)

#EDA

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(magrittr)

df <- AMZN %>%
  select(Date, close_price) %>%
  gather(key = "variable", value = "stock_price", -Date)
head(df, 3)

# line plot
ggplot(data = AMZN, aes(x = Date, y = AMZN.High))+
  geom_line(color = "#00AFBB", size = 2)

# Area plot
p<-ggplot(df, aes(x = Date, y = stock_price)) + 
  geom_area(aes(color = variable, fill = variable), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = "#00AFBB") +
  scale_fill_manual(values = "#00AFBB")

p

p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)

## arima function

#checking directionality

adf.test(AMZN$close_price)
Amzn_stock<-AMZN$close_price[1:2000]
autoplot(acf(Amzn_stock, plot = FALSE), conf.int.fill = '#0000FF', conf.int.value = 0.8, conf.int.type = 'ma')
autoplot(pacf(Amzn_stock, plot = FALSE), conf.int.fill = '#00AFBB', conf.int.value = 0.8, conf.int.type = 'ma')
arima_model<-auto.arima(Amzn_stock)
ggtsdiag(arima_model)

summary(arima_model)
aheads<-predict(arima_model,n.ahead=83)

# Create a table for the accuracy of the forecast
comparsion = data.frame(cbind(AMZN$close_price[2001:2083],aheads$pred))
a<-diff(comparsion$AMZN.close_price.2001.2083.,lag=1)
b=diff(comparsion$aheads.pred,lag=1)
Accuracy = sign(a)==sign(b)
print(cbind(a,b,Accuracy))

# Compute the accuracy percentage metric
Accuracy_percentage = sum(Accuracy == 1)*100/length(Accuracy)
print(Accuracy_percentage)
stargazer(arima_model, type = "html", 
          dep.var.labels.include = FALSE,
          model.numbers          = FALSE)

# checking next value

arima_model<-arima(amzn_tweet$close_price[1:40],order=c(1,2,1),xreg = amzn_tweet$neg_tweet[1:40] )
ggtsdiag(arima_model)
plot(forecast(arima_model,h = 22,xreg =amzn_tweet$neg_tweet[41:62] ))
summary(arima_model)
aheads<-predict(arima_model,n.ahead=22,newxreg=amzn_tweet$neg_tweet[41:62])
mean(abs(aheads$pred-amzn_tweet$close_price[41:62])/amzn_tweet$close_price[41:62]) #4%
s1<-cbind(AMZN$close_price[1351:1484],aheads1) 
View(s1)

## UCM

library(rucm)

ucm_model<-ucm(log_price~0, data=AMZN[2020:2060,], irregular = TRUE, irregular.var = NA, level = TRUE,
               level.var = NA, slope = TRUE, slope.var = NA, season = FALSE,
               season.length = NA, season.var = NA, cycle = FALSE, cycle.period = NA,cycle.var = NA)


a<-predict(ucm_model,n.ahead=23)

# Create a table for the accuracy of the forecast
comparsion = data.frame(cbind(AMZN$log_price[2061:2083],a))
comparsion$Accuracy = sign(comparsion$AMZN.log_price.2061.2083.)==sign(comparsion$a)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

model<-arima(log(AMZN$close_price[1000:1350]),order = c(1,1,1 ),xreg = FB$lag_year1[1000:1350])
summary(model)
aheads<-predict(model,n.ahead=134,newxreg = FB$lag_year1[1351:1484])
aheads1<-2.718^aheads$pred
summary(aheads1)
mean(abs(aheads1-AMZN$close_price[1351:1484])/AMZN$close_price[1351:1484]) 

#read.csv 

tweet_data<-read.csv("C:/Users/rosha/OneDrive - University of Connecticut/DM BI/amazon_stock_sentiment.csv")
tweet_data$file<-as.Date(tweet_data$file)

amzn_tweet<-merge(AMZN,tweet_data,by.x="Date",by.y="file")
amzn_tweet$neg_tweet_1 = Lag(amzn_tweet$neg_tweet, 1)

ucm_model<-ucm(log_price~neg_tweet, data=amzn_tweet[2:40,], irregular = TRUE, irregular.var = NA, level = TRUE,
               level.var = NA, slope = TRUE, slope.var = NA, season = FALSE,
               season.length = NA, season.var = NA, cycle = FALSE, cycle.period = NA,cycle.var = NA)

modelVar <- "neg_tweet"
test_data<-amzn_tweet[41:62,]
fitData <- SSModel(as.formula(paste0("rep(NA,nrow(test_data)) ~ ", modelVar, "+ SSMtrend(2, Q = list(ucm_model$est.var.level,ucm_model$est.var.slope))")),
                   H = ucm_model$irr.var, data = test_data)
a<-predict(ucm_model$model,newdata=fitData)
fitValue <- data.frame(predict(ucm_model$model,newdata=fitData),amzn_tweet[41:62,"Date"])

ggplot() + 
  geom_line(data = amzn_tweet, aes(x = Date, y = log_price), color = "#00AFBB",size=1) +
  geom_line(data = fitValue, aes(x = Date, y = fit), color = "#E7B800",size=2) +
  xlab('Date') +
  ylab('Change in stock price')

# Create a table for the accuracy of the forecast
comparsion = data.frame(cbind(amzn_tweet$log_price[41:62],fitValue))
comparsion$Accuracy = sign(comparsion$amzn_tweet.log_price.41.62.)==sign(comparsion$fit)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

#Load Library
library(e1071)

#Scatter Plot
plot(data)

#Regression with SVM
modelsvm = svm(log_price~lag1+lag2+lag3+lag4+lag5+lag6+lag7+Friday+pos_tweet,amzn_tweet[1:40,])

#Predict using SVM regression
predYsvm = predict(modelsvm, amzn_tweet[41:62,])

# Create a table for the accuracy of the forecast
comparsion = data.frame(cbind(amzn_tweet$log_price[41:62],predYsvm))
comparsion$Accuracy = sign(comparsion$V1)==sign(comparsion$predYsvm)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

## Neural Network

fit <- nnetar(amzn_tweet$log_price[1:40],size = 4, xreg = amzn_tweet[1:40,"pos_tweet"])

predNN<-forecast(fit,h=22,xreg = amzn_tweet[41:62,"pos_tweet"])
predNN<-data.frame(cbind(amzn_tweet[41:62,"Date"]),predNN)
ggplot() + 
  geom_line(data = amzn_tweet, aes(x = Date, y = log_price), color = "#00AFBB",size=1) +
  geom_line(data = predNN, aes(x = Date, y = Point.Forecast), color = "#E7B800",size=2) +
  xlab('Date') +
  ylab('Change in stock price')

# Create a table for the accuracy of the forecast
comparsion = data.frame(cbind(amzn_tweet$log_price[41:62],predNN$mean))
comparsion$Accuracy = sign(comparsion$amzn_tweet.log_price.41.62.)==sign(comparsion$predNN.mean)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)

## holt winters model

x<-ts(AMZN[1500:2052,"AMZN.Close"],start=c(2015,12),frequency = 252)
modelhw<-HoltWinters(diff(log(x),lag=1))
plot(x)

plot(modelhw)
forecast <- predict(modelhw, n.ahead = 32, prediction.interval = T, level = 0.95)
plot(modelhw, forecast)

# Create a table for the accuracy of the forecast
comparsion = data.frame(cbind(AMZN[2052:2083],forecast))
comparsion$Accuracy = sign(diff(log(comparsion$AMZN.Close),lag=1))==sign(comparsion$fit)
print(comparsion)

# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)
