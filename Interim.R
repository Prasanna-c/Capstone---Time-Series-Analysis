# Hierarchical Time Series Analysis.
library(readr)
library(tidyverse)
library(fabletools)
library(fpp3)
library(forecast)
library(tseries)
library(hts)
library(ggplot2)
library(dlookr)
library(outliers)
library(dplyr)
library(psych)
library(zoo)
top8 <- read.csv(choose.files())

str(top8)

top8$Material <- as.character(top8$Material)
top8$Date <- as.Date(as.yearmon(top8$Date))
top8$Plant <- as.character(top8$Plant)
top8$AMU <- as.numeric(top8$AMU)


# Outlier Analysis

boxplot(top8$AMU, main="Purchases", boxwex=0.1)
outlier_top8 <- boxplot.stats(top8$AMU)
head(outlier_top8)

# These are original purchase values and cannot be considered as outliers.


sum(top8$quantity<0)
# 0

#These are raw materials used for production and nothing returned.


# Top sourced products are as below.

summary(top8$AMU ~ top8$Material)
# 1. 11373, 2. 1374, 3. 2985

# lets analyze the top 3 products

# subsetting the data to product level.

prod_11373 <- subset(top8, Material == '11373')
prod_1374 <- subset(top8, Material == '1374')
prod_2985 <- subset(top8, Material == '2985')


# Creating Time Series.

p.11373 <- ts(prod_11373[,4], start=c(2013, 3), end=c(2020, 2), frequency=12)
p.1374 <- ts(prod_1374[,4], start=c(2013, 3), end=c(2020, 2), frequency=12)
p.2985 <- ts(prod_2985[,4], start=c(2013, 3), end=c(2020, 2), frequency=12)


summary(p.11373)

summary(p.1374)

summary(p.2985)

# p.11373

ggtsdisplay(p.11373)

autoplot(decompose(p.11373))

plot(p.11373, xlab = 'Years', ylab = 'Purchases', 
     main = 'Purchasing Pattern of Material Code - 11373',col = 'cadetblue')


# AB line to check the trend line.

abline(reg=lm(p.11373 ~ time(p.11373)), col = "firebrick1")



# season plot

ggseasonplot(p.11373, xlab = 'Years', ylab = 'Purchases', 
             main = 'Seasonal Purchase Pattern of Material Code - 11373')

# decompose

decompose(p.11373)

plot(decompose(p.11373), col = 'cadetblue')


# Boxplot.

boxplot(p.11373 ~ cycle(p.11373), col = 'cadetblue', 
        main = 'Monthly Purchase Pattern of Material Code - 11373',
        ylab = 'Quantity', xlab = 'Months')

# p.1374

ggtsdisplay(p.1374)

autoplot(decompose(p.1374))

plot(p.1374, xlab = 'Years', ylab = 'Purchases', 
     main = 'Purchasing Pattern of Material Code - 1374',col = 'cadetblue')

abline(reg=lm(p.1374 ~ time(p.1374)), col = "firebrick1")


# season plot

ggseasonplot(p.1374, xlab = 'Years', ylab = 'Purchases', 
             main = 'Seasonal Purchase Pattern of Material Code - 1374')

# decompose

decompose(p.1374)

plot(decompose(p.1374), col = 'cadetblue')


# Boxplot.

boxplot(p.1374 ~ cycle(p.1374), col = 'cadetblue', 
        main = 'Monthly Purchase Pattern of Material Code - 1374',
        ylab = 'Quantity', xlab = 'Months')



# p.2985

ggtsdisplay(p.2985)

autoplot(decompose(p.2985))


plot(p.2985, xlab = 'Years', ylab = 'Purchases', 
     main = 'Purchasing Pattern of Material Code - 2985',col = 'cadetblue')

abline(reg=lm(p.2985 ~ time(p.2985)), col = "firebrick1")


# season plot

ggseasonplot(p.2985, xlab = 'Years', ylab = 'Purchases', 
             main = 'Seasonal Purchase Pattern of Material Code - 2985')

# decompose

decompose(p.2985)

plot(decompose(p.2985), col = 'cadetblue')


# Boxplot.

boxplot(p.2985 ~ cycle(p.1374), col = 'cadetblue', 
        main = 'Monthly Purchase Pattern of Material Code - 2985',
        ylab = 'Quantity', xlab = 'Months')




# ADF Test

adf.test(p.11373)

# p-value = 0.03956 - Data is stationary.


# kpss.test

kpss.test(p.11373)

# P value is 0.04855, looks like it is not stationary.

ndiffs(p.11373)
# d = 1




adf.test(p.1374)
# p-value = 0.2086 - Data is Non - stationary.

adf.test(p.2985)
# p-value = 0.01 - Data is stationary.



# kpss.test

kpss.test(p.11373)

# P value is 0.04855, looks like it is not stationary.

ndiffs(p.11373)
# d = 1


kpss.test(p.1374)

# P value is 0.1, date is stationary.

ndiffs(p.1374)
# d = 0, no differencing is required

kpss.test(p.2985)
# P value is 0.1, date is stationary.

ndiffs(p.2985)
# d = 0, no differencing is required

# 1 differencing is required to make the data stationary for product 11373.

autoplot(diff(p.11373))

# Seasonality can be handled by taking log.

autoplot(diff(log(p.11373)))


# Stationarity check.

adf.test((diff(log(p.11373))))
# P value is 0.01, data is now stationary.

kpss.test((diff(log(p.11373))))
# P value is greater than alpha and data is stationary.

ggAcf((diff(log(p.11373))))


# Splitting Train & Test data.

train.data <- window(p.11373, start=c(2013,3), end=c(2018, 2)) 
valid.data <- window(p.11373, start=c(2018,3), end=c(2020,2))



plot(train.data, xlab = 'Years', ylab = 'Purchases', 
     main = 'Train Date - 11373',col = 'cadetblue')

plot(log(train.data), xlab = 'Years', ylab = 'Purchases', 
     main = 'Train Date - 11373',col = 'cadetblue')


# Auto Arima model.

model.arima<-auto.arima(train.data)
summary(model.arima)

autoplot(model.arima)

checkresiduals(model.arima)

#Checking for Correlation between residuals
Box.test(model.arima$residuals, type = "Ljung-Box")

#p-values is 0.22, there is no correlation

#Checking for mean of residuals

mean(model.arima$residuals)
# -1.212709e-12

#Checking for Normality of Residuals

shapiro.test(model.arima$residuals)
# 0.01919

checkresiduals(model.arima)


# Predicting for next 24 periods

model.predict<-predict(model.arima,n.ahead = 24)

model.predict<-exp(model.predict$pred)

model.predict

autoplot(forecast(model.arima, h=24))+ylab("Purchases")+xlab("Year")

accuracy(model.predict, valid.data)


MAAPE(model.predict, valid.data, na.rm = TRUE)


#Tuning the values

acf(log(train.data)) #q =1
pacf(log(train.data)) #p = 2



model.arima1<-arima(log(train.data), order = c(0,1,1), seasonal = list(order=c(0,1,1), period=12))
model.predict1<-predict(model.arima1,n.ahead = 24)
model.predict1<-exp(model.predict1$pred)
summary(model.arima1)



MAAPE(model.predict1, valid.data, na.rm = TRUE)



autoplot(forecast(model.predict1, h=24))+ylab("Purchases")+xlab("Year")

# Linear Regression

prod_lm <- subset(mydata, Material.Description == '11373')

library(caTools)
set.seed(7)
spl = sample.split(mydata$AMU, SplitRatio = 0.7)
train_data = subset(mydata, spl==TRUE)
test_data = subset(mydata, spl==FALSE)



lm1 <- lm(mydata$AMU ~ mydata$Total.Cost + mydata$Discount, train_data)
summary(lm1)

pred_lm <- predict(lm1, data = test_data)

accuracy(lm1$fitted.values, test_data$AMU)

RMSE(pred_lm)

lm1$fitted.values
