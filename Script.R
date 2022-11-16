list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(boot)
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(plyr)


Path<-"D:/R assignments/Case Study 2"
setwd(Path)
getwd()

#Load the data set. Split the data set into training and testing sets as follows: place all observations for 2012 and earlier in the training set, and all observations for 2013 and 2014 into the testing set.How many observations are in the training set?
  
data=read.csv("elantra.csv")
data1=data


str(data1)
summary(data1)

set.seed(123)

train.data = subset(data1, Year<=2012)
str(train.data)
dim(train.data)

test.data = subset(data1, Year>= 2013)
str(test.data)
dim(test.data)

#Build a linear regression model to predict monthly Elantra sales using Unemployment, CPI_all, CPI_energy and Queries as the independent variables. Use all of the training set data to do this.What is the model R-squared?
Sales <- lm(ElantraSales ~ Unemployment + Queries+ CPI_energy+ CPI_all, data = train.data)
summary(Sales)

#In our problem, since our data includes the month of the year in which the units were sold, it is feasible for us to incorporate monthly seasonality. From a modeling point of view, it may be reasonable that the month plays an effect in how many Elantra units are sold.To incorporate the seasonal effect due to the month, build a new linear regression model that predicts monthly Elantra sales using Month as well as Unemployment, CPI_all, CPI_energy and Queries. Do not modify the training and testing data frames before building the model. What is the model RSquared
MonthSales <- lm(ElantraSales ~ Unemployment + Month + Queries+ CPI_energy+ CPI_all, data = data1)
summary(MonthSales)

#Re-run the regression with the Month variable modeled as a factor variable. What is the model RSquared?
Months <- as.factor(data1$Month)
data1$Months <- as.factor(data1$Month)
str(data1)
train <- subset(data1, Year < 2013)  
test <- subset(data1, Year > 2012)

MonthlySales <- lm(ElantraSales ~ Unemployment + Months + Queries+ CPI_energy+ CPI_all, data = train)
summary(MonthlySales)

#Which variables are significant, or have levels that are significant? Use 0.10 as your p-value cutoff. (Select all that apply.)
MonthlySales1 <- lm(ElantraSales ~ Unemployment + Months + CPI_energy+ CPI_all, data = train)
summary(MonthlySales1)

#Using the model from train data set (best fit), make predictions on the test set. What is the sum of squared errors of the model on the test set?
fitted_sales <- predict(MonthlySales1, newdata = test)
SSE <- sum((test$ElantraSales - fitted_sales)^2) 
print("Sum of sqaured error of the model on the test set is")
SSE

#What is the test set R-Squared?
SST <- sum((mean(train$ElantraSales) - test$ElantraSales)^2)
R_sq <- 1-(SSE/SST)
print("R-squared of the model on test set is")
R_sq

#What is the largest absolute error that we make in our test set predictions? In which period (Month, Year pair) do we make the largest absolute error in our prediction?
print("The largest error in predicted sales and actual sales is")
max(abs(test$ElantraSales - fitted_sales)) 
max_error <- test$Month[which.max(abs(test$ElantraSales - fitted_sales))]
max_error_year <-test$Year[which.max(abs(test$ElantraSales - fitted_sales))]
print(paste("The period with the largest absolute error is =", max_error,",", max_error_year))