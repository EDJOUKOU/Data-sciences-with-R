## Import the data from an excle spreadsheet
library(XLConnect)
w <-loadWorkbook(file.choose(), create = TRUE)
Sec <- readWorksheet(w, sheet = 3, startRow = 13, startCol = 2, endRow =16, endCol = 6, header = TRUE )
View(Sec)


### Sectors' primary data
SecDF <- data.frame(Sec)
Industry <- ts(data = SecDF[,2], start = 1990, end = 2016, frequency = 1)
Transport <- ts(data = SecDF[,3], start = 1990, end = 2016, frequency = 1)
Residential <- ts(data = SecDF[,4], start = 1990, end = 2016, frequency = 1)
Services <- ts(data = SecDF[,5], start = 1990, end = 2016, frequency = 1)
Agri <- ts(data = SecDF[,6], start = 1990, end = 2016, frequency = 1)

# Import necessary libraries
library(forecast)
library(nlme)
library(MuMIn)
library(caret)
library(mlr)
library(psych)
library(mlbench)
library(rcompanion)
library(gridExtra)
library(ggplot2)

# Data checking, cleaning and basic exploratory data anaysis
anyNA(SecDF)
describe(SecDF)
summary(SecDF)
str(SecDF)
head(SecDF)


####Regression model built on Residential sector consumption##############################
# The regression is built with two predictors that are population (r1) and GDP per capita (r2)


#Data partition
SecDF <- data.frame(Sec)
SectrainDF3 <- data.frame(r1 = Sectrain[,7], r2= Sectrain[,8], r = Sectrain[,4])
SectrainDF3
SectestDF3 <- data.frame(r1 = Sectest[,7], r2= Sectest[,8], r = Sectest[,4])
SectestDF3
dim(SectrainDF3)
dim(SectestDF3)


#Model training using the caret package 
ControlParameters <- caret::trainControl(method = "cv", number = 3, savePredictions = TRUE,classProbs = TRUE)
OLS.Model.resid <- caret::train(r~r1+r2, data = SectrainDF3, method="lm", trcontrol=ControlParameters, preProcess=c("center", "scale"))
OLS.Model.resid

#forecast computation with OLS (Ordinary Least Square) model along with performance
fR.OLS.resid=predict(OLS.Model.resid, newdata=SectestDF3, interval=c("prediction"), level=0.95)
fR.OLS.resid

accmeasures_Rc= regr.eval(SectestDF3$r, fR.OLS.resid)
accmeasures_Rc
checkresiduals(fR.OLS.resid)

#### Forecast of the Residential sector comsumption till 2030 #############
#Model training using caret package 
ControlParameters <- caret::trainControl(method = "cv", number = 3, savePredictions = TRUE,classProbs = TRUE)
OLS.Model.Resid <- caret::train(Residential~Population + GDP_capita, data = SecDF, method="lm", trcontrol=ControlParameters, preProcess=c("center", "scale"))
OLS.Model.Resid
summary(OLS.Model.Resid)
fR.OLS.Resid=predict(OLS.Model.Resid, newdata = newh, interval=c("prediction"), level=0.95)
fR.OLS.Resid
