####### SVR Model built on Total final Energy consumption #########################
#
# Import and load data on the work directory
EnerdataDF

# Import necessary libraries
library(kernlab)
library(caret)

#data partition for the total final consumption
TFCtrain <- data.frame(x1 = trainSVR[,5], x2= trainSVR[,6], y = trainSVR[,2])
TFCtrain
TFCtest <- data.frame(x1 = testSVR[,5], x2= testSVR[,6], y = testSVR[,2])
TFCtest

### parameters training with caret###################################################
CV_folds <- 3 # number of folds
CV_repeats <- 3 # number of repeats
train_control <- caret::trainControl(method = "repeatedcv", number = CV_folds, repeats = CV_repeats)
param <- expand.grid(C = seq(0.1,100,1), sigma=seq(0.01,10,0.5))

Fc_SVR_tuned_model <- caret::train(y~., 
                                   data = TFCtrain,
                                   method = "svmRadial",
                                   trControl = train_control,
                                   tuneGrid = param)
Fc_SVR_tuned_model

##Prediction  test data##########################################################
pred.Fc.SVR <- predict(Fc_SVR_tuned_model, newdata= TFCtest)
pred.Fc.SVR

## Performance over test set###################################################
accmeasures_Fc_SVR <- regr.eval(TFCtest$y, pred.Fc.SVR)
accmeasures_Fc_SVR 
