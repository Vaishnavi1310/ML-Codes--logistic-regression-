library(readr)
Insurance_Dataset_ <- read_csv("D:/new/Insurance Dataset .csv")
#View(Insurance_Dataset_)
Insurance_Dataset_ <- Insurance_Dataset_[,-c(1:5,7,11,14:19,21,26:27,31:32)]
sum(is.na(Insurance_Dataset_))
summary(Insurance_Dataset_)
library(Hmisc)
Insurance_Dataset_["Description_illness"] <- with(Insurance_Dataset_,impute(Insurance_Dataset_$Description_illness,mode))
Insurance_Dataset_["Mortality_risk"] <- with(Insurance_Dataset_,impute(Insurance_Dataset_$Mortality_risk,mode))
data_factor <- as.data.frame(lapply(Insurance_Dataset_[,-c(12,15)],factor))
str(data_factor)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(Insurance_Dataset_[,c(12,15)],FUN=normalize))


final_data <- data.frame(data_factor,data_norm)
sum(is.na(final_data))

set.seed(3)
final_data_1<- final_data[sample(nrow(final_data)),]
train <- final_data_1[1:as.integer(0.70*nrow(final_data)),]
library(caret)
train <- upSample(train,train$Result)
test <- final_data_1[-c(1:as.integer(0.70*nrow(final_data))),]


# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
formula_nn <- paste("Result",paste(colnames(train[,-c(14,17)]),collapse ="+"),sep="~")
model <- glm(formula_nn,data=train[,-17],family = "binomial")

# Confusion matrix table 
train_prob <- predict(model,train[,-c(14,17)],type="response")
train_predict <- ifelse(train_prob>0.5,"Genuine","Fraudulent")
train_predict <- as.factor(train_predict)
library(caret)
confusionMatrix(train_predict,train$Result)
# We are going to use NULL and Residual Deviance to compare the between different models
test_prob <- predict(model,test[,-c(14,17)],type="response")
test_predict <- ifelse(test_prob>0.5,"Genuine","Fraudulent")
test_predict <- as.factor(test_predict)
confusionMatrix(test_predict,test$Result)


# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
train[,"prob"] <- prob
train[,"pred_values"] <- pred_values
train[,"yes_no"] <- yes_no

View(train[,c(1,78:80)])

table(claimants$ATTORNEY,claimants$pred_values)
# Calculate the below metrics
# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(train_prob,train$Result)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)




