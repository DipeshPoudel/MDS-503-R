## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------
library(haven)
bank_loan_df <- read_sav("P4_bankloan_5000_clients.sav")


## --------------------------------------------------------------------------
bank_loan_df$defaulted_loan<-as.factor(bank_loan_df$defaulted_loan)
bank_loan_df$education_level<-as.factor(bank_loan_df$education_level)


## --------------------------------------------------------------------------
set.seed(1234)
library(caret)
ind<-sample(2,nrow(bank_loan_df),replace=T,prob = c(0.7,0.3))
train_data<-bank_loan_df[ind==1,]
test_data<-bank_loan_df[ind==2,]


## --------------------------------------------------------------------------
library("ipred")
bag_dtree_clf<-bagging(defaulted_loan~.,
                       data = train_data,
                       coob=T
                       )


## --------------------------------------------------------------------------
print(bag_dtree_clf)


## --------------------------------------------------------------------------
predicted_bag_tree<-predict(bag_dtree_clf,newdata = test_data)


## --------------------------------------------------------------------------
library(caret)
confusionMatrix(predicted_bag_tree,test_data$defaulted_loan)


## --------------------------------------------------------------------------
set.seed(1234)
library(randomForest)
rf_clf<-randomForest(defaulted_loan~.,
                       data = train_data)


## --------------------------------------------------------------------------
rf_clf


## --------------------------------------------------------------------------
predicted_rf<-predict(rf_clf,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_rf,test_data$defaulted_loan)


## --------------------------------------------------------------------------
xglm_clf<-train(defaulted_loan~.,
                data = train_data,
                method="xgbTree",
                verbose=F
                )


## --------------------------------------------------------------------------
xglm_clf


## --------------------------------------------------------------------------
predicted_xgb<-predict(xglm_clf,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_xgb,test_data$defaulted_loan)

## --------------------------------------------------------------------------
summary(xglm_clf)

## --------------------------------------------------------------------------


## --------------------------------------------------------------------------
varImp(xglm_clf)


## --------------------------------------------------------------------------
plot(varImp(xglm_clf))

