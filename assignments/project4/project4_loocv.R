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
loocv_train_control<-trainControl(method = "LOOCV")


## ----warning=FALSE---------------------------------------------------------
logistic_clf1<-train(defaulted_loan~.,
  data=train_data,
  method="glm",
  family="binomial",
  trControl=loocv_train_control
)
summary(logistic_clf1)


## --------------------------------------------------------------------------
predicted_val_log1<-predict(logistic_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_log1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
knn_clf1<-train(defaulted_loan~.,data = train_data,
               method="knn",
                 trControl=loocv_train_control
               )


## --------------------------------------------------------------------------
knn_clf1$result


## --------------------------------------------------------------------------
predicted_val_knn1<-predict(knn_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_knn1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
library(naivebayes)
nb_clf1<-train(defaulted_loan~.,
               data=train_data,
               method="naive_bayes",
               usepoisson = TRUE,
               trControl=loocv_train_control
               )


## --------------------------------------------------------------------------
summary(nb_clf1)


## --------------------------------------------------------------------------
predicted_val_nb1<-predict(nb_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_nb1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
#ctrl <- trainControl(method = "LOOCV", savePred=T)
#svm_clf1<-train(defaulted_loan~.,
#               data=train_data,
#               method="svmLinear",
#               trControl=ctrl,
#               )
#svm_clf


## --------------------------------------------------------------------------
#predicted_val_svm1<-predict(svm_clf1,newdata = test_data)


## --------------------------------------------------------------------------
#confusionMatrix(predicted_val_svm1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
dtree_clf1<-train(defaulted_loan~.,
                 data = train_data,
                 method="rpart",
                 parms = list(split = "information"),
                 tuneLength=10,
                 trControl=loocv_train_control
                 )
dtree_clf1


## --------------------------------------------------------------------------
predicted_val_dtree1<-predict(dtree_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_dtree1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
#ann_clf1 <- train(defaulted_loan ~ ., data = train_data, 
#  method = "nnet",
#  preProcess = c("center","scale"), 
#  maxit = 250,    # Maximum number of iterations
#  tuneGrid = data.frame(size = 1, decay = 0),
# tuneGrid = data.frame(size = 0, decay = 0),skip=TRUE, # Technically, this is log-reg
#  metric = "Accuracy",
#  trControl=loocv_train_control)


## --------------------------------------------------------------------------
#predicted_val_ann1<-predict(ann_clf1,newdata = test_data)


## --------------------------------------------------------------------------
#confusionMatrix(predicted_val_ann1,test_data$defaulted_loan)

