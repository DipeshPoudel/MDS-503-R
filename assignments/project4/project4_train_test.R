## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::purl(input, output, documentation = 2)


## ------------------------------------------------------------------------------
library(haven)
bank_loan_df <- read_sav("P4_bankloan_5000_clients.sav")


## ------------------------------------------------------------------------------
bank_loan_df$defaulted_loan<-as.factor(bank_loan_df$defaulted_loan)
bank_loan_df$education_level<-as.factor(bank_loan_df$education_level)


## ------------------------------------------------------------------------------
str(bank_loan_df)


## ------------------------------------------------------------------------------
library(caret)


## ------------------------------------------------------------------------------
set.seed(1234)
ind<-sample(2,nrow(bank_loan_df),replace=T,prob = c(0.7,0.3))
train_data<-bank_loan_df[ind==1,]
test_data<-bank_loan_df[ind==2,]


## ----warning=FALSE-------------------------------------------------------------
logistic_clf<-train(defaulted_loan~.,
  data=train_data,
  method="glm",
  family="binomial"
)
summary(logistic_clf)


## ------------------------------------------------------------------------------
predicted_val_log<-predict(logistic_clf,newdata = test_data)


## ------------------------------------------------------------------------------
confusionMatrix(test_data$defaulted_loan, predicted_val_log)


## ------------------------------------------------------------------------------
knn_clf<-train(defaulted_loan~.,data = train_data,
               method="knn",
               preProcess = c("center", "scale"),
               tuneLength = 10
               )


## ------------------------------------------------------------------------------
knn_clf$result


## ------------------------------------------------------------------------------
predicted_val_knn<-predict(knn_clf,newdata = test_data)
confusionMatrix(test_data$defaulted_loan, predicted_val_knn)


## ------------------------------------------------------------------------------
library(e1071)
nb_clf<-naiveBayes(defaulted_loan~.,data=train_data)


## ------------------------------------------------------------------------------
(nb_clf)


## ------------------------------------------------------------------------------
predicted_val_nb<-predict(nb_clf,newdata = test_data)


## ------------------------------------------------------------------------------
confusionMatrix(predicted_val_nb,test_data$defaulted_loan)


## ------------------------------------------------------------------------------
svm_clf<-train(defaulted_loan~.,
               data=train_data,
               method="svmLinear"
               )
svm_clf


## ------------------------------------------------------------------------------
predicted_val_svm<-predict(svm_clf,newdata = test_data)


## ------------------------------------------------------------------------------
confusionMatrix(predicted_val_svm,test_data$defaulted_loan)


## ------------------------------------------------------------------------------
dtree_clf<-train(defaulted_loan~.,
                 data = train_data,
                 method="rpart",
                 parms = list(split = "information"),
                 tuneLength=10
                 )
dtree_clf


## ------------------------------------------------------------------------------
predicted_val_dtree<-predict(dtree_clf,newdata = test_data)


## ------------------------------------------------------------------------------
confusionMatrix(predicted_val_dtree,test_data$defaulted_loan)


## ------------------------------------------------------------------------------
ann_clf <- train(defaulted_loan ~ ., data = train_data, 
  method = "nnet",
  preProcess = c("center","scale"), 
  maxit = 250,    # Maximum number of iterations
  tuneGrid = data.frame(size = 1, decay = 0),
  # tuneGrid = data.frame(size = 0, decay = 0),skip=TRUE, # Technically, this is log-reg
  metric = "Accuracy")


## ------------------------------------------------------------------------------
predicted_val_ann<-predict(ann_clf,newdata = test_data)


## ------------------------------------------------------------------------------
confusionMatrix(predicted_val_ann,test_data$defaulted_loan)

