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

### Leave one Out Cross Validation
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

## K Fold Cross Validation


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
cv_train_control<-trainControl(method = "cv",number = 10)


## ----warning=FALSE---------------------------------------------------------
logistic_clf1<-train(defaulted_loan~.,
                     data=train_data,
                     method="glm",
                     family="binomial",
                     trControl=cv_train_control
)
summary(logistic_clf1)


## --------------------------------------------------------------------------
predicted_val_log1<-predict(logistic_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_log1,test_data$defaulted_loan)

## --------------------------------------------------------------------------
plot(varImp(logistic_clf1),main="Variable Importance of Logistic Regression Model",ylab="Features",xlab="Importance")


## --------------------------------------------------------------------------
knn_clf1<-train(defaulted_loan~.,data = train_data,
                method="knn",
                trControl=cv_train_control
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
               trControl=cv_train_control
)


## --------------------------------------------------------------------------
summary(nb_clf1)


## --------------------------------------------------------------------------
predicted_val_nb1<-predict(nb_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_nb1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
svm_clf1<-train(defaulted_loan~.,
                data=train_data,
                method="svmLinear",
                trControl=cv_train_control,
)


## --------------------------------------------------------------------------
svm_clf1


## --------------------------------------------------------------------------
predicted_val_svm1<-predict(svm_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_svm1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
dtree_clf1<-train(defaulted_loan~.,
                  data = train_data,
                  method="rpart",
                  parms = list(split = "information"),
                  tuneLength=10,
                  trControl=cv_train_control
)
dtree_clf1


## --------------------------------------------------------------------------
predicted_val_dtree1<-predict(dtree_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_dtree1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
ann_clf1 <- train(defaulted_loan ~ ., data = train_data, 
                  method = "nnet",
                  preProcess = c("center","scale"), 
                  maxit = 250,    # Maximum number of iterations
                  tuneGrid = data.frame(size = 1, decay = 0),
                  # tuneGrid = data.frame(size = 0, decay = 0),skip=TRUE, # Technically, this is log-reg
                  metric = "Accuracy",
                  trControl=cv_train_control)


## --------------------------------------------------------------------------
predicted_val_ann1<-predict(ann_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_ann1,test_data$defaulted_loan)

## Repeated K-Fold Cross Validation
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
rep_cv_train_control<-trainControl(method = "repeatedcv",number = 10,repeats = 3)


## ----warning=FALSE---------------------------------------------------------
logistic_clf1<-train(defaulted_loan~.,
                     data=train_data,
                     method="glm",
                     family="binomial",
                     trControl=rep_cv_train_control
)
summary(logistic_clf1)


## --------------------------------------------------------------------------
predicted_val_log1<-predict(logistic_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_log1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
knn_clf1<-train(defaulted_loan~.,data = train_data,
                method="knn",
                trControl=rep_cv_train_control
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
               trControl=rep_cv_train_control
)


## --------------------------------------------------------------------------
summary(nb_clf1)


## --------------------------------------------------------------------------
predicted_val_nb1<-predict(nb_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_nb1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
svm_clf1<-train(defaulted_loan~.,
                data=train_data,
                method="svmLinear",
                trControl=rep_cv_train_control,
)


## --------------------------------------------------------------------------
svm_clf1


## --------------------------------------------------------------------------
predicted_val_svm1<-predict(svm_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_svm1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
dtree_clf1<-train(defaulted_loan~.,
                  data = train_data,
                  method="rpart",
                  parms = list(split = "information"),
                  tuneLength=10,
                  trControl=rep_cv_train_control
)


## --------------------------------------------------------------------------
dtree_clf1


## --------------------------------------------------------------------------
predicted_val_dtree1<-predict(dtree_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_dtree1,test_data$defaulted_loan)


## --------------------------------------------------------------------------
ann_clf1 <- train(defaulted_loan ~ ., data = train_data, 
                  method = "nnet",
                  preProcess = c("center","scale"), 
                  maxit = 250,    # Maximum number of iterations
                  tuneGrid = data.frame(size = 1, decay = 0),
                  # tuneGrid = data.frame(size = 0, decay = 0),skip=TRUE, # Technically, this is log-reg
                  metric = "Accuracy",
                  trControl=rep_cv_train_control)


## --------------------------------------------------------------------------
predicted_val_ann1<-predict(ann_clf1,newdata = test_data)


## --------------------------------------------------------------------------
confusionMatrix(predicted_val_ann1,test_data$defaulted_loan)

## Bagging, Boosting and Random Forest
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

