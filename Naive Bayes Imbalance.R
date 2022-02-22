data<-read.csv("D:/KMMI Bisnis Analitik/Data KMMI.csv", header = TRUE, sep = ",")
data$BMI[is.na(data$BMI)]<-median(data$BMI,na.rm=TRUE)
# Libraries
library(ggplot2)
library(dplyr)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
data%>%
  ggplot( aes(x=Stroke, fill=Stroke)) + 
  geom_bar()+
  scale_fill_manual(values = c("#CB7A09","#0D8295")) +
  theme(legend.position="none")
prop.table(table(data$Stroke))
#Create Data Partition
set.seed(1234)
index <- createDataPartition(data$Stroke, p=0.80, list=FALSE)
# select 80% of the data for Training
train <- data[index,]
dim(train)
# use the remaining 80% of data to testing the models
test <- data[-index,]
dim(test)
library(caTools)
library(e1071)

# Predicting on test data'
y_pred2 <- predict(classifier_cl, newdata = train)
cm <- table(train$Stroke, y_pred2)
cm
# Model Evaluation
confusionMatrix(cm)
table(train$Stroke)
# Over Sampling
over <- ovun.sample(Stroke~., data = train, method = "over", N = 408)$data
table(over$Stroke)
#Pemodelan dengan Over Sampling
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Stroke ~ ., data = over)
confusionMatrix(predict(classifier_cl, test), test$Stroke)
under <- ovun.sample(Stroke~., data=train, method = "under", N = 72)$data
table(under$Stroke)
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Stroke ~ ., data = under)
confusionMatrix(predict(classifier_cl, test), test$Stroke)
#bOth
both <- ovun.sample(Stroke~., data=train, method = "both",
                    p = 0.5,
                    seed = 222,
                    N = 240)$data
table(both$Stroke)
classifier_cl <- naiveBayes(Stroke ~ ., data = both)
confusionMatrix(predict(classifier_cl, test), test$Stroke)
#ROSE Function
rose <- ROSE(Stroke~., data = train, N = 500, seed=111)$data
table(rose$Stroke)
classifier_r <- naiveBayes(Stroke ~ ., data = rose)
confusionMatrix(predict(classifier_r, test), test$Stroke)
#SMOTE
library(performanceEstimation)
smote<-smote(Stroke~., train, perc.over = 2, k = 5, perc.under = 2)
table(smote$Stroke)
classifier_s <- naiveBayes(Stroke ~ ., data = smote)
confusionMatrix(predict(classifier_r, test), test$Stroke)
#Evaluation Metrics
library(cvAUC)
set.seed(120)  # Setting Seed
classifier_cl <- naiveBayes(Stroke ~ ., data = train)
classifier_cl
# Predicting on training data'
y_pred <- predict(classifier_cl, newdata = test)
# Confusion Matrix
cm <- table(test$Stroke, y_pred)
cm
# Model Evaluation
confusionMatrix(cm,"F1")
#AUC

auc <- AUC(as.numeric(y_pred),as.numeric(test$Stroke) )
auc
#Receiver Operating Characterictic (ROC)
roc.curve(test$Stroke,y_pred)
#ROC
library(ROCit)
ROCit_obj <- rocit(score=as.numeric(y_pred),class=as.numeric(test$Stroke))
plot(ROCit_obj)

#precision
prec<-precision(cm)
rec<-recall(cm)

library(precrec)
precrec_obj <- evalmod(scores = as.numeric(y_pred), labels = as.numeric(test$Stroke))
autoplot(precrec_obj)

precrec_obj2 <- evalmod(scores = as.numeric(y_pred), labels = as.numeric(test$Stroke), mode="basic")
autoplot(precrec_obj2)   

cm <- confusionMatrix( y_pred,test$Stroke,mode="prec_recall")

#Cross Validation
# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(as.factor(Stroke)~., data=train, trControl=train_control, method="nb")
# Summarise Results
print(model)

