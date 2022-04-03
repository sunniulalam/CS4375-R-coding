#Sunniul Alam
#SXA180118
#CS4375.002
#March 27th, 2022

#Include necessary libraries
library(tidyverse)
library(scatterplot3d)
library(arules)
library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(e1071)
library(caret)
library(parallel)
library(iterators)
library(foreach)
library(doParallel)

#Read in the hepatitis_csv.csv file into the hepatitis object
hepatitis<-read.csv("hepatitis_csv.csv", sep=",", header=TRUE)

#Improving of the Data Set
#Remove all rows that have are missing 3 or more continuous values 
hepatitis <- hepatitis[rowSums(is.na(hepatitis)) <3, ]
#Replace all empty strings with False
hepatitis[hepatitis==""]<-"False"
#Replace any NA in bilirubin with the mean. However, there are no NA after removing NA,
#so the command is only there for good practice.
hepatitis$bilirubin[is.na(hepatitis$bilirubin)]<-mean(hepatitis$bilirubin, na.rm=TRUE)
#Replace any NA in alk_phosphate with the mean
hepatitis$alk_phosphate[is.na(hepatitis$alk_phosphate)]<-mean(hepatitis$alk_phosphate, na.rm=TRUE)
#Replace any NA in sgot with the mean
hepatitis$sgot[is.na(hepatitis$sgot)]<-mean(hepatitis$sgot, na.rm=TRUE)
#Replace any NA in albumin with the mean
hepatitis$albumin[is.na(hepatitis$albumin)]<-mean(hepatitis$albumin, na.rm=TRUE)
#Replace any NA in protime with the mean
hepatitis$protime[is.na(hepatitis$protime)]<-mean(hepatitis$protime, na.rm=TRUE)
#Display Summary of Improved data set
summary(hepatitis)
#1
#Create a decision tree to classify the objects of the dataset
for(i in c(3:13))
  hepatitis[[i]] <-as.factor(hepatitis[[i]])
summary(hepatitis)
tree_default<- rpart(class ~ ., data= hepatitis)
tree_default
rpart.plot(tree_default)

pred<- predict(tree_default, hepatitis, type="class")
pred

confusion_table<- table(hepatitis$class, pred)
confusion_table

true_positive_rate<- confusion_table[1]/(confusion_table[1]+confusion_table[3])
false_positive_rate<- confusion_table[2]/(confusion_table[2]+confusion_table[4])
true_negative_rate<- confusion_table[4]/(confusion_table[4]+confusion_table[2])
false_negative_rate<- confusion_table[3]/(confusion_table[3]+confusion_table[1])

true_positive_rate
false_positive_rate
true_negative_rate
false_negative_rate

correct <- sum(diag(confusion_table))
correct
error <- sum(confusion_table)-correct
error

accuracy <- correct/ (correct-error)
accuracy

error<- error/(correct-error)
error

con_table<- table(head(hepatitis$class,10), head(pred,10))
con_table

correct <- sum(diag(con_table))
correct
error <- sum(con_table)-correct
error

#2
tree <- rpart(class ~., data=hepatitis, control=rpart.control(minsplit=2, cp=0))
rpart.plot(tree, extra = 2, under = TRUE,  varlen=0, faclen=0)

pred<- predict(tree, hepatitis, type="class")
pred

confusion_table<- table(hepatitis$class, pred)
confusion_table

true_positive_rate<- confusion_table[1]/(confusion_table[1]+confusion_table[3])
false_positive_rate<- confusion_table[2]/(confusion_table[2]+confusion_table[4])
true_negative_rate<- confusion_table[4]/(confusion_table[4]+confusion_table[2])
false_negative_rate<- confusion_table[3]/(confusion_table[3]+confusion_table[1])

true_positive_rate
false_positive_rate
true_negative_rate
false_negative_rate

correct <- sum(diag(confusion_table))
correct
error <- sum(confusion_table)-correct
error

accuracy <- correct/ (correct-error)
accuracy
error<- error/(correct-error)
error

#3
tree<- rpart(class ~ ., data= hepatitis)
rpart.plot(tree)

tree<- rpart(class ~., data=hepatitis, control=rpart.control(minsplit=2, cp=0))
rpart.plot(tree, extra = 2, under = TRUE,  varlen=0, faclen=0)

#4
n_train<- as.integer(nrow(hepatitis)*.66)
train_id<- sample(1:nrow(hepatitis), n_train)
train <- hepatitis[train_id, ]
test <- hepatitis[-train_id, colnames(hepatitis) != "class"]
test_type<- hepatitis[-train_id, "class"]
tree <- rpart(class ~., data=train, control=rpart.control(minsplit=2))
tree
rpart.plot(tree, extra=2, under = TRUE, varlen=0, faclen=0)
pred<- predict(tree, train, type="class")
pred

confusion_table<- table(train$class, pred)
confusion_table

train_id <- sample(1:nrow(hepatitis), 15)
train<-hepatitis[train_id, ]
test<-hepatitis[-train_id, ]
tree <- rpart(class ~., data=train, control=rpart.control(minsplit=2))
tree
rpart.plot(tree, extra=2, under = TRUE, varlen=0, faclen=0)
pred<- predict(tree, train, type="class")
pred

confusion_table<- table(train$class, pred)
confusion_table


#5
accuracy <- function(truth, prediction) {
  tbl <-table(truth, prediction)
  sum(diag(tbl)/sum(tbl))
}

shuffled_hepatitis <- 1:nrow(hepatitis)
shuffled_hepatitis <- sample(shuffled_hepatitis)

fold <- rep(1:10, each=nrow(hepatitis)/10)[1:nrow(hepatitis)]
fold
folds <- split(shuffled_hepatitis, fold)
folds
accs <- vector(mode="numeric")
trainError <- vector(mode="numeric")
for(i in 1:length(folds)){
  tree <- rpart(class ~ ., data=hepatitis[-folds[[i]],], control=rpart.control(minsplit = 2))
  accs[i] <- accuracy(hepatitis[folds[[i]],]$class, predict(tree, hepatitis[folds[[i]],], type="class"))
  error[i] <- 1-accs[i]
  rpart.plot(tree, extra=2, under=TRUE, varlen=0, faclen=0)
}
accs
error
avg_error= mean(error)
avg_error