path <- "G:/DataScience/practice_datasets/Adult"

setwd(path)

library(data.table)
library(mlr)
setcol <- c("age", "workclass", "fnlwgt", "education", "education-num", "marital-status", 
            "occupation", "relationship", "race", "sex", "capital-gain", "capital-loss",
            "hours-per-week", "native-country", "target")

train <- read.table("adultdata.txt", header = F, sep = ",", col.names = setcol, na.strings = c(" ?"),
                    stringsAsFactors = F)

test <- read.table("adult_test.txt",header = F,sep = ",",col.names = setcol,skip = 1, na.strings = c(" ?"),stringsAsFactors = F)

train1 <- train
test1 <- test
#train <-train1
#test <-test1

train <- setDT(train)
test <- setDT(test)
class(test)
class(train)
table(is.na(train))
sapply(train, function(x) sum(is.na(x))/length(x))*100
table(is.na(test))
sapply(test, function(x) sum(is.na(x))/length(x))*100


colnames(train)
colnames(test)
library(stringr)
 
test <- test [,target := substr(target,start = 1,stop = nchar(target)-1)]

sapply (test,is.character)
char_col <- colnames(train)[ sapply (test,is.character)]
for(i in char_col) set(train,j=i,value = str_trim(train[[i]],side = "left"))
for(i in char_col) set(test,j=i,value = str_trim(test[[i]],side = "left"))


train[is.na(train)] <- "Missing"
test[is.na(test)] <- "Missing"


str(train)
str(test)


#using one hot encoding

#train <- train1

library(dplyr)
train <- train %>% mutate(target = ifelse(target == "<=50K" , "0", "1"))
test <- test %>% mutate(target = ifelse(target == "<=50K" , "0", "1"))

train$target <- as.numeric(train$target)
test$target <- as.numeric(test$target)

str(train)
str(test)

labels <- train$target
ts_label <- test$target
class(labels)

colnames(train)
train <- train %>% select(-target)
test <- test %>% select(-target)

#new_tr <- model.matrix(~.+0,data = train[,-c("target")])
#new_ts <- model.matrix(~.+0,data = test[,-c("target")])
class(train)
class(test)
str(test)
str(train)
library(dummies)
train <- dummy.data.frame(train, names=c("workclass","education", "race","marital.status","occupation",
                                        "relationship","race", "native.country", "sex"), sep="_")

test <- dummy.data.frame(test, names=c("workclass","education", "race","marital.status","occupation",
                                         "relationship","race", "native.country", "sex"), sep="_")

train_bkp <- train
test_bkp <- test

str(train)

class(labels)
labels <- as.numeric(labels)
ts_label <- as.numeric(ts_label)


class(train)
class(test)

train[] <- lapply(train, function(x) as.numeric(as.character(x)))
test[] <- lapply(test, function(x) as.numeric(as.character(x)))
str(train)


train <- as.matrix(train)
test <- as.matrix(test)
str(train)
head(train)

library(xgboost)
class(train)
#preparing matrix
dtrain <- xgb.DMatrix(data = as.matrix(train),label = labels)
dtest <- xgb.DMatrix(data = test,label=ts_label)


params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
               gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)


xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, 
                 stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F) ##best iteration = 79


min(xgbcv$test.error.mean)
