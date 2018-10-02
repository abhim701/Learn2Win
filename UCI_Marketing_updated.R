data <- read.csv("G:/DataScience/practice_datasets/UCI_Bank_Marketing/bank-additional/bank-additional/bank-additional-full.csv",header = T,
                 sep = ';'
)


library(party)
library(dplyr) 
library(sqldf)
library(plotly)
library(rpivotTable)

data$y <- (data$y == "yes")*1

"
tail(data,100)
str(data)

summary(data)

cf1 <- cforest(y ~ . , data= data, control=cforest_unbiased(mtry=2,ntree=50))
varimp_x <- varimp(cf1)  # get variable importance, based on mean decrease in accuracy
sort(varimp_x)

age            job        marital      education        default        housing 
0.000490       0.000214       0.000019       0.000265       0.000489      -0.000066 
loan        contact          month    day_of_week       duration       campaign 
0.000032       0.003288       0.007086       0.000591       0.024272       0.000338 
pdays       previous       poutcome   emp.var.rate cons.price.idx  cons.conf.idx 
0.002634       0.000759       0.002832       0.010391       0.005948       0.004848 
euribor3m    nr.employed 
0.009906       0.011574 


sort(varimp_x)
class(varimp_x)
varimp(cf1, conditional=TRUE)  # conditional=True, adjusts for correlations between predictors
varimpAUC(cf1)    # more robust towards class imbalance.

age            job        marital      education        default        housing 
0.0004441      0.0003055      0.0000024      0.0002072      0.0005256     -0.0000792 
loan        contact          month    day_of_week       duration       campaign 
0.0000133      0.0033252      0.0069958      0.0005229      0.0241906      0.0003717 
pdays       previous       poutcome   emp.var.rate cons.price.idx  cons.conf.idx 
0.0025518      0.0008365      0.0028316      0.0103661      0.0059093      0.0049936 
euribor3m    nr.employed 
0.0099407      0.0114494 



hist(data$age)
sort(unique(data$pdays))

"

temp <- sqldf("select  count(y) as cnt,pdays
               from data 
               group by pdays order by pdays")


rpivotTable(data, rows="education", col="y" , aggregatorName="Count", 
                       vals="mpg")


## Check Missing Values
colnames(data)
sum(is.na(data$nr.employed))
sapply(data, function(x) sum(is.na(x)))
apply(data, 2, function(x) sum(is.na(x)))

##treat missing values
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)



## Outliers
"
Capping Skewed Values
Once if the data is found to be skewed the upper and lower boundary can be capped with
quintiles, standard deviation, etc. according to the business needs.
>m<-mean(X21, na.rm = TRUE) # Mean of the variable X21
>sd<-sd(X21, na.rm = TRUE) #StdDev of the variable X21
>ul<-m+(2*sd) # Upper limit is calculated as Mean+2(StdDev)
>ll<-m-(2*sd) # Lower limit is calculated as Mean-2(StdDev)
# Capping values based on Mean & 2(StdDev)measures
>X21<-as.numeric(
  ifelse(data$X21<=ll,ll,
         ifelse(data$X21>=ul,ul,
                X21)))
>hist(X21)

"

##Binning Values
"
Binning Values
Sometimes it is very difficult to explain the numeric values. Hence binning such variables can
add some business sense and also very easy to explore data.
>summary(X4)
>age<-as.factor(
  ifelse(X4<=250,'Young',
         ifelse(X4<=350,'Mature',
                'Old')))
>summary(age)
> counts <- table(Y,age)
>counts #Table View



# Stacked bar plot

#barplot(counts,
#main=Stacked Bar Plot"
#xlab="Age", ylab="Frequency",
#col=c("yellow","green"),
#legend=rownames(counts))



boxplot(data$age)
summary(data$age)
sd(data$age)

data$Age_bin <- cut(data$age, breaks = c(15,25,35,45,55,65,100),
                    include.lowest = TRUE)
sort(unique(data$Age_bin))

data$Age_bin
hist(data$Age_bin)

Age.freq <- table(data$Age_bin)
Age.freq
class(Age.freq)
barplot(Age.freq)
class(data)

unique(data$pcon )
data$pcon <- (data$pdays != 999)*1
str(data)
data$pcon <- as.factor(data$pcon)
data$y <- as.factor(data$y)
data <- subset(data, select=-c(age,pdays))

##Chisq test of independence
library(MASS)       # load the MASS package 
tbl = table(data$y, data$Age_bin) 
tbl
chisq.test(tbl)

library(InformationValue)

factor_vars <- c ("job", "marital", "education", "default", "housing", "loan", "contact",
                  "month","day_of_week", "poutcome","Age_bin","pcon")


all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars))
                     , stringsAsFactors = F)  # init output dataframe

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=data[, factor_var], Y=data$y)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=data[, factor_var], Y=data$y), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ]  # sort

all_iv

for(factor_var in factor_vars){
  data[[factor_var]] <- WOE(X=data[, factor_var], Y=data$y)
}

unique(WOE(X=data[, "pcon"], Y=data$y))

##removed as suggested by low IV
data <- subset(data, select=-c(marital ,day_of_week,housing, loan))

"

data <- subset(data, select=-c(poutcome))
marital day_of_week ,emp.var.rate

data <- subset(data, select=-c(marital,day_of_week))

"

nr.employed   #removed for high VIF
data <- subset(data, select=-c(nr.employed))

data <- subset(data, select=-c(emp.var.rate))
data <- subset(data, select=-c(poutcome))




library(caret)
intrain<-createDataPartition(y=data$y,p=0.7,list=FALSE)
train<-data[intrain,]
test<-data[-intrain,]
str(test)

#Logistic Regression
model <- glm(y ~ ., family = binomial(link = 'logit'), data = train)
summary(model)


cor(train)
alias( model )


##temp1 <- subset(data, select=c(emp.var.rate,duration,nr.employed))
##cor(temp1)
##sort(unique(data$job))


#we can run the anova() function on the model to analyze the table of deviance

" The difference between the null deviance and the residual deviance shows how our model is doing 
against the null model (a model with only the intercept). The wider this gap, the better. 
Analyzing the table we can see the drop in deviance when adding each variable one at a time. 
Again, adding Pclass, Sex and Age significantly reduces the residual deviance. The other 
variables seem to improve the model less even though SibSp has a low p-value. A large 
p-value here indicates that the model without the variable explains more or less the same amount 
of variation. Ultimately what you would like to see is a significant drop in deviance and the AIC."


anova(model, test = 'Chisq')



##Check Multicolliniarity

library(car)
vif(model)




#Confusion Matrix

train$predict <- predict(model, newdata=train, type='response')
train$predict_r <- ifelse(train$predict > 0.5,1,0)


test$predict <- predict(model, newdata=test, type='response')
test$predict_r <- ifelse(test$predict > 0.5,1,0)

misClasificError_train <- mean(train$predict_r != train$y)
1-misClasificError_train



misClasificError_test <- mean(test$predict_r != test$y)
1-misClasificError_test



table(test$predict_r,test$y)
table(train$predict_r,train$y)


## ROC
##Gini  Coeffients   :   How good it classifies  
nrow(test)
table(test$y)

library(pROC)

auc_train <- auc(train$y, train$predict)
auc_train
Gini_train <- 2*auc_train -1 


auc_test<- auc(test$y, test$predict)
auc_test
Gini_test <- 2*auc_test -1
" 0-30%  poor
  30-40% fair
  40-50% satisfactory
  50+ good "



g <- roc(y ~ predict, data = train)
g <- roc(y ~ predict, data = test)
g

plot(g) 

colnames(data)

y ~ job + education + default + contact + month + duration + 
  campaign + previous + cons.price.idx + cons.conf.idx + euribor3m + 
  Age_bin + pcon

##Final variable list
 colnames(data)
[1] "job"            "marital"        "education"      "default"        "contact"       
[6] "month"          "day_of_week"    "duration"       "campaign"       "previous"      
[11] "cons.price.idx" "cons.conf.idx"  "euribor3m"      "y"              "Age_bin"       
[16] "pcon"
 
library(devtools)
install_github("riv","tomasgreif")
library(woe)
iv.mult(data,"gb",TRUE)
head(data)

#to generate information WOE and info values for columns to use it in the model
#install.packages("InformationValue") 
library(InformationValue)
options(scipen = 999, digits = 2)
WOET<- WOETable(X=data$month, Y=data$y)
class(WOET)
IV(X=data$month, Y=data$y)


class(data)

names(WOET)[names(WOET) == "CAT"] <- "month"
WOET$month <- as.factor(WOET$month)
names(WOET)
data <-subset( inner_join(data,WOET), select = -c(GOODS,BADS,TOTAL,PCT_G,PCT_B,IV) )
names(data)[names(data) == "WOE_age"] <- "WOE"

#####################################


data$campaign <- as.factor(data$campaign)
str(data)

unique(data$campaign)


## Calculate ROC and cutoff probability


library(ROCR)
ROCRpred_train <- prediction( train$predict , train$y)
ROCRpred_test <- prediction( test$predict , test$y)

ROCRperf_train <- performance (ROCRpred_train, 'tpr','fpr')
PRperf_train <- performance(ROCRpred_train, 'prec','rec')
ROCRperf_train
ROCRperf_test <- performance (ROCRpred_test, 'tpr','fpr')
PRperf_test<- performance(ROCRpred_test, 'prec','rec')
ROCRperf_test


class(ROCRperf_train)
class(ROCRperf_test)
class(PRperf_test)
PRperf_test

str(ROCRperf)

plot(ROCRperf_train, colorize = TRUE, text.adj = c(-0.2,1.7))
plot(PRperf_train, colorize = TRUE, text.adj = c(-0.2,1.7))

plot(ROCRperf_train, colorize = TRUE, text.adj = c(-0.2,1.7))
plot(PRperf_test, colorize = TRUE, text.adj = c(-0.2,1.7))



cutoffs_train <- data.frame(cut=ROCRperf_train@alpha.values[[1]], fpr=ROCRperf_train@x.values[[1]], 
                      tpr=ROCRperf_train@y.values[[1]])
cutoffs_test <- data.frame(cut=ROCRperf_test@alpha.values[[1]], fpr=ROCRperf_test@x.values[[1]], 
                            tpr=ROCRperf_test@y.values[[1]])

cutoffs_train_pr <- data.frame(cut=PRperf_train@alpha.values[[1]], prec=PRperf_train@x.values[[1]], 
                           rec=PRperf_train@y.values[[1]])

cutoffs_test_pr <- data.frame(cut=PRperf_test@alpha.values[[1]], prec=PRperf_test@x.values[[1]], 
                               rec=PRperf_test@y.values[[1]])

cutoffs_train[ which.max(cutoffs_train$fpr + 1 - cutoffs_train$tpr),]
cutoffs_test[ which.max(cutoffs_test$fpr + 1 - cutoffs_test$tpr),]


nrow(cutoffs_train)
head(cutoffs)
names(cutoffs)
###best solution
threshold1 <- function(predict, response) {
  perf <- ROCR::performance(ROCR::prediction(predict, response), "sens", "spec")
  df <- data.frame(cut = perf@alpha.values[[1]], sens = perf@x.values[[1]], spec = perf@y.values[[1]])
  df[which.max(2*df$sens + df$spec), "cut"]
}

threshold1(train$predict, train$y)
threshold1(test$predict, test$y)

"best.sum <- which.max(Sens.model.RM@y.values[[1]]+Spec.model.RM@y.values[[1]])
Sens.model.RM@x.values[[1]][best.sum]
data('test')
plotROC(actuals=test$y, predictedScores=test$predict, returnSensitivityMat = TRUE) 



max_sens_cutoff <- optimalCutoff(actuals=test$y, predictedScores = test$predict, optimiseFor='Ones')  # determine cutoff to maximise sensitivity.
print(max_sens_cutoff) 



sensitivity(actuals = test$y, predictedScores = test$predict, threshold=max_sens_cutoff)
specificity(actuals=test$y, predictedScores=test$predict)



specificity(actuals=test$y, predictedScores=test$predict, threshold = 0.35)

precision(actuals=test$y, predictedScores=test$predict)


youdensIndex(actuals=test$y, predictedScores=test$predict)
misClassError(actuals=test$y, predictedScores=test$predict, threshold=0.5)
"

## Concordance and Discordance ratios

Concordance(actuals=train$y, predictedScores=train$predict)
Concordance(actuals=test$y, predictedScores=test$predict)

somersD(actuals=train$y, predictedScores=train$predict)
somersD(actuals=test$y, predictedScores=test$predict)


###KS Stat, table and Plot

ks_stat(actuals=train$y, predictedScores=train$predict)
ks_plot(actuals=train$y, predictedScores=train$predict)

ks_stat(actuals=test$y, predictedScores=test$predict)
ks_plot(actuals=test$y, predictedScores=test$predict)


#KS Table for train sample
head(train)
class(train$predict)
rm(train_ks)
train_ks<-train[rev(order(train$predict)),]
train_ks <- train_ks %>%
  mutate(decile = ntile(predict, 10))
class(train_ks$y)
train_ks$y <- as.numeric(train_ks$y)
train_ks$y <- ifelse(as.numeric(train_ks$y) == 2,1,0)

train_ks_summary <- train_ks %>%
  group_by(decile) %>%
  summarise(count= n(),
            count_1=sum(y))

train_ks_summary<-train_ks_summary[rev(order(train_ks_summary$decile)),]


train_ks_summary <-  train_ks_summary  %>% mutate ( count_0 = count - count_1)
train_ks_summary <-  train_ks_summary  %>% 
                              mutate ( cumsum_t = cumsum(count),
                                      cumsum_1 = cumsum(count_1),
                                      cumsum_0 = cumsum(count_0) )

train_ks_summary <- train_ks_summary %>%
                              mutate (percumsum_t = cumsum_t/max (cumsum_t),
                                      percumsum_1 = cumsum_1/max (cumsum_1),
                                      percumsum_0 = cumsum_0/max (cumsum_0),
                                      KSlist = percumsum_0 - percumsum_1 )

KS_train <- max(abs(train_ks_summary$KSlist))


#KS Table for test sample

head(test)
class(test$predict)
rm(test_ks)
test_ks<-test[rev(order(test$predict)),]
test_ks <- test_ks %>%
  mutate(decile = ntile(predict, 10))
test_ks$y <- ifelse(as.numeric(test_ks$y) == 2,1,0)

test_ks_summary <- test_ks %>%
  group_by(decile) %>%
  summarise(count= n(),
            count_1=sum(y))

test_ks_summary <-  test_ks_summary  %>% mutate ( count_0 = count - count_1)
test_ks_summary <-  test_ks_summary  %>% 
  mutate ( cumsum_t = cumsum(count),
           cumsum_1 = cumsum(count_1),
           cumsum_0 = cumsum(count_0) )

test_ks_summary <- test_ks_summary %>%
  mutate (percumsum_t = cumsum_t/max (cumsum_t),
          percumsum_1 = cumsum_1/max (cumsum_1),
          percumsum_0 = cumsum_0/max (cumsum_0),
          KSlist = percumsum_0 - percumsum_1 )

KS_test <- max(test_ks_summary$KSlist)


  



#optimalCutoff(actuals=test$y, predictedScores=test$predict, optimiseFor = "Both", returnDiagnostics=TRUE)
