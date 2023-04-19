---
title: "Assignment 2"
output: html_notebook
---

Data Exploration

1. Download the dataset and store in a dataframe.

```{R}
bank <- read.csv("bank-full.csv", sep = ";", stringsAsFactors = FALSE, na.strings = "unknown")


```


2. Explore overall structure of the dataset using str(). Get a summary of statistics and then explain what each type of variable.

```{R}
str(bank)
summary(bank)

```

age: Continuous

job: Categorical (unordered)

marital: Categorical (unordered)

education: Categorical (ordered)

default: Categorical (unordered)

balance: Continuous

housing: Categorical (unordered)

loan: Categorical (unordered)

contact: Categorical (unordered)

day: Continuous

month: Categorical (ordered)

duration: Continuous

campaign: Continuous

pdays: Continuous

previous: Continuous

poutcome: Categorical (unordered)

y: Categorical (unordered)



3. Get frequency table of the target variable "y". Is y balanced? 

```{R}
bank_y <- factor(bank$y)
table(bank_y)

```
From the frequency table, it seems that y is not balanced.


4. Explore the data in order to investigate the association between the target variable y and other variables in the dataset. 

```{R}
attach(bank)
boxplot(age~y, col ="red")
oneway.test(age~y, data =bank)
```
There appears to be an association with age and y.


```{R}
job_table <- table(bank$job,bank$y)
mosaicplot(job_table, ylab = "job", xlab ="y or n", main ="Mosaic graph of job vs y or n", shade=TRUE)
chisq.test(job_table)

```
There appears to be an association between job and y.


```{R}
marital_table <- table(bank$marital, bank$y)
mosaicplot(marital_table, ylab ="marital status", xlab ="y or n", main ="Mosiac graph of marital status vs y or n", shade=TRUE)
chisq.test(marital_table)


```

There appears to be an association with y and marital.


```{R}
edu_table <- table(bank$education, bank$y)
mosaicplot(edu_table, ylab ="Education", xlab ="y or n", main ="Mosiac graph of education vs y or n", shade=TRUE)
chisq.test(edu_table)


```

There appears to be an association between education and y.


```{R}
default_table <- table(bank$default, bank$y)
mosaicplot(default_table, ylab ="Default", xlab ="y or n", main ="Mosiac graph of default vs y or n", shade=TRUE)
chisq.test(default_table)

```
There appears to be an association with default and y


```{R}
boxplot(balance~y, col ="red")
oneway.test(balance~y, data = bank)

```
There appears to be an association with balance and y.


```{R}
housing_table <- table(bank$housing, bank$y)
mosaicplot(housing_table, ylab ="housing", xlab ="y or n", main ="Mosiac graph of housing vs y or n", shade=TRUE)
chisq.test(housing_table)

```
There appears to be an association with housing and y.


```{R}
loan_table <- table(bank$loan, bank$y)
mosaicplot(loan_table, ylab ="Loan", xlab ="y or n", main ="Mosiac graph of loan vs y or n", shade=TRUE)
chisq.test(loan_table)



```

There appears to be an association with loan and y.


```{R}
contact_table <- table(bank$contact, bank$y)
mosaicplot(contact_table, ylab ="Contact", xlab ="y or n", main ="Mosiac graph of contact vs y or n", shade=TRUE)
chisq.test(contact_table)



```
There appears to be an association with contact and y.


```{R}
boxplot(day~y, col="red")
oneway.test(day~y, data =bank)


```

There appears to be an association with day and y.



```{R}
month_table <- table(bank$month, bank$y)
mosaicplot(month_table, ylab ="Month", xlab ="y or n", main ="Mosiac graph of month vs y or n", shade=TRUE)
chisq.test(month_table)


```
There appears to be an association with month and y.

```{R}
boxplot(duration~y, col="red")
oneway.test(duration~y, data = bank)


```

There appears to be an association with duration and y.



```{R}
boxplot(campaign~y, col="red")
oneway.test(campaign~y)


```
There appears to be an association with campaign and y.


```{R}
boxplot(pdays~y, col="red")
oneway.test(pdays~y, data = bank)


```
There appears to be an association with pdays and y.


```{R}
boxplot(previous~y, col="red")
oneway.test(previous~y, data =bank)


```
There appears to be an asssociation with previous and y.



```{R}
pout_table <- table(bank$poutcome,bank$y)
mosaicplot(pout_table, ylab ="poutcome", xlab ="y or n", main ="Mosiac graph of poutcome vs y or n", shade=TRUE)
chisq.test(pout_table)




```
There appears to be an association with poutcome and y.







5. 

```{R}
colSums(is.na(bank))

```
Job, education, contact, and poutcome have missing values.





6.

```{R}
calc_mode <- function(x){
  unique_val <- na.omit(unique(x))
  unique_tab <- tabulate(match(x, unique_val))
  unique_val[which.max(unique_tab)]
}

bank$job[is.na(bank$job)] <- calc_mode(bank$job)
bank$education[is.na(bank$education)] <- calc_mode(bank$education)
bank$contact[is.na(bank$contact)] <- calc_mode(bank$contact)
bank$poutcome[is.na(bank$poutcome)] <- calc_mode(bank$poutcome)
colSums(is.na(bank))

```


7. Set seed of random number generator to fixed integer

8. Randomize the order of the rows


```{R}
set.seed(1)
shuffle_bank <- bank[sample(nrow(bank), replace =FALSE),]
str(shuffle_bank)

```

9.


```{R}
library(data.table)
bank_dt <- as.data.table(shuffle_bank)
class(bank_dt)
bank_dt$job <- factor(bank_dt$job)
bank_dt$marital <- factor(bank_dt$marital)
bank_dt$default <- factor(bank_dt$default)
bank_dt$housing <- factor(bank_dt$housing)
bank_dt$loan <- factor(bank_dt$loan)
bank_dt$contact <- factor(bank_dt$contact)
bank_dt$poutcome <- factor(bank_dt$poutcome)
bank_one_hot <-one_hot(bank_dt, cols = c("job","marital","default","housing","loan","contact","poutcome"), sparsifyNAs = FALSE, naCols =FALSE, dropCols =TRUE, dropUnusedLevels = TRUE)
bank_one_hot

```
```{R}
#convert data table back to data frame
bank_frame <- as.data.frame(bank_one_hot, row.names =NULL, optional = FALSE)
class(bank_frame)


```

10. Split data to training and test sets with the first 36168 rows for training and the rest for testing.


```{R}
bank_train <- bank_frame[1:36168, ]
bank_test <- bank_frame[36169:45211, ]

#one_hot_frame = subset(bank_one_hot, select= -c(age,balance,day,duration,campaign,pdays,previous))
bank_train
bank_test

```

11. Scale all numeric features with z-score normalization (exclude one-hot encoded variables)

```{R}

bank_train$age = scale(bank_train$age)
bank_train$balance = scale(bank_train$balance)
bank_train$day = scale(bank_train$day)
bank_train$duration = scale(bank_train$duration)
bank_train$campaign = scale(bank_train$campaign)
bank_train$pdays = scale(bank_train$pdays)
bank_train$previous = scale(bank_train$previous)

bank_test$age = scale(bank_test$age)
bank_test$balance = scale(bank_test$balance)
bank_test$day = scale(bank_test$day)
bank_test$duration = scale(bank_test$duration)
bank_test$campaign = scale(bank_test$campaign)
bank_test$pdays = scale(bank_test$pdays)
bank_test$previous = scale(bank_test$previous)


```



12. use 5-fold cross validation with KNN on Training set to predict y variable and report accuracy of cross-validation

```{R}
library(caret)
library(class)
crossValidationError = function(features,target,k){
  folds=createFolds(target,k=5)
  errors=sapply(folds,knn_fold,features=features,target=target,k=k)
  return(mean(errors))
}
knn_fold = function(features,target,fold,k){
train = features[-fold,]
validation = features[fold,]
train_labels = target[-fold]
validation_labels = target[fold]
validation_preds = knn(train,validation,train_labels,k=k)
t = table(validation_labels,validation_preds)
error = (t[1,2]+t[2,1])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
return(error)
}

createFolds(bank_train$y, 5)
#crossValidationError(bank_train,bank_train$y,5)


```





