rm(list=ls(all=T))
setwd("C:/Users/admin/Documents/R files")
# Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','dplyr','class')

# install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Load the data
# also replacing empty strings if present with NA
train = read.csv("Train_data.csv")
test = read.csv("Test_data.csv")

# comibining train and test so that it will be easier to preprocess it together than separate
train=rbind(train,test)
rmExcept('train')
# Structure of data
str(train)
head(train,10)
# Univariate Analysis 
train$area.code = as.factor(train$area.code)
# Since, we have to predict the churn score based on usage pattern
# so features which are not related to usage pattern will be removed from training data
# like "area code", "phone number"ans'state'are not important so we will remove them, we will remove them after chi-square reduction
str(train)
# Missing Value Analysis
any(is.na(train))                                               # It returns a FALSE value. So, there's no missing value.
# Summary gives all Stats and Also Count of NA values after Max.
summary(train)                                                  # Also after calling Summary we can see that there are no NA's

##### Visualization #########
ggplot(train, aes(Churn, account.length)) +                   # we can see that account.length is not statistically significant 
  geom_boxplot()                                              # in predicting Churn, thus we can remove it.

# churning of customer w.r.t international.plan
intlplanVSChurn <- train %>%
  select(international.plan,Churn) %>%
  group_by(international.plan) 
ggplot(intlplanVSChurn, aes(fill=Churn, x=international.plan)) +
  geom_bar() + ggtitle("international.plan Vs Churn")

# churning of customer w.r.t voice.mail.plan
vmpVSChurn <- train %>%
  select(voice.mail.plan,Churn) %>%
  group_by(voice.mail.plan) 
vmpVSChurn
ggplot(vmpVSChurn, aes(fill = Churn, x = voice.mail.plan)) +
  geom_bar()+ggtitle('voice.mail.plan Vs Churn')
  
# churning of customer w.r.t area.code
acVSChurn <- train %>%
  select(area.code,Churn) %>%
  group_by(area.code) 
acVSChurn
ggplot(acVSChurn, aes(fill = Churn, x = area.code)) +
  geom_bar()+ggtitle('area.code Vs Churn')

# churning of customer w.r.t number.customer.service.calls
cscVSChurn <- train %>%
  select(number.customer.service.calls,Churn) %>%
  group_by(number.customer.service.calls) 
cscVSChurn
ggplot(cscVSChurn, aes(fill = Churn, x = number.customer.service.calls)) +
  geom_bar()+ggtitle('number.customer.service.calls Vs Churn')

#################### converting our factor variables into numeric factor levels ######################
for(i in c(1:21)){
  
  if(class(train[,i]) == 'factor'){
    
    train[,i] = factor(train[,i], labels=(1:length(levels(factor(train[,i])))))
    
  }
}

# let's separate continuous data from categorical data for feature selection
numeric_index = vapply(train,is.numeric,logical(1))             # selecting only numeric(vapply is safer than sapply)
numeric_data = train[,numeric_index]                            
num_col = names(numeric_data)                                   # names() return colnames in data.frame
print(length(num_col))
################# Outlier detection using Boxplot #################
#outlier analysis works only on numerical variables
for (i in 1:length(num_col)){
  assign(paste0("gn",i),
         ggplot(aes_string(y = (num_col[i]), x = 'Churn'),data = train) +
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "skyblue",
                        outlier.shape=18,outlier.size=3, notch=FALSE) +
           labs(y=num_col[i],x="Churn"))
}
# Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)
gridExtra::grid.arrange(gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15,ncol=3)

# There are outliers present in the data. But if we understand and think about the business model here, we get that the dataset is related to customer usage pattern. 
# therefore we come to conclusion that to leave the dataset with outliers to get the most out of the dataset and predict our target variable ‘Churn’.

####### Feature Selection ######
# Let's look at the correlation plot for only continuous data
corrgram(train[,num_col], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
# So we can see that in correlation plot dark blue indicates that 2 variables are highly correlated with each other
# here, total.day.minutes is highly correlated with total.day.charge
#       total.eve.minutes is highly correlated with total.eve.charge
#       total.night.minutes is highly correlated with total.night.charge
#       total.intl.minutes is highly correlated with total.intl.charge
# now instead of selecting all variables for our modelling, we would select one of each of them 
# lets select all charges features-total.day.charge, total.eve.charge, total.night.charge, total.intl.charge
# and drop all minutes features-total.day.minutes, total.eve.minutes, total.night.minutes, total.intl.minutes
# we will drop them after chi-sa=quare analysis
# Now Chi-square test of independence for Categorical data
# As chi-square is only for categorical variables so we will select only categorical variables from training data
cat_index = vapply(train,is.factor,logical(1))  # selecting only numeric(vapply is safer than sapply)
cat_data = train[,cat_index]                            
cat_col = names(cat_data)                       # names() return colnames in data.frame
cat_col
# Applying Chi-square test of independence for Categorical data
for (i in 1:5){
  print(names(cat_data)[i])
  print(chisq.test(table(cat_data$Churn,cat_data[,i])))
}
# If p-value is less than 0.05 then we reject the null hypothesis saying that 2 variables are dependent and if
# p-value is greater than 0.05 then we accept the null hypothesis saying that 2 variables are independent. 
# If p-value<0.05 then select the variable, 
# If p-value>0.05 then ignore the variable as this independent variable doesn't carry any information to explain the dependent variable therefore it will not add any information while modelling.  
# after analyzing p value of all categorical features, we come to a conclusion that, feature- phone.number,area.code will be removed
# and account.length
# Dimensionality Reduction
train = subset(train,
               select = -c(state,account.length,area.code,phone.number,total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))
str(train)

# Again select only numeric and only categorical variables separately from our new data
numeric_index = vapply(train,is.numeric,logical(1))             # selecting only numeric(vapply is safer than sapply)
numeric_data = train[,numeric_index]                            
num_col = names(numeric_data) 
cat_index = vapply(train,is.factor,logical(1))                  # selecting only numeric(vapply is safer than sapply)
cat_data = train[,cat_index]                            
cat_col = names(cat_data)
###### Feature Scaling ######
# It is performed only on Continuous Variables

library(car)
dev.off()
par(mfrow=c(1,2))

qqPlot(train$number.vmail.messages)                             # qqPlot, it has a x values derived from gaussian distribution, if data is distributed normally then the sorted data points should lie very close to the solid reference line 
truehist(train$number.vmail.messages)                           # truehist() scales the counts to give an estimate of the probability density.
lines(density(train$number.vmail.messages))  # left skewed      # lines() and density() functions to overlay a density plot on histogram

qqPlot(train$total.day.calls)
truehist(train$total.day.calls)              
lines(density(train$total.day.calls))        # normal

qqPlot(train$total.day.charge)
truehist(train$total.day.charge)
lines(density(train$total.day.charge))       # normal

qqPlot(train$total.eve.calls)
truehist(train$total.eve.calls)
lines(density(train$total.eve.calls))        # normal

qqPlot(train$total.eve.charge)
truehist(train$total.eve.charge)
lines(density(train$total.eve.charge))       # normal

qqPlot(train$total.night.calls)
truehist(train$total.night.calls)
lines(density(train$total.night.calls))      # normal

qqPlot(train$total.night.charge)
truehist(train$total.night.charge)
lines(density(train$total.night.charge))     # normal

qqPlot(train$total.intl.charge)
truehist(train$total.intl.charge)
lines(density(train$total.intl.charge))      # almost normal   

qqPlot(train$total.intl.calls)
truehist(train$total.intl.calls)
lines(density(train$total.intl.calls))       # left skewed

qqPlot(train$number.customer.service.calls)
truehist(train$number.customer.service.calls)
lines(density(train$number.customer.service.calls))# exibits multimodal distribution
# After plotting all the graphs we can say that most of the variables are normally distributed
# therefore, we will go for standardization over normalization as our most of the data is distributed normally
# Standardization
for(i in num_col){
  print(i)
  train[,i] = (train[,i] - mean(train[,i]))/sd(train[,i])
}
summary(train) # we can see now our data is standarized
# Z is negative when the raw score is below the mean and Z is positive when above mean.

############## A check for Target class imbalance problem for categorical Target Variable-Churn ##########

imbalance = data.frame(table(train$Churn))                      # False : 4293 & True : 707,
colnames(imbalance) = c('Churn_categories','Churn_count')       # i.e. Ratio=4293:707, event rate=(707/5000)*100=14.14%
ggplot(imbalance, aes(x = Churn_categories, y = Churn_count))+
  geom_col()+ggtitle('Churn Distribution Graph')

# As sampling is only applied on training dataset, so lets divide our whole data into train and test
dim(train)
df =train
#train = df
set.seed(2019)
train.index = createDataPartition(train$Churn, p = .75, list = FALSE)
train = train[ train.index,]
test  = train[-train.index,]
dim(train)
dim(test)
prop.table(table(train$Churn)) # 1=FALSE,2=TRUE,FALSE = 85.84% & TRUE = 14.15%

#1. Random Over Sampling
library(ROSE)
table(train$Churn)
sampling_result=ovun.sample(formula = Churn~. , data=train, method = "over" , N= 3220/(1-0.3333),seed=2019)
# selecting only data from large ovun sample
train_over = sampling_result$data
table(train_over$Churn)
prop.table(table(train_over$Churn))

#2. Random under Sampling
table(train$Churn)
sampling_result=ovun.sample(formula = Churn~. , data=train, method = "under" , N= 483/0.333,seed=2019)
# selecting only data from large ovun sample
train_under = sampling_result$data
table(train_under$Churn)
prop.table(table(train_under$Churn))


# 3. Combining under and over sampling
# Defining desired no. of new cases in balanced dataset and fraction of TRUE cases
n_new = 4000
true_fraction = 0.34
sampling_result=ovun.sample(formula = Churn~. , data=train, method = "both" , N= n_new,p = true_fraction,seed=2019)
# selecting only data from large ovun sample
train_both = sampling_result$data
table(train_both$Churn)
prop.table(table(train_both$Churn))

# 4. SMOTE-can only be applied on numeric variables since it uses the euclidean distance to determine nearest neighbors.
library(smotefamily)
table(train$Churn)
# Setting the number of TRUE and FALSE cases, and the desired percentage of FALSE cases
true_cases = 531
false_cases = 3220
desired_precent = 0.6
# Calculate the value for the 'dup_size' parameter of SMOTE
# ntimes is the desired value such that the over-sampled dataset contains 60% FALSE cases
ntimes <- ((1 - desired_precent) / desired_precent) * (false_cases / true_cases) - 1
# Creating synthetic TRUE cases with SMOTE
names(train)
str(train)
smote_output = SMOTE(X =train[ , -c(1, 2, 13)], target = train$Churn, K = 5, dup_size = ntimes)
train_smote <- smote_output$data
str(train_smote)
names(train_smote)[11]<- "Churn" 
train_smote$Churn = as.factor(train_smote$Churn)
table(train_smote$Churn)
prop.table(table(train_smote$Churn))                                   # now True cases is 0.397% and False is 0.602% 

# Model development #
#function for calculating error metrics
error_metric = function(cm){
  TN = cm[1,1]
  FP = cm[1,2]
  FN = cm[2,1]
  TP = cm[2,2]
  print(paste0('Accuracy- ',((TN+TP)/(TN+TP+FN+FP))*100))
  print(paste0('FNR- ',((FN)/(TP+FN))*100))
  print(paste0('Sensitivity/Recall/TPR-  ',((TP)/(TP+FP))*100))
}

######### C5.0 #######

##Decision tree for classification

#Develop Model on training data of sampled train
C50_model = C5.0(Churn ~., train, trials = 100, rules = TRUE)
# Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-13], type = "class")
##Evaluate the performance of classification model
ConfMatrix_C50 = table(actual = test$Churn,predictions = C50_Predictions)
# Because our problem statement in more focused to Churn reduction, we are interested in knowing the customers who are going too churn out
# so our positive class is -  True cases and true case is labelled as 2
# so adding a parameter positive to define our positive class and assigning it to 2.
confusionMatrix(ConfMatrix_C50,positive = "2") 
error_metric(ConfMatrix_C50)
# sensitivity = 1, accuracy = 0.9945, FNR =0.037
# according to problem statement we want to correctly identify people who will Churn out i.e Sensitivity/Recall/TPR
# though we can see here accuracy and fnr is good, we know that our model is trained on unbalanced dataset,
# and here accuracy is misleading because our model is predicting the majority class.
# as classifier tend to favour majority class.
# Therefore, we will use AUC and ROC which is better performance metric.
# Area under ROC curve
roc.curve(test$Churn,C50_Predictions)
#Area under the curve for sampled train: 0.9811

#Develop Model on training data of under sampled -train_under
C50_model = C5.0(Churn ~., train_under, trials = 100, rules = TRUE)
# Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-13], type = "class")
# Evaluate the performance of classification model
ConfMatrix_C50 = table(predictions = C50_Predictions, actual = test$Churn)
confusionMatrix(ConfMatrix_C50,positive = "2") 
error_metric(ConfMatrix_C50)
# sensitivity = 1, accuracy = 0.9727, FNR = 0.1592
roc.curve(test$Churn,C50_Predictions)
#Area under the curve for train_under: 0.9841

#Develop Model on training data of over sampled -train_over
C50_model = C5.0(Churn ~., train_over, trials = 100, rules = TRUE)
# Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-13], type = "class")
# Evaluate the performance of classification model
ConfMatrix_C50 = table(predictions = C50_Predictions, actual = test$Churn)
confusionMatrix(ConfMatrix_C50,positive = "2") 
error_metric(ConfMatrix_C50)
# sensitivity = 0.9924, accuracy = 0.9989, FNR =0
roc.curve(test$Churn,C50_Predictions)
#Area under the curve for train_over: 0.9962

#Develop Model on training data of both-over and under sampled-train_both
C50_model = C5.0(Churn ~., train_both, trials = 100, rules = TRUE)
# Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-13], type = "class")
# Evaluate the performance of classification model
ConfMatrix_C50 = table(predictions = C50_Predictions, actual = test$Churn)
confusionMatrix(ConfMatrix_C50,positive = "2")
error_metric(ConfMatrix_C50)
# sensitivity = 0.9924, accuracy = 0.9912, FNR =0.050
roc.curve(test$Churn,C50_Predictions)
#Area under the curve for train_both: 0.992

#Develop Model on training data of over sampled synthetic- train_smote
C50_model = C5.0(Churn ~., train_smote, trials = 100, rules = TRUE)
# Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-13], type = "class")
# Evaluate the performance of classification model
ConfMatrix_C50 = table(predictions = C50_Predictions, actual = test$Churn)
confusionMatrix(ConfMatrix_C50,positive = "2") 
error_metric(ConfMatrix_C50)
# sensitivity = 1, accuracy = 1, FNR =0
roc.curve(test$Churn,C50_Predictions)
#Area under the curve for train_smote: 1

#Logistic Regression on sampled train-train
logit_model = glm(Churn ~ ., data = train, family = "binomial")
#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
# Confusion matrix
ConfMatrix_LR = table(test$Churn,logit_Predictions>0.5)# can adjust p to correctly classify TRUE cases
ConfMatrix_LR
error_metric(ConfMatrix_LR)
# accuracy = 0.8593, sensitivity =  0.5272, FNR =0.7803
roc.curve(test$Churn,logit_Predictions>0.5)
#Area under the curve for train: 0.593

#Logistic Regression on under sampled train-train_under
logit_model = glm(Churn ~ ., data = train_under, family = "binomial")
#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
# Confusion matrix
ConfMatrix_LR = table(test$Churn,logit_Predictions>0.5)# can adjust p to correctly classify TRUE cases
ConfMatrix_LR
error_metric(ConfMatrix_LR)
# accuracy = 0.7938, sensitivity =  0.3674, FNR =0.4015
roc.curve(test$Churn,logit_Predictions>0.5)
#Area under the curve for train_under: 0.713

#Logistic Regression on over sampled train-train_over
logit_model = glm(Churn ~ ., data = train_over, family = "binomial")
#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
# Confusion matrix
ConfMatrix_LR = table(test$Churn,logit_Predictions>0.5)# can adjust p to correctly classify TRUE cases
ConfMatrix_LR
error_metric(ConfMatrix_LR)
# accuracy = 0.8233, sensitivity =  0.4147, FNR = 0.4469
roc.curve(test$Churn,logit_Predictions>0.5)
#Area under the curve for train_over: 0.711

#Logistic Regression on both over and under sampled train-train_both
logit_model = glm(Churn ~ ., data = train_both, family = "binomial")
#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
# Confusion matrix
ConfMatrix_LR = table(test$Churn,logit_Predictions>0.5)# can adjust p to correctly classify TRUE cases
ConfMatrix_LR
error_metric(ConfMatrix_LR)
# accuracy = 0.8233, sensitivity =  0.4147, FNR = 0.4469
roc.curve(test$Churn,logit_Predictions>0.5)
#Area under the curve for train_both: 0.711

#Logistic Regression on over sampled synthetically train-train_smote
logit_model = glm(as.factor(Churn) ~ ., data = train_smote, family = "binomial")
#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
# Confusion matrix
ConfMatrix_LR = table(test$Churn,logit_Predictions>0.5)# can adjust p to correctly classify TRUE cases
ConfMatrix_LR
error_metric(ConfMatrix_LR)
# accuracy = 0.7808, sensitivity =  0.3580, FNR = 0.3409
roc.curve(test$Churn,logit_Predictions>0.5)
#Area under the curve for train_smote: 0.730

# Random Forest on sampled train-train
RF_model = randomForest( Churn~ ., train, importance = TRUE, ntree = 500)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-13])
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF,positive = '2')
error_metric(ConfMatrix_RF)
# accuracy = 1, sensitivity =  1,, FNR =0
roc.curve(test$Churn,RF_Predictions)
#Area under the curve for train_:1 

# Random Forest on under sampled train-train_under
RF_model = randomForest( Churn~ ., train_under, importance = TRUE, ntree = 500)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-13])
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF,positive = '2')
error_metric(ConfMatrix_RF)
# accuracy = 0.976, sensitivity =  0.8571, FNR =0
roc.curve(test$Churn,RF_Predictions)
#Area under the curve for train_under:0.986


# Random Forest on over sampled train-train_over
RF_model = randomForest( Churn~ ., train_over, importance = TRUE, ntree = 500)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-13])
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF,positive = '2')
error_metric(ConfMatrix_RF)
# accuracy = 0.9989, sensitivity =  1, FNR =0.0075
roc.curve(test$Churn,RF_Predictions)
#Area under the curve for train_over:0.996


# Random Forest on both over and under sampled train-train_both
RF_model = randomForest( Churn~ ., train_both, importance = TRUE, ntree = 500)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-13])
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF,positive = '2')
error_metric(ConfMatrix_RF)
# accuracy = 0.9890, sensitivity =  0.9357, FNR =0.0075
roc.curve(test$Churn,RF_Predictions)
#Area under the curve for train_both:0.990

# Random Forest on over sampled synthetically train-train_smote
RF_model = randomForest( Churn~ ., train_smote, importance = TRUE, ntree = 500)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-13])
##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF,positive = '2')
error_metric(ConfMatrix_RF)
# accuracy = 1, sensitivity =  1, FNR =0
roc.curve(test$Churn,RF_Predictions)
#Area under the curve for train_smote:1

# KNN on sampled train-train
KNN_Predictions = knn(train[-13], test[-13], train$Churn, k = 7)
#Confusion matrix
ConfMatrix_KNN = table(test$Churn,KNN_Predictions)
confusionMatrix(ConfMatrix_KNN,positive = '2')
error_metric(ConfMatrix_KNN)
# accuracy = 0.9116, sensitivity =  0.9473,FNR = 0.5909
roc.curve(test$Churn,KNN_Predictions)
#Area under the curve for train_smote:0.703

# KNN on under sampled train-train_under
KNN_Predictions = knn(train_under[-13], test[-13], train_under$Churn, k = 7)
#Confusion matrix
ConfMatrix_KNN = table(test$Churn,KNN_Predictions)
confusionMatrix(ConfMatrix_KNN,positive = '2')
error_metric(ConfMatrix_KNN)
# accuracy = 0.9193, sensitivity =  0.6883,FNR = 0.1969
roc.curve(test$Churn,KNN_Predictions)
#Area under the curve for train_under:0.871

# KNN on over sampled train-train_over
KNN_Predictions = knn(train_over[-13], test[-13], train_over$Churn, k = 7)
#Confusion matrix
ConfMatrix_KNN = table(test$Churn,KNN_Predictions)
confusionMatrix(ConfMatrix_KNN,positive = '2')
error_metric(ConfMatrix_KNN)
# accuracy = 0.8865, sensitivity =  0.5752,FNR =0.1893
roc.curve(test$Churn,KNN_Predictions)
#Area under the curve for train_over:0.855

# KNN on both over and under sampled train-train_both
KNN_Predictions = knn(train_both[-13], test[-13], train_both$Churn, k = 7)
#Confusion matrix
ConfMatrix_KNN = table(test$Churn,KNN_Predictions)
confusionMatrix(ConfMatrix_KNN,positive = '2')
error_metric(ConfMatrix_KNN)
# accuracy = 0.8942, sensitivity =  0.6047,FNR =0.2348
roc.curve(test$Churn,KNN_Predictions)
#Area under the curve for train_both:0.843

# KNN on over sampled synthetically train-train_smote
KNN_Predictions = knn(train_smote[-11], test[,-c(1,2,13)], train_smote$Churn, k = 7)
#Confusion matrix
ConfMatrix_KNN = table(test$Churn,KNN_Predictions)
confusionMatrix(ConfMatrix_KNN,positive = '2')
error_metric(ConfMatrix_KNN)
# accuracy = 0.8964, sensitivity =  0.5829,FNR =0.015
roc.curve(test$Churn,KNN_Predictions)
#Area under the curve for train_smote:0.933

# Naive Bayes on sampled train-train
NB_model = naiveBayes(Churn ~ ., data = train)
#predict on test cases 
NB_Predictions = predict(NB_model, test[-13], type = 'class')
#Confusion matrix
ConfMatrix_NB = table(test$Churn,NB_Predictions)
confusionMatrix(ConfMatrix_NB,positive = '2')
error_metric(ConfMatrix_NB)
# accuracy = 0.8724, sensitivity =  0.6086,FNR =0.6818
roc.curve(test$Churn,NB_Predictions)
#Area under the curve for train:0.642

# Naive Bayes on under sampled train-train_under
NB_model = naiveBayes(Churn ~ ., data = train_under)
#predict on test cases #raw
NB_Predictions = predict(NB_model, test[-13], type = 'class')
#Confusion matrix
ConfMatrix_NB = table(test$Churn,NB_Predictions)
confusionMatrix(ConfMatrix_NB,positive = '2')
error_metric(ConfMatrix_NB)
# accuracy = 0.8538, sensitivity =  0.4950,FNR =0.2348
roc.curve(test$Churn,NB_Predictions)
#Area under the curve for train_under:0.817

# Naive Bayes on over sampled train-train_over
NB_model = naiveBayes(Churn ~ ., data = train_over)
#predict on test cases #raw
NB_Predictions = predict(NB_model, test[-13], type = 'class')
#Confusion matrix
ConfMatrix_NB = table(test$Churn,NB_Predictions)
confusionMatrix(ConfMatrix_NB,positive = '2')
error_metric(ConfMatrix_NB)
# accuracy = 0.8756, sensitivity =  0.5505,FNR =0.2575
roc.curve(test$Churn,NB_Predictions)
#Area under the curve for train_over:0.820

# Naive Bayes on both over and under sampled train-train_both
NB_model = naiveBayes(Churn ~ ., data = train_both)
#predict on test cases 
NB_Predictions = predict(NB_model, test[-13], type = 'class')
#Confusion matrix
ConfMatrix_NB = table(test$Churn,NB_Predictions)
confusionMatrix(ConfMatrix_NB,positive = '2')
error_metric(ConfMatrix_NB)
# accuracy = 0.8680, sensitivity =  0.5284,FNR =0.2272
roc.curve(test$Churn,NB_Predictions)
#Area under the curve for train_both:0.828

# Naive Bayes on over sampled synthetically train-train_smote
NB_model = naiveBayes(Churn ~ ., data = train_smote)
#predict on test cases 
NB_Predictions = predict(NB_model,test[,-c(1,2,13)], type = 'class')
#Confusion matrix
ConfMatrix_NB = table(test$Churn,NB_Predictions)
confusionMatrix(test$Churn,NB_Predictions,positive = '2')
error_metric(ConfMatrix_NB)
# accuracy = 0.8200, sensitivity =  0.4273,FNR =0.2651
roc.curve(test$Churn,NB_Predictions)
#Area under the curve for train_smote:0.785
