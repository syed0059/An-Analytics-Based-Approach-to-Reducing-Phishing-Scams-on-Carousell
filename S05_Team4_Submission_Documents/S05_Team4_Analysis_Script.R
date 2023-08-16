#This R file project contains data cleaning on the dataset, URL data exploration (EDA), Random Forest model (URL & website), MARS model (URL & website).

library(neuralnet)
library(caTools)
library(NeuralNetTools)
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)
library(earth)


# USING DATASET #3
url.df <- read.csv("S05_Team4_Dataset.csv", stringsAsFactors = T)
summary(url.df)

#Check if there is null or missing values
sum(is.na(url.df))
#there is no NA value in the dataset
sum(url.df == "NA")
#there is no empty or blank cell in the dataset
sum(url.df == "")
#number of rows: 11055 rows
nrow(url.df)
#number of column: 32 columns
ncol(url.df)

#_______________________________________________________________
#                         DATA PREPARATION
#_______________________________________________________________

url.df$Result[url.df$Result == 1] <- 0 # change legitimate to 0
url.df$Result[url.df$Result == -1] <- 1 # change phishing to 1

url.df[] <- lapply(url.df, factor) 

# Only keep these variables related to URL: 
url.df = url.df[c('having_IPhaving_IP_Address', 'URLURL_Length', 'Shortining_Service', 'having_At_Symbol', 'double_slash_redirecting', 'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State', 'Domain_registeration_length', 'port', 'HTTPS_token', 'Result')]

summary(url.df)
ncol(url.df)

#_______________________________________________________________
#                         DATA EXPLORATION (EDA)
#_______________________________________________________________

# Result
ggplot(data = url.df, aes(Result, fill=Result)) + 
  geom_bar() +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy", x = 'Result', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Legitimate','Phishing'))

# Result by having_IPhaving_IP_Address
ggplot(data = url.df, aes(having_IPhaving_IP_Address, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by IP address", x = 'IP Address', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Yes','No'))

# Result by URLURL_Length
ggplot(data = url.df, aes(URLURL_Length, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by URL length", x = 'URL Length', y = 'Number of Cases') + 
  scale_x_discrete(labels =c('>75','>=54 & <=75','<54'))

# Result by Shortining_Service
ggplot(data = url.df, aes(Shortining_Service, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by Shortining_Service", x = 'Shortining_Service', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Yes','No'))

# Result by having_At_Symbol
ggplot(data = url.df, aes(having_At_Symbol, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by @ Symbol", x = '@ Symbol', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Yes','No'))

# Result by double_slash_redirecting
ggplot(data = url.df, aes(double_slash_redirecting, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by Double Slash", x = 'double_slash_redirecting', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Yes',"No"))  

# Result by Prefix_Suffix
ggplot(data = url.df, aes(Prefix_Suffix, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by Prefix_Suffix", x = 'Prefix_Suffix', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Yes','No'))

# Result by having_Sub_Domain
ggplot(data = url.df, aes(having_Sub_Domain, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by having_Sub_Domain", x = 'having_Sub_Domain', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Multi Sub Domains', '1 Sub Domain','0 Sub Domain'))

# Result by SSLfinal_State
ggplot(data = url.df, aes(SSLfinal_State, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by SSLfinal_State", x = 'SSLfinal_State', y = 'Number of Cases') + 
    scale_x_discrete(labels = c('No HTTPS', 'HTTPS but Not Trusted','HTTPS and Trusted')) 

# Result by Domain_registeration_length
ggplot(data = url.df, aes(Domain_registeration_length, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by Domain_registeration_length", x = 'Domain_registeration_length', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('Expring in > 1 Year','Expiring in 1 Year'))

# Result by port
ggplot(data = url.df, aes(port, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by port", x = 'port', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('All Ports Opened','Only Selected Ports Opened'))

# Result by HTTPS_token
ggplot(data = url.df, aes(HTTPS_token, fill=Result)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "#169c11", "1" = "#b33c1b"), labels = c("Legitimate", "Phishing")) +
  labs(title = "URL Legitimacy by HTTPS_token", x = 'HTTPS_token', y = 'Number of Cases') + 
  scale_x_discrete(labels = c('HTTPS in URL','HTTPS not in URL'))

#_______________________________________________________________
#                     BALANCED RANDOM FOREST DATA
#_______________________________________________________________

# Random Forest Train data (balanced data)

# Train-Test 70-30 split
set.seed(1)
train <- sample.split(Y = url.df$Result, SplitRatio = 0.7)
trainset <- subset(url.df, train==T)
testset <- subset(url.df, train==F)

# BALANCING by downsampling the majority class of trainset
majority <- trainset[trainset$Result == 0,]
minority <- trainset[trainset$Result == 1,]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases.
set.seed(1) 
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen <- majority[chosen,]

# Combine two data tables by appending the rows.
trainset <- rbind(majority.chosen, minority)
summary(trainset)

# Develop train model
set.seed(1)
m.RF.2 <- randomForest(Result ~ . , data = trainset, importance = T)
m.RF.2 # OOB error rate of 9.81%
plot(m.RF.2) # stabilised after about 500 trees

set.seed(1)
m.RF.train.yhat <- predict(m.RF.2, trainset)
mean(m.RF.train.yhat == trainset$Result) # Overall accuracy rate of trainset is 90.46%

# Check variable importance
var.impt <- importance(m.RF.2)
var.impt
par(mfrow = c(1,2))
varImpPlot(m.RF.2, type = 1) # Plot out importance of x-variables by MeanDecreaseAccuracy
varImpPlot(m.RF.2, type = 2) # Plot out importance of x-variables by MeanDecreaseGini

# Names of importance variables
imp <- as.data.frame(var.impt)
imp[order(imp$MeanDecreaseAccuracy, decreasing = T),] # Show in order of importance by MeanDecreaseAccuracy
rownames(imp)[which(imp$MeanDecreaseAccuracy > 0)] 

train_table <- table(Trainset.Actual =  trainset$Result, m.RF.train.yhat) 
train_table

# Miclassification error rate of testset
round(prop.table(train_table[1,]), 7) # TNR = 93.35% & FPR = 6.65%
round(prop.table(train_table[2,]), 7) # FNR = 12.42% & TPR = 87.58%
cm.trainset <- confusionMatrix(reference = trainset$Result, m.RF.train.yhat, mode = "everything", positive="1")

cm.trainset # From confusionMatrix(): Overall Accuracy = 90.46%, Sensitivity = 87.58%, Specificity = 93.35%, Precision = 92.94% Recall = 87.58%, F1 = 90.18% 

#_______________________________________________________________
# Random Forest Test data (balanced data)

# Predict testset using the trained model
set.seed(1)
m.RF.yhat <- predict(m.RF.2, newdata = testset)

# Confusion matrix
table <- table(Testset.Actual =  testset$Result, m.RF.yhat) 
table
# 1 means phishing, 0 is legitimate

mean(m.RF.yhat == testset$Result) # Overall accuracy rate of testset is 90.11%

# Miclassification error rate of testset
round(prop.table(table[1,]), 7) # TNR = 93.34% & FPR = 6.66%
round(prop.table(table[2,]), 7) # FNR = 13.96% & TPR = 86.04%
cm.testset <- confusionMatrix(reference = testset$Result, m.RF.yhat, mode = "everything", positive="1")

cm.testset # From confusionMatrix(): Overall Accuracy = 90.11%, Sensitivity = 86.04%, Specificity = 93.34%, Precision = 91.13% Recall = 86.04%, F1 = 88.52% 

#_______________________________________________________________
#              RANDOM FOREST FOR WEBSITE CONTENT
#_______________________________________________________________

#_______________________________________________________________
#                         DATA CLEANING
#_______________________________________________________________
url.df <- read.csv("S05_Team4_Dataset.csv", stringsAsFactors = T)
summary(url.df)

#Check if there is null or missing values
sum(is.na(url.df))
#there is no NA value in the dataset
sum(url.df == "NA")
#there is no empty or blank cell in the dataset
sum(url.df == "")
#number of rows: 11055 rows
nrow(url.df)
#number of column: 32 columns
ncol(url.df)

#_______________________________________________________________
#                       DATA PREPARATION
#_______________________________________________________________

webContentData <- url.df[,!(names(url.df) %in% c('index', 'having_IPhaving_IP_Address', 'URLURL_Length',
                                                 'Shortining_Service', 'having_At_Symbol', 'double_slash_redirecting',
                                                 'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State',
                                                 'Domain_registeration_length', 'HTTPS_token', 'port'))]
webContentData$Result[webContentData$Result == 1] = 0 # change legitimate to 0
webContentData$Result[webContentData$Result == -1] = 1 # change phishing to 1

webContentData[] = lapply(webContentData, factor) 

set.seed(1) 
train = sample.split(Y = webContentData$Result, SplitRatio = 0.7)
trainset = subset(webContentData, train==T)
testset = subset(webContentData, train==F)

# BALANCING by downsampling the majority class of trainset
majority = trainset[trainset$Result == 0,]
minority = trainset[trainset$Result == 1,]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases.
set.seed(1) 
chosen = sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen = majority[chosen,]

# Combine two data tables by appending the rows.
trainset = rbind(majority.chosen, minority)
summary(trainset)

#_______________________________________________________________
#                          MODEL BUILDING
#_______________________________________________________________

# Develop train model
set.seed(1)
m.RF.2 <- randomForest(Result ~ . , data = trainset, importance = T)
m.RF.2 # OOB error rate of 8.57%
plot(m.RF.2) # stabilised after about 500 trees

set.seed(1)
m.RF.train.yhat <- predict(m.RF.2, trainset)
mean(m.RF.train.yhat == trainset$Result) # Overall accuracy rate of trainset is 93.72%

# Check variable importance
var.impt <- importance(m.RF.2)
var.impt
par(mfrow = c(1,2))
varImpPlot(m.RF.2, type = 1) # Plot out importance of x-variables by MeanDecreaseAccuracy
varImpPlot(m.RF.2, type = 2) # Plot out importance of x-variables by MeanDecreaseGini

# Names of importance variables
imp <- as.data.frame(var.impt)
imp[order(imp$MeanDecreaseAccuracy, decreasing = T),] # Show in order of importance by MeanDecreaseAccuracy
rownames(imp)[which(imp$MeanDecreaseAccuracy > 0)] 

train_table <- table(Trainset.Actual =  trainset$Result, m.RF.train.yhat) 
train_table

# Miclassification error rate of testset
round(prop.table(train_table[1,]), 7) # TNR = 96.12% & FPR = 3.88%
round(prop.table(train_table[2,]), 7) # FNR = 8.69% & TPR = 91.31%
cm.trainset <- confusionMatrix(reference = trainset$Result, m.RF.train.yhat, mode = "everything", positive="1")

cm.trainset # From confusionMatrix(): Overall Accuracy = 93.72%, Sensitivity = 91.31%, Specificity = 96.12%, Precision = 95.93% Recall = 91.31%, F1 = 93.56% 

#_______________________________________________________________
# Random Forest Test data (balanced data)

# Predict testset using the trained model
set.seed(1)
m.RF.yhat <- predict(m.RF.2, newdata = testset)

# Confusion matrix
table <- table(Testset.Actual =  testset$Result, m.RF.yhat) 
table
# 1 means phishing, 0 is legitimate

mean(m.RF.yhat == testset$Result) # Overall accuracy rate of testset is 91.80%

# Miclassification error rate of testset
round(prop.table(table[1,]), 7) # TNR = 94.26% & FPR = 5.74%
round(prop.table(table[2,]), 7) # FNR = 11.30% & TPR = 88.70%
cm.testset <- confusionMatrix(reference = testset$Result, m.RF.yhat, mode = "everything", positive="1")

cm.testset # From confusionMatrix(): Overall Accuracy = 91.80%, Sensitivity = 88.70%, Specificity = 94.26%, Precision = 92.48% Recall = 88.70%, F1 = 90.55% 

#_______________________________________________________________
#                               MARS
#_______________________________________________________________

#_______________________________________________________________
#                         DATA CLEANING
#_______________________________________________________________

url.df <- read.csv("S05_Team4_Dataset.csv", stringsAsFactors = T)
summary(url.df)

#Check if there is null or missing values
sum(is.na(url.df))
#there is no NA value in the dataset
sum(url.df == "NA")
#there is no empty or blank cell in the dataset
sum(url.df == "")
#number of rows: 11055 rows
nrow(url.df)
#number of column: 32 columns
ncol(url.df)

#_______________________________________________________________
#                         DATA PREPARATION
#_______________________________________________________________

url.df$Result[url.df$Result == 1] <- 0 # change legitimate to 0
url.df$Result[url.df$Result == -1] <- 1 # change phishing to 1

url.df[] <- lapply(url.df, factor) 

# Only keep these variables related to URL: 
url.df = url.df[c('having_IPhaving_IP_Address', 'URLURL_Length', 'Shortining_Service', 'having_At_Symbol', 'double_slash_redirecting', 'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State', 'Domain_registeration_length', 'port', 'HTTPS_token', 'Result')]

summary(url.df)
ncol(url.df)

set.seed(1)
train = sample.split(Y = url.df$Result, SplitRatio = 0.7)
trainset = subset(url.df, train==T)
testset = subset(url.df, train==F)

# BALANCING by downsampling the majority class of trainset
majority = trainset[trainset$Result == 0,]
minority = trainset[trainset$Result == 1,]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases.
set.seed(1) 
chosen = sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen = majority[chosen,]

# Combine two data tables by appending the rows.
trainset = rbind(majority.chosen, minority)
summary(trainset)

#_______________________________________________________________
#                       MARS MODEL BUILDING
#_______________________________________________________________

set.seed(1)
mars = earth(Result ~ ., data=trainset)

mars.prob = predict(mars, newdata=testset)
mars.prob

threshold = 0.5
mars.yhat = ifelse(mars.prob > threshold, "1", "0")
mars.table = table(Actual = testset$Result, mars.yhat, deparse.level = 2)
result = round(prop.table(mars.table),3)
result
mars.result = confusionMatrix(reference = testset$Result, as.factor(mars.yhat), mode = "everything", positive="1")
mars.result
recall = mars.result$byClass["Recall"]
accuracy = mars.result$overall["Accuracy"]

#_______________________________________________________________
#           MARS MODEL For MALICIOUS WEBSITE CONTENT
#_______________________________________________________________

#_______________________________________________________________
#                         DATA CLEANING
#_______________________________________________________________

url.df <- read.csv("S05_Team4_Dataset.csv", stringsAsFactors = T)
summary(url.df)

#Check if there is null or missing values
sum(is.na(url.df))
#there is no NA value in the dataset
sum(url.df == "NA")
#there is no empty or blank cell in the dataset
sum(url.df == "")
#number of rows: 11055 rows
nrow(url.df)
#number of column: 32 columns
ncol(url.df)

#_______________________________________________________________
#                         DATA PREPARATION
#_______________________________________________________________

webContentData <- url.df[,!(names(url.df) %in% c('index', 'having_IPhaving_IP_Address', 'URLURL_Length',
                                                 'Shortining_Service', 'having_At_Symbol', 'double_slash_redirecting',
                                                 'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State',
                                                 'Domain_registeration_length', 'HTTPS_token', 'port'))]

webContentData$Result[webContentData$Result == 1] = 0 # change legitimate to 0
webContentData$Result[webContentData$Result == -1] = 1 # change phishing to 1

webContentData[] = lapply(webContentData, factor) 

set.seed(1) 
train = sample.split(Y = webContentData$Result, SplitRatio = 0.7)
trainset = subset(webContentData, train==T)
testset = subset(webContentData, train==F)

# BALANCING by downsampling the majority class of trainset
majority = trainset[trainset$Result == 0,]
minority = trainset[trainset$Result == 1,]

# Randomly sample the row numbers to be in trainset. Same sample size as minority cases.
set.seed(1) 
chosen = sample(seq(1:nrow(majority)), size = nrow(minority))

# Subset the original trainset based on randomly chosen row numbers.
majority.chosen = majority[chosen,]

# Combine two data tables by appending the rows.
trainset = rbind(majority.chosen, minority)
summary(trainset)

#_______________________________________________________________
#                       MARS MODEL BUILDING
#_______________________________________________________________

set.seed(1)
mars = earth(Result ~ ., data=trainset)

mars.prob = predict(mars, newdata=testset)
mars.prob

threshold = 0.5
mars.yhat = ifelse(mars.prob > threshold, "1", "0")
mars.table = table(Actual = testset$Result, mars.yhat, deparse.level = 2)
result = round(prop.table(mars.table),3)
result
mars.result = confusionMatrix(reference = testset$Result, as.factor(mars.yhat), mode = "everything", positive="1")
mars.result
recall = mars.result$byClass["Recall"]
accuracy = mars.result$overall["Accuracy"]

#_______________________________________________________________
#              Neural Network Malicious URL Detection
#_______________________________________________________________
df <-read.csv('S05_Team4_Dataset.csv')

#convert result of -1 (malicious) and 1 (benign) to binary 0 and 1
#1 is benign, 0 is malicious 
df$Result <- ifelse(df$Result==1, 1, 0)
unique(df$Result)

str(df)
summary(df)
# no missing data, it would seem that the data is fairly clean already.

#keep only the variables containing url information
df <- df[,(names(df) %in% c('Result', 'having_IPhaving_IP_Address', 'URLURL_Length',
                            'Shortining_Service', 'having_At_Symbol', 'double_slash_redirecting',
                            'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State',
                            'Domain_registeration_length', 'HTTPS_token', 'port'))]


# train-test split, 70-30 ------------------------------------------------------
set.seed(1)
df.train <- sample.split(Y = df$Result, SplitRatio = 0.7)
df.trainset <- subset(df, df.train == T)
df.testset <- subset(df, df.train == F)
dim(df.trainset)
dim(df.testset)

# Developing Model on Balanced Data --------------------------------------------

# check value number of rows with value 0 or 1. 
dim(df.trainset[df.trainset$Result == 1,])
dim(df.trainset[df.trainset$Result == 0,])
#Result = 0 is the majority.

#downsample
majority <- df.trainset[df.trainset$Result == 1,]
minority <- df.trainset[df.trainset$Result == 0,]
dim(majority)
dim(minority)

set.seed(1)
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority),replace = F)
majority.chosen <- majority[chosen,]

df.trainset <- rbind(minority, majority.chosen)
summary(df.trainset) 

df.NN1 <- neuralnet(Result~., data = df.trainset, hidden = 7, err.fct="ce", linear.output=FALSE,stepmax = 1e6)

df.NN1$startweights   # starting weights used
df.NN1$weights        # Final optimised weights

df.NN1$net.result     # predicted outputs. 
df.NN1$result.matrix  # summary.

# View final weights Neural Network diagram
plot(df.NN1)

#Evaluate Model. 
table2 <- data.frame('Neural Network (train)' = 1:5, 
                     'Neural Network (test)' = 1:5)
rownames(table2) <- c('Overall Accuracy', 'False Positive Rate', 'False Negative Rate', 'Precision', 'Recall')

pred.df.NN1.train <- ifelse(unlist(df.NN1$net.result) > 0.5, 1, 0)
matrix.train <- table(trainset.Actual=df.trainset$Result, pred.df.NN1.train)
colnames(matrix.train) <- c("Phishing","Legitimate")
rownames(matrix.train) <- c("Phishing","Legitimate")
cat('Train set Confusion Matrix with neuralnet:')
matrix.train
table2[1,1]<-mean(pred.df.NN1.train == df.trainset$Result)
table2[2,1]<-matrix.train[2,1] / (matrix.train[1,2] + matrix.train[2,2])
table2[3,1]<-matrix.train[2,1] / (matrix.train[2,1] + matrix.train[1,1])
table2[4,1]<-matrix.train[1,1] / (matrix.train[1,1] + matrix.train[2,1])
table2[5,1]<-matrix.train[1,1] / (matrix.train[1,1] + matrix.train[1,2])

pred.df.NN1.test <- ifelse(unlist(predict(df.NN1,df.testset)) > 0.5, 1, 0)
matrix.test <- table(testset.Actual=df.testset$Result, pred.df.NN1.test)
colnames(matrix.test) <- c("Phishing","Legitimate")
rownames(matrix.test) <- c("Phishing","Legitimate")
cat('Test set Confusion Matrix with neuralnet:')
matrix.test
table2[1,2]<-mean(pred.df.NN1.test == df.testset$Result)
table2[2,2]<-matrix.test[2,1] / (matrix.test[1,2] + matrix.test[2,2])
table2[3,2]<-matrix.test[2,1] / (matrix.test[2,1] + matrix.test[1,1])
table2[4,2]<-matrix.test[1,1] / (matrix.test[1,1] + matrix.test[2,1])
table2[5,2]<-matrix.test[1,1] / (matrix.test[1,1] + matrix.test[1,2])

#adjusting the threshold
cutoffs <- c( 0.055, 0.060, 0.065, 0.066, 0.067, 0.068, 0.069, 0.070, 0.075,0.1,0.2,0.3,0.4,0.5,0.6,
              0.7, 0.8, 0.9, 0.95, 0.955, 0.960, 0.965, 0.966, 0.967, 0.968, 0.969, 0.970, 0.975)
count <- length(cutoffs)
table3 <- data.frame('cutoff' = 1:count, 
                     'Accuracy' = 1:count,
                     'FPR' = 1:count,
                     'FNR' = 1:count,
                     'Precision' = 1:count,
                     'Recall' = 1:count)

for (value in 1:length(cutoffs)){
  pred.df.NN1.test <- ifelse(unlist(predict(df.NN1,df.testset)) > cutoffs[value], 1, 0)
  matrix.test <- table(testset.Actual=df.testset$Result, pred.df.NN1.test)
  table3[value,1]<-cutoffs[value]
  table3[value,2]<- mean(pred.df.NN1.test == df.testset$Result) 
  table3[value,3]<-matrix.test[2,1] / (matrix.test[1,2] + matrix.test[2,2])
  table3[value,4]<-matrix.test[2,1] / (matrix.test[2,1] + matrix.test[1,1])
  table3[value,5]<-matrix.test[1,1] / (matrix.test[1,1] + matrix.test[2,1])
  table3[value,6]<-matrix.test[1,1] / (matrix.test[1,1] + matrix.test[1,2])
  
}

# Feature importance using Garson and Olden ------------------------------------

garson(df.NN1)+coord_flip()
olden(df.NN1)+coord_flip()


#_______________________________________________________________
#                 Neural Network Website Content
#_______________________________________________________________
df <-read.csv('S05_Team4_Dataset.csv')

#convert result of -1 (malicious) and 1 (benign) to binary 0 and 1, 1 is malicious, 0 is benign 
df$Result <- ifelse(df$Result==1, 0, 1)
unique(df$Result)

str(df)
summary(df)
# no missing data, it would seem that the data is fairly clean already.

#keep only the variables containing information of the website. (exclude url info)
df <- df[,!(names(df) %in% c('index', 'having_IPhaving_IP_Address', 'URLURL_Length',
                             'Shortining_Service', 'having_At_Symbol', 'double_slash_redirecting',
                             'Prefix_Suffix', 'having_Sub_Domain', 'SSLfinal_State',
                             'Domain_registeration_length', 'HTTPS_token', 'port'))]


# train-test split, 70-30 ------------------------------------------------------
set.seed(1)
df.train <- sample.split(Y = df$Result, SplitRatio = 0.7)
df.trainset <- subset(df, df.train == T)
df.testset <- subset(df, df.train == F)
dim(df.trainset)
dim(df.testset)

# Developing Model on Balanced Data --------------------------------------------

# check value number of rows with value 0 or 1. 
dim(df.trainset[df.trainset$Result == 1,])
dim(df.trainset[df.trainset$Result == 0,])
#Result = 0 is the majority.

#downsample
majority <- df.trainset[df.trainset$Result == 0,]
minority <- df.trainset[df.trainset$Result == 1,]
dim(majority)
dim(minority)

set.seed(1)
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority),replace = F)
majority.chosen <- majority[chosen,]

df.trainset <- rbind(minority, majority.chosen)
summary(df.trainset) 


df.NN2 <- neuralnet(Result~., data = df.trainset, hidden = 7, err.fct="ce", linear.output=FALSE,stepmax = 1e6)

df.NN2$startweights   # starting weights used
df.NN2$weights        # Final optimised weights

df.NN2$net.result     # predicted outputs. 
df.NN2$result.matrix  # summary.

# View final weights Neural Network diagram
plot(df.NN2)

#Evaluate Model. 
table2 <- data.frame('Neural Network (train)' = 1:5, 
                     'Neural Network (test)' = 1:5)
rownames(table2) <- c('Overall Accuracy', 'False Positive Rate', 'False Negative Rate', 'Precision', 'Recall')

pred.df.NN2.train <- ifelse(unlist(df.NN2$net.result) > 0.5, 1, 0)
matrix.train <- table(trainset.Actual=df.trainset$Result, pred.df.NN2.train)
colnames(matrix.train) <- c("Legitimate","Phishing")
rownames(matrix.train) <- c("Legitimate","Phishing")
cat('Train set Confusion Matrix with neuralnet:')
matrix.train
table2[1,1]<-mean(pred.df.NN2.train == df.trainset$Result)
table2[2,1]<-matrix.train[1,2] / (matrix.train[2,1] + matrix.train[1,1])
table2[3,1]<-matrix.train[1,2] / (matrix.train[1,2] + matrix.train[2,2])
table2[4,1]<-matrix.train[2,2] / (matrix.train[2,2] + matrix.train[1,2])
table2[5,1]<-matrix.train[2,2] / (matrix.train[2,2] + matrix.train[2,1])

pred.df.NN2.test <- ifelse(unlist(predict(df.NN2,df.testset)) > 0.5, 1, 0)
matrix.test <- table(testset.Actual=df.testset$Result, pred.df.NN2.test)
colnames(matrix.test) <- c("Legitimate","Phishing")
rownames(matrix.test) <- c("Legitimate","Phishing")
cat('Test set Confusion Matrix with neuralnet:')
matrix.test
table2[1,2]<-mean(pred.df.NN2.test == df.testset$Result)
table2[2,2]<-matrix.test[1,2] / (matrix.test[2,1] + matrix.test[1,1])
table2[3,2]<-matrix.test[1,2] / (matrix.test[1,2] + matrix.test[2,2])
table2[4,2]<-matrix.test[2,2] / (matrix.test[2,2] + matrix.test[1,2])
table2[5,2]<-matrix.test[2,2] / (matrix.test[2,2] + matrix.test[2,1])

# Feature importance using Garson and Olden ------------------------------------
garson(df.NN2)+coord_flip()
olden(df.NN2)+coord_flip()