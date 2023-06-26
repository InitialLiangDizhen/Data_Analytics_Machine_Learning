rm(list = ls())

library(e1071)
library(rpart)
library(ROCR)
library(tree)
library(randomForest)
library(adabag)
library(caret)

setwd("C:/Users/DavidL/OneDrive/CS/FIT3152/A2")
WAUS <- read.csv("HumidPredict2023D.csv")
L <- as.data.frame(c(1:49))
set.seed(31240291) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 2000 rows


#get the rows that has no NA in all columns
#WAUS_cc <- WAUS[complete.cases(WAUS[, sapply(WAUS, is.numeric)]), ] #is.numeric to identify column that is numeric
#WAUS_cc <- WAUS[complete.cases(WAUS),]
#str(WAUS_cc)

#Q1 



library(dplyr)
#length of more humid
WAUS_01 = aggregate(WAUS[2], WAUS[ncol(WAUS)],length)
WAUS_01
WAUS_proportion = WAUS_01[2,2]/sum(WAUS_01[2])
WAUS_proportion #proportion = 0.4837 WAUS: 0.4716
WAUS_proportion = WAUS_01[1,2]/sum(WAUS_01[2])

#get real_values attributes, no categorical value
col_real = sapply(WAUS, is.numeric)

#just numeric value
WAUS_num_pre = WAUS[col_real]

#automatically ignore NA
summary(WAUS_num_pre)

#omit pressure9am and pressure3pm and RISK_MM, values too big for comparison
WAUS_NoPressure = WAUS_num_pre %>% select(c(-Pressure9am, -Pressure3pm, - RISK_MM))
boxplot(WAUS_NoPressure, las=2, main = "Boxplot For Most Real-valued Attributes")
# Add summary values as labels


WAUS_Press = WAUS_num_pre %>% select(c(Pressure9am, Pressure3pm))
boxplot(WAUS_Press, las=2, main = "Boxplot For Pressure Attributes")

WAUS_Risk = WAUS_num_pre %>% select(RISK_MM)
boxplot(WAUS_Risk, las=2, main = "Boxplot For Risk Attribute")


#Q2
#reomove the columns that have too many NAs which is not worth for analysing
WAUS$Evaporation <- NULL
WAUS$Sunshine <- NULL
WAUS$Cloud9am <- NULL
WAUS$Cloud3pm <- NULL

#omit the rows of data if NAs in Character type column and MHT in hard to be 
#replaced with suitable value, since it is a categorical variable
WAUS_omit <- na.omit(WAUS,cols= 
                       c("Year","Location", "WindGustDir", "WindDir9am"
                         ,"WindDir3pm","RainToday", "MHT"))

#change categorical non-character type attributes into factor
WAUS_omit$Year <- as.factor(WAUS_omit$Year)
WAUS_omit$Location <- as.factor(WAUS_omit$Location)
WAUS_omit$MHT <- as.factor(WAUS_omit$MHT)

#change all categorical character type attributes into factor
col_chr <- sapply(WAUS_omit, is.character)
#for numeric categorical attribute
WAUS_omit[col_chr] <- lapply(WAUS_omit[col_chr], factor)
#factorise MHT since it should be a categorical variable
#WAUS_cc[,ncol(WAUS_cc)] <- as.factor(WAUS_cc[,ncol(WAUS_cc)])
#str(WAUS_cc)

#sapply and lapply are both functions in R that can be used to apply a function to each element of a list or vector
#lapply return a list, sapply return a matrix, if size 1 then vector not list


#AUS_other <- select(WAUS_omit, -one_of(c("Year","Location", "WindGustDir"
                                          #, "WindDir9am","WindDir3pm","RainToday")))
#String variable can not be 0 or mean
#All are categorical variables

#col_num <- sapply(WAUS_omit, is.integer)
#for numeric categorical attribute
#WAUS_omit <-WAUS_omit[complete.cases(WAUS_omit[col_num]),]
#str(WAUS_omit)


#for real-valued attribute column convert NA into mean
#col_num <- sapply(WAUS_omit, is.double) 
#return a vector of booleans that the column is numeric type

#WAUS_omit[col_num] <- lapply(WAUS_omit[col_num], function(x) {x[is.na(x)] <- round(mean(x, na.rm = TRUE),digits=0); x})
#str(WAUS_omit)



#All NA cleared until now

#scaling - for all double variable, to avoid big number that higher impact



#col_num <- col_num[-length(col_num)]
#names(col_num) <- names(col_num)[-length(col_num)]

WAUS_cc <- WAUS_omit
#improve the performance of many machine learning algorithms
col_num = sapply(WAUS_cc, is.numeric) #is.numeric count int and double as numeric as well
WAUS_cc[col_num] = lapply(WAUS_cc[col_num], scale)
#str(WAUS_cc)

#specialised for Artificial neural network
WAUS_nn <- WAUS_cc


#WAUS_cc$MHT <- NULL
#head(WAUS_cc)
#WAUS_cc <- cbind(WAUS_cc, MHT)
#str(WAUS_cc)


#Q3
set.seed(31240291) #Student ID as random seed
train.row = sample(1:nrow(WAUS_cc), 0.7*nrow(WAUS_cc))
WAUS.train = WAUS_cc[train.row,]
WAUS.test = WAUS_cc[-train.row,]



#Q4
#Accuracy function
acc <- function(table) {
  tn = table[1,1]
  fn = table[1,2]
  fp = table[2,1]
  tp = table[2,2]
  return ((tp + tn)/(tn+fn+fp+tp))
}

#sprintf("\n Accuracy:  \n")



#Q5
#Decision Tree
#str(WAUS.train)#use head can not see its actual type
WAUS_tree = tree(MHT~., data=WAUS.train)
#summary(WAUS_tree)
plot(WAUS_tree)
text(WAUS_tree, pretty=0)

#create confusion matrix
WAUS_predict = predict(WAUS_tree, WAUS.test, type = "class")
t1 = table(Predicted_Class = WAUS.test$MHT, Actual_Class = WAUS_predict)
a1 = acc(t1)
cat("\n Decision Tree \n")
print(t1)
#[0,0]: 102, [0,1] = 126, [1,0] = 68, [1,1] = 134
sprintf("Accuracy: %.3f", a1)
#Accuracy = 102 + 134/(102+134+68+126) = 0.549


#Q6
WAUS_pred_vec = predict(WAUS_tree, WAUS.test, type = "vector")
#head(WAUS_pred_vec)
WAUS_pred_comp = prediction(WAUS_pred_vec[,2], WAUS.test$MHT)
WAUS_perf <- performance(WAUS_pred_comp, "tpr", "fpr")
#plot if ROC curve
plot(WAUS_perf, col="red", main = "Decision Tree vs Bagging 
     vs Naive Bayes vs Boosting vs Random Forest", xlab = "False Positive Rate"
     , ylab = "True Positive Rate")
abline(0,1)


#AUC
auc1 <- performance(WAUS_pred_comp, "auc")@y.values[[1]] #AUC of performance instance
auc1 <- round(auc1, 3)
sprintf("AUC Decision Tree: %.3f", auc1)


#Naive Bayes Q5 ~ Q6
#Q5
WAUS_NB = naiveBayes(MHT~., data=WAUS.train)
WAUS_pre.NB = predict(WAUS_NB, WAUS.test)
#create confusion matrix
t2=table(Predicted_Class = WAUS_pre.NB, Actual_Class = WAUS.test$MHT)
cat("\n#NaiveBayes Confusion\n")
print(t2)
#Accuracy
a2 = acc(t2)
sprintf("Accuracy: %.3f", a2)


#trainControl argument to specify cv for cv
#control <- trainControl(method = "none")

# Train the model using caret. otherwise can not get the variable importance
#trainControl to enable cross-validation, otherwith set to None
WAUS.caret <- train(MHT~., data = WAUS.train, method = "naive_bayes")
WAUS_pre.car = predict(WAUS.caret, WAUS.test)
#create confusion matrix
#t2=table(Predicted_Class = WAUS_pre.car, Actual_Class = WAUS.test$MHT)
#cat("\n#NaiveBayes Confusion\n")
#print(t2)
#Accuracy
#a2 = acc(t2)
#sprintf("Accuracy: %.3f", a2)


#Q6
#output as confidence level
#obtain class probabilities in vector
probs.NB <- predict(WAUS_NB, WAUS.test, type = "raw")
#head(probs.NB)
WAUS_NB_pred = prediction(probs.NB[,2],WAUS.test$MHT)
WAUS_NB_perf = performance(WAUS_NB_pred, "tpr", "fpr")
plot(WAUS_NB_perf, add = T, col = "blue")

#@y.values[[1]] part of the code extracts the value of the AUC from the 
#performance object. The @y.values slot of the performance object is a 
#list that contains the calculated performance measures. Since we only calculated 
#one performance measure (the AUC), we can access it using [[1]]
#AUC
#performance(WAUS_NB_pred, "auc") performance instance
#y.values = valueson of measuament at y-axis
auc2 <- performance(WAUS_NB_pred, "auc")@y.values[[1]] #AUC of performance instance
auc2 <- round(auc2, 3)
sprintf("AUC_NB: %.3f", auc2)


#Bagging
#Q5
t3 = WAUS_pre.bag$confusion
a3 = acc(t3)
cat("\n#Bagging Confusion\n")
print(t3)
sprintf("Accuracy: %.3f", a3)

auc3 <- performance(WAUS_Bagpred, "auc")@y.values[[1]] #AUC of performance instance
auc3 <- round(auc3, 3)
sprintf("AUC_Bag: %.3f", auc3)


#Q6
WAUS.bag <- bagging(MHT~., data=WAUS.train, mfinal=5)
WAUS_pre.bag <- predict.bagging(WAUS.bag, WAUS.test)
#use the predicted probability to predict the whether it Y or No
WAUS_Bagpred <- prediction(WAUS_pre.bag$prob[,2], WAUS.test$MHT) #test has the actual 
#comparing like confusion matrix
#see the performance of prediction with varies threshold of classifying as Y
#for the same set of predicted probabilities
WAUS_bag_perf <- performance(WAUS_Bagpred, "tpr", "fpr")
plot(WAUS_bag_perf, add=TRUE, col = "darkgreen")






#Boosting Q5 ~ Q6
#Q5
WAUS.Boost <- boosting(MHT ~. , data = WAUS.train, mfinal=10)
WAUS_pred.boost <- predict.boosting(WAUS.Boost, newdata=WAUS.test)
t4 = WAUS_pred.boost$confusion
a4 = acc(t4)
cat("\n#Boosting Confusion\n")
print(t4)
sprintf("Accuracy: %.3f", a4)


#Q6
WAUS_Boostpred <- prediction(WAUS_pred.boost$prob[,2], WAUS.test$MHT)
WAUS_Boostperf <- performance(WAUS_Boostpred,"tpr","fpr")
plot(WAUS_Boostperf, add=TRUE, col = "yellow")
#legend("bottomright", legend = c("Boosting"), col = "yellow", lty = 1)


#AUC
auc4 <- performance(WAUS_Boostpred, "auc")@y.values[[1]] #AUC of performance instance
auc4 <- round(auc4, 3)
sprintf("AUC_Boost: %.3f", auc4)


#Random Forest Q5 ~ Q6
#Q5
WAUS.rf <- randomForest(MHT ~. , data = WAUS.train, na.action = na.exclude)
WAUSpred.rf <- predict(WAUS.rf, WAUS.test)
t5=table(Predicted_Class = WAUSpred.rf, Actual_Class = WAUS.test$MHT)

#Accuracy
a5 = acc(t5)

cat("\n#Random Forest Confusion")
sprintf("Accuracy: %.3f", a5)


#Q6
#these are all from ROCR package
#output as vector of predicted probabilities
WAUS_Ppred.rf <- predict(WAUS.rf, WAUS.test, type="prob")
WAUS_rf_pred <- prediction(WAUS_Ppred.rf[,2], WAUS.test$MHT)

str(WAUS.test$MHT)
WAUS_rf_perf <- performance(WAUS_rf_pred,"tpr","fpr")
options(digits = 3)

#AUC from 
auc5 <- performance(WAUS_rf_pred, "auc")@y.values[[1]] #AUC of performance instance
auc5 <- round(auc5, 3)
sprintf("AUC_Boost: %.3f", auc5)


plot(WAUS_rf_perf, add=TRUE, col = "purple")
legend("bottomright", legend = c(paste("Decision Tree: ", as.character(auc1))
                                 ,paste("Naive Bayes: ", as.character(auc2))
                                 ,paste("Bagging: ", as.character(auc3))
                                 ,paste("Boosting: ", as.character(auc4))
                                 ,paste("Random Forest: " , as.character(auc5)))
                                 ,col = c("red","blue","darkgreen","yellow","purple"), lty = 1)
#lty specify the line type

#for legend to 
#as.vector(models)
##list of ith cluster (mfr), 
#ck_fit$cluster = as.factor(ck_fit$cluster)
#By using the as.factor function to convert the cluster component to a factor variable, 
#the code ensures that the cluster assignments are treated as categorical data 
#rather than numerical data





#Q7 Table for all models
# Precision
pre <- function(table) {
  tn = table[1,1]
  fn = table[1,2]
  fp = table[2,1]
  tp = table[2,2]
  return(tp / (tp + fp))
}


# Sensitivity (True Positive Rate)
tpr <- function(table) {
  tn = table[1,1]
  fn = table[1,2]
  fp = table[2,1]
  tp = table[2,2]
  return(tp / (tp + fn))
}

# Specificity (True Negative Rate)
tnr <- function(table) {
  tn = table[1,1]
  fn = table[1,2]
  fp = table[2,1]
  tp = table[2,2]
  return (tn / (tn + fp))
}

# False Positive Rate
fpr <- function(table) {
  tn = table[1,1]
  fn = table[1,2]
  fp = table[2,1]
  tp = table[2,2]
  return (fp / (fp + tn))
}



tab <- matrix(c(a1,a2,a3,a4,a5
                ,auc1,auc2,auc3,auc4,auc5
                ,tpr(t1),tpr(t2),tpr(t3),tpr(t4),tpr(t5)
                ,fpr(t1),fpr(t2),fpr(t3),fpr(t4),fpr(t5)
                ,tnr(t1),tnr(t2),tnr(t3),tnr(t4),tnr(t5)
                ,pre(t1),pre(t2),pre(t3),pre(t4),pre(t5)), ncol=6, byrow = FALSE)

colnames(tab) <- c('Accuracy', 'AUC', 'TPR', 'FPR', 'TNR', 'Precision')
rownames(tab) <- c('Decision Tree','Naive Bayes','Bagging','Boosting','Random Forest')
tab <- as.table(tab)
tab




#Q8
#Attribute Importance
cat("\n#Decision Tree Attribute Importance\n")
print(summary(WAUS_tree))
#"Rainfall"   "Location"   "WindDir9am"

#feature has a high conditional probability for one value and a low conditional 
#probability for another value, it means that the feature can help distinguish 
#between instances of the class and instances of other classes. On the other hand, 
#if a feature has similar conditional probabilities for all values, it means that 
#the feature is not very useful for predicting the class
cat("\n#Naive Bayes Attribute Importance\n")
#Determine importance of each variable
varImp(WAUS.caret)


cat("\n#Baging Attribute Importance\n")
print(WAUS.bag$importance)
#WindDir3pm    WindDir9am   WindGustDir
#16.155417     15.704986     18.078031


cat("\n#Boosting Attribute Importance\n")
print(WAUS.Boost$importance)
#WindDir3pm    WindDir9am   WindGustDir  Year
#15.7315826    17.5663088    15.3548518  14.5384410 

cat("\n#Random Forest Attribute Importance\n")
print(WAUS.rf$importance)
#Year                 47.070367
#WindGustDir          56.352709
#WindDir9am           56.980031
#WindDir3pm           54.784604




#Q10
WAUS_tree_imp = tree(MHT ~ .- Location - WindDir9am - WindGustSpeed, data=WAUS.train)
summary(WAUS_tree_imp)
plot(WAUS_tree_imp)
text(WAUS_tree_imp, pretty=0)

#create confusion matrix
WAUS_predict = predict(WAUS_tree_imp, WAUS.test, type = "class")

ptt = table(Predicted_Class = WAUS.test$MHT, Actual_Class = WAUS_predict)

#accuracy
a6 = acc(ptt)
a6 = round(a6, 3)
a6


#neuralnet would conflit with the ROCR package
detach(package:neuralnet)
WAUS_pred_vec = predict(WAUS_tree_imp, WAUS.test, type = "vector")
WAUS_pred_comp = prediction(WAUS_pred_vec[,2], WAUS.test$MHT)
WAUS_perf <- performance(WAUS_pred_comp, "tpr", "fpr")
#plot if ROC curve

#AUC
auc6 <- performance(WAUS_pred_comp, "auc")@y.values[[1]] #AUC of performance instance
auc6 <- round(auc6, 3)
sprintf("AUC_Purned Tree: %.3f", auc6)


tab <- rbind(tab, c(a6,auc6,tpr(ptt),fpr(ptt),tnr(ptt),pre(ptt)))
rownames(tab)[nrow(tab)] <- "Pruned Tree"
tab


#since the random forest create multiple decision tree by subseting the attributes
#which suggest better decision tree possibility
#


#cv tree 
WAUS_cvtree = cv.tree(WAUS_tree_imp, FUN = prune.misclass)
print(WAUS_cvtree)

#prune the tree
WAUS_prune_tree = prune.misclass(WAUS_tree_imp, best = 4)
plot(WAUS_prune_tree)
text(WAUS_prune_tree, pretty = 0)

#create confusion matrix
WAUS_ppredict = predict(WAUS_prune_tree, WAUS.test, type = "class")
pt = table(predicted_values = WAUS_ppredict, actual_value = WAUS.test$MHT)
print(pt)
acc(pt)

cat("\n#Pruned Decision Tree Attribute Importance\n")
print(summary(WAUS_tree_imp))
#"Rainfall"    "WindGustDir" "Temp9am" 







#Q11
library(neuralnet)

options(digit=3)


WAUS.nn.data <- WAUS_nn
str(WAUS.nn.data)
#WAUS.nn.data = cbind(WAUS.nn.data, MHT)
#WAUS.nn.data$MHT <- NULL
WAUS.nn.data$Location <- NULL
WAUS.nn.data$Year <- NULL


col_num = sapply(WAUS.nn.data, is.numeric)
WAUS.nn.num <- WAUS.nn.data[,col_num]
str(WAUS.nn.num)

#one-hot encoding to convert the Location column into multiple binary columns, 
#one for each level of the factor. This can be done using the model.matrix function in R
WAUS_ptmm = model.matrix(~WindGustDir + WindDir9am + WindDir3pm, data=WAUS.nn.data)


WAUS.com = cbind(WAUS_ptmm, WAUS.nn.num)
str(WAUS.com)

# Remove columns by names
#WAUS.com <- WAUS.com %>% select(-c("WindGustDir","WindDir9am","WindDir3pm"))
WAUS.com$MHT <- WAUS.nn.data$MHT


#sample train and test data
set.seed(31240291)
nn_train.row = sample(1:nrow(WAUS.com), 0.8*nrow(WAUS.com))
#nn_train.row <- createDataPartition(WAUS.com, p = .8, list = FALSE)
#works the same

WAUS.nn.train = WAUS.com[nn_train.row,]
WAUS.nn.test = WAUS.com[-nn_train.row,]
MHT.nn = WAUS.nn.test$MHT
WAUS.nn.test.pre = WAUS.nn.test[,1:ncol(WAUS.nn.test)-1]

#input variables must be explicit since for the input neuron of ANN
#Location - WindDir9am - WindGustSpeed can be discarded
WAUS.nn = neuralnet(MHT ~ WindGustDirENE+WindGustDirESE+WindGustDirN+WindGustDirNE
                    +WindGustDirNNE+WindGustDirNNW+WindGustDirNW+WindGustDirS+WindGustDirSE
                    +WindGustDirSSE+WindGustDirSSW+WindGustDirSW+WindGustDirW+WindGustDirWNW
                    +WindGustDirWSW+WindDir9amENE+WindDir9amESE+WindDir9amN+WindDir9amNE
                    +WindDir9amNNE+WindDir9amNNW+WindDir9amNW+WindDir9amS+WindDir9amSE
                    +WindDir9amSSE+WindDir9amSSW+WindDir9amSW+WindDir9amW+WindDir9amWNW
                    +WindDir9amWSW+WindDir3pmENE+WindDir3pmESE+WindDir3pmN+WindDir3pmNE
                    +WindDir3pmNNE+WindDir3pmNNW+WindDir3pmNW+WindDir3pmS
                    +WindDir3pmSE+WindDir3pmSSE+WindDir3pmSSW+WindDir3pmSW
                    +WindDir3pmW+WindDir3pmWNW+WindDir3pmWSW+MinTemp+MaxTemp
                    +Rainfall+WindSpeed9am+WindSpeed3pm+Pressure9am
                    +Pressure3pm+Temp9am+Temp3pm+RISK_MM,WAUS.nn.train,hidden=3,linear.output=FALSE)


#predict with the model
#summary(WAUS.nn)

#[,-MHT] to ignore response variable since it is not one of the output variable 
#in output neuron of  ANN model 
WAUS.nn.comp = compute(WAUS.nn, WAUS.nn.test)
ann.tfpr = WAUS.nn.comp$net.result[,2]


#round to integer if < 0.5 -> = 0
ann.predr = round(ann.tfpr, 0)


t7 = table(Predicted_Class = ann.predr, Actual_Class = WAUS.nn.test$MHT)
print(t7)

#Accuracy
a7 = acc(t7)
a7 = round(a7,3)
sprintf("Accuracy: %.3f", a7)


#Q6
#conflict between ROCR and neuralnet package
detach(package:neuralnet)
# Calculate the predicted values
WAUS_pred.ann = predict(WAUS.nn, newdata = WAUS.nn.test)

WAUS.nn.test = as.data.frame(WAUS.nn.test)

WAUS_value = as.data.frame(WAUS.nn.comp$net.result)

WAUS_ANNpred = prediction(WAUS_value$V1, WAUS.nn.test$MHT)
WAUS_ANNperf = performance(WAUS_ANNpred,"tpr","fpr")

# Plot the ROC curve
plot(WAUS_ANNperf, add=TRUE, col = "orange", main = "ROC Curve for ANN Model"
     , xlab = "False Positive Rate", ylab = "True Positive Rate")
abline(0,1)

#AUC
auc7 <- performance(WAUS_ANNpred, "auc")@y.values[[1]] #AUC of performance instance
auc7 <- round(auc7, 3)
sprintf("AUC_ANN: %.3f", auc7)

#Q7
tab <- rbind(tab, c(a7,auc7,tpr(t7),fpr(t7),tnr(t7),pre(t7)))
rownames(tab)[nrow(tab)] <- "ANN"
tab


#Q12
#The cforest function in the party package uses conditional inference trees to 
#create random forests. Conditional inference trees use statistical tests to 
#determine the best split at each node of the tree. At each node, the algorithm 
#tests the null hypothesis that the response variable is independent of each 
#predictor variable, given the values of the other predictor variables. 
#The predictor variable with the lowest p-value (i.e., the strongest association 
#with the response variable) is selected for splitting. The algorithm then uses 
#a statistical test to determine the best split point for the selected predictor 
#variable. This process is repeated recursively for each child node until a stopping criterion is met.

#This approach differs from traditional decision tree algorithms, which use 
#measures such as information gain or Gini impurity to determine splits. By 
#using statistical tests to determine splits, conditional inference trees avoid 
#bias towards factor variables with many levels/categories and provide more 
#robust results when multifactorial variables are involved.


#Q5 ~ Q6
#install.packages("party")
library(party)
?cforest

#OOB argument as TRUE to indicate that you want to make OOB predictions and 
#the factor argument as T to indicate that the response variable is a factor.
#OOB predictions are a way to estimate the performance of a random forest model 
#on the training data. For each observation in the training data, an OOB 
#prediction is made using only the trees in the forest that were not trained on that observation. This provides an unbiased estimate of the model¡¯s performance on new data.
#If you don¡¯t encounter any errors, the WAUSpred.cf object should contain the 
#OOB predictions for the WAUS.train dataset.

#always use ? to see the document
WAUS_cf <- cforest(MHT ~ .,data = WAUS.train)

#can not ignore newdata
WAUS.cf.pred <- predict(WAUS_cf, newdata = WAUS.test)
length(WAUS.cf.pred)
str(WAUS.cf.pred)

t8 = table(predicted = WAUS.cf.pred, actual = WAUS.test$MHT)

a8 = acc(t8)
a8 <- round(a8, 3)
sprintf("CForest Accuracy: %.3f", a8)



#Q6
WAUS.cf.conf <- predict(WAUS_cf, newdata = WAUS.test, type = "prob")

#dictionary data type
avector <- vector(mode = "numeric", length = 0)
#loop through the dictionary data type
for (name in names(WAUS.cf.conf)) {
  prob_1 <- WAUS.cf.conf[[name]][2]
  avector <- c(avector, prob_1)
}

WAUS_cf_pred <- prediction(avector, WAUS.test$MHT)

WAUS_cf_perf <- performance(WAUS_cf_pred,"tpr","fpr")
plot(WAUS_cf_perf)
abline(0,1)

#AUC from 
auc8 <- performance(WAUS_cf_pred, "auc")@y.values[[1]] #AUC of performance instance
auc8 <- round(auc8, 3)
sprintf("CForest AUC: %.3f", auc8)

#add into table
tab <- rbind(tab, c(a8,auc8,tpr(t8),fpr(t8),tnr(t8),pre(t8)))
rownames(tab)[nrow(tab)] <- "CForest"
tab













