#load the data
bank <- read.table("bank.csv",header = T,sep=";")

#how many subscribe?
prop.table(table(bank$y))
#how many has loan?
prop.table(table(bank$loan))

# create minified data using only bank client data
bank_client_data <- bank[,1:8]

# check summary
lapply(bank_client_data,summary)

#create dummy variables
#for job
bank_client_data <- cbind(bank_client_data,model.matrix(~job-1,data=bank_client_data))
bank_client_data <- bank_client_data[,-ncol(bank_client_data)]
#for marital
bank_client_data <- cbind(bank_client_data,model.matrix(~marital-1,data=bank_client_data))
bank_client_data <- bank_client_data[,-ncol(bank_client_data)]
#for education
bank_client_data <- cbind(bank_client_data,model.matrix(~education-1,data=bank_client_data))
bank_client_data <- bank_client_data[,-ncol(bank_client_data)]
#for default
bank_client_data <- cbind(bank_client_data,model.matrix(~default-1,data=bank_client_data))
bank_client_data <- bank_client_data[,-ncol(bank_client_data)]
#for housing
bank_client_data <- cbind(bank_client_data,model.matrix(~housing-1,data=bank_client_data))
bank_client_data <- bank_client_data[,-ncol(bank_client_data)]
#for loan
bank_client_data <- cbind(bank_client_data,model.matrix(~loan-1,data=bank_client_data))
bank_client_data <- bank_client_data[,-ncol(bank_client_data)]

#append the class
bank_client_data <- cbind(bank_client_data,bank$y)

#split to training and test set
set.seed(1234)
train.split <- sample(1:nrow(bank_client_data),ceiling(0.75*nrow(bank_client_data)))

# set up training set
bank_client_data.train <- bank_client_data[train.split,]
#get the class for training set
bank_client_data.train.class <- bank_client_data.train[,ncol(bank_client_data.train)]
bank_client_data.train <- bank_client_data.train[,c(1,6,9:27)]
# normalize the training set
bank_client_data.train <- lapply(bank_client_data.train,scale)
bank_client_data.train <- as.data.frame(bank_client_data.train)
# set up test set
bank_client_data.test <- bank_client_data[-train.split,]
#get the class for training set
bank_client_data.test.class <- bank_client_data.test[,ncol(bank_client_data.test)]
bank_client_data.test <- bank_client_data.test[,c(1,6,9:27)]
# normalize the test set
bank_client_data.test <- lapply(bank_client_data.test,scale)
bank_client_data.test <- as.data.frame(bank_client_data.test)

#train the model
library("class")
#prediction using knn, k = 3
bank_client_data.pred <- knn(train = bank_client_data.train, test=bank_client_data.test, 
                             cl = bank_client_data.train.class,k = 3)

# test several k's; max k @ sqrt(# of observation)
for (k in 3:13) {
    bank_client_data.pred <- knn(train = bank_client_data.train, test=bank_client_data.test, 
                                 cl = bank_client_data.train.class,k = k, prob=T)
    result <- table(bank_client_data.test.class,bank_client_data.pred)
    accuracy <- (result[1] + result[4])/ sum(result)
    tpr <- result[4]/(result[2]+result[4]) #TP/TP+FN
    specificity <- result[1]/(result[1]+result[3]) #TN/TN+FP
    fpr <- 1 - specificity
    print(paste(k, " accuracy ", accuracy, " tpr ", tpr, " fpr ", fpr, sep=""))
}

#use SMOTE to balance the classes for training
library("unbalanced")
data <- ubSMOTE(X = bank_client_data.train,Y=factor(bank_client_data.train.class,labels=c(0,1)),perc.over = 700,perc.under=100)
new_data <- cbind(data$X,data$Y)

# try several k's again using the balanced training data 
for (k in 3:sqrt(nrow(new_data))) {
    bank_client_data.pred <- knn(train = new_data[,-22], test=bank_client_data.test, 
                                 cl = new_data[,22],k = k, prob=T)
    result <- table(bank_client_data.test.class,bank_client_data.pred)
    accuracy <- (result[1] + result[4])/ sum(result)
    tpr <- result[4]/(result[2]+result[4]) #TP/TP+FN
    specificity <- result[1]/(result[1]+result[3]) #TN/TN+FP
    fpr <- 1 - specificity
    print(paste(k, " accuracy ", accuracy, " tpr ", tpr, " fpr ", fpr, sep=""))
}

# involve new variables
# examine contact
table(bank$y,bank$contact)
tapply(as.data.frame(table(bank$y,bank$contact))$Freq,as.data.frame(table(bank$y,bank$contact))$Var2,function(x) x[2]/sum(x))
barplot(tapply(as.data.frame(table(bank$y,bank$contact))$Freq,as.data.frame(table(bank$y,bank$contact))$Var2,function(x) x[2]/sum(x)))

# examine month
table(bank$y,bank$month)
tapply(as.data.frame(table(bank$y,bank$month))$Freq,as.data.frame(table(bank$y,bank$month))$Var2,function(x) x[2]/sum(x))
barplot(tapply(as.data.frame(table(bank$y,bank$month))$Freq,
               as.data.frame(table(bank$y,bank$month))$Var2,function(x) x[2]/sum(x)))

# examine day
table(bank$y,bank$day)
tapply(as.data.frame(table(bank$y,bank$day))$Freq,as.data.frame(table(bank$y,bank$day))$Var2,function(x) x[2]/sum(x))
barplot(tapply(as.data.frame(table(bank$y,bank$day))$Freq,as.data.frame(table(bank$y,bank$day))$Var2,function(x) x[2]/sum(x)))

#examine pdays
hist(bank$pdays)
#create a new dummy variable for contacted; 0=never
bank$contacted <- ifelse(bank$pdays<0,0,1)
table(bank$y,bank$contacted)

#examine previous
hist(bank$previous)

#examine poutcome
table(bank$poutcome)
barplot(table(bank$poutcome))

#add variables to bank: contact, month, day, pdays, previous, poutcome
bank_cl_cmp_data <- bank[,c(1:16,18,17)]
#fix pdays variable: if pdays < 0, 0
bank_cl_cmp_data$pdays <- ifelse(bank_cl_cmp_data$pdays<0, 999, bank_cl_cmp_data$pdays)

#create dummy variables
#for job
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~job-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for marital
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~marital-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for education
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~education-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for default
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~default-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for housing
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~housing-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for loan
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~loan-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for contact
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~contact-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for month
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~month-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]
#for poutcome
bank_cl_cmp_data <- cbind(bank_cl_cmp_data,model.matrix(~poutcome-1,data=bank_cl_cmp_data))
bank_cl_cmp_data <- bank_cl_cmp_data[,-ncol(bank_cl_cmp_data)]

#tidy up data set
bank_cl_cmp_data <- bank_cl_cmp_data[,c(1,6,19:53,18)]

# split training set
bank_cl_cmp_data.train <- bank_cl_cmp_data[train.split,]
# split training set class
bank_cl_cmp_data.train.class <- bank_cl_cmp_data.train$y
# split test set
bank_cl_cmp_data.test <- bank_cl_cmp_data[-train.split,]
# split training set class
bank_cl_cmp_data.test.class <- bank_cl_cmp_data.test$y

#normalize training set
bank_cl_cmp_data.train <- 
    as.data.frame(lapply(bank_cl_cmp_data.train[,-ncol(bank_cl_cmp_data.train)], scale))
#normalize test set
bank_cl_cmp_data.test <- 
    as.data.frame(lapply(bank_cl_cmp_data.test[,-ncol(bank_cl_cmp_data.test)], scale))

#use SMOTE to balance the classes for training on new data set
library("unbalanced")
cl_cmp_data <- ubSMOTE(X = bank_cl_cmp_data.train,Y=factor(bank_cl_cmp_data.train.class,labels=c(0,1)),perc.over = 800,perc.under=100)
new_cl_cmp_data <- cbind(cl_cmp_data$X,cl_cmp_data$Y)

# try several k's again using the new balanced training data set
for (k in 1:sqrt(nrow(new_cl_cmp_data))) {
    new_cl_cmp_data.pred <- knn(train = new_cl_cmp_data[,-ncol(new_cl_cmp_data)], test=bank_cl_cmp_data.test, 
                                 cl = new_cl_cmp_data[,ncol(new_cl_cmp_data)],k = k, prob=T)
    result <- table(bank_cl_cmp_data.test.class,new_cl_cmp_data.pred)
    accuracy <- (result[1] + result[4])/ sum(result)
    tpr <- result[4]/(result[2]+result[4]) #TP/TP+FN
    specificity <- result[1]/(result[1]+result[3]) #TN/TN+FP
    fpr <- 1 - specificity
    print(paste(k, " accuracy ", accuracy, " tpr ", tpr, " fpr ", fpr, sep=""))
}

# choose k = 62
knn.pred <- knn(train = new_cl_cmp_data[,-ncol(new_cl_cmp_data)], test=bank_cl_cmp_data.test, 
    cl = new_cl_cmp_data[,ncol(new_cl_cmp_data)],k = 62, prob=T)
pred <- prediction(attributes(knn.pred)$prob,ifelse(as.character(bank_cl_cmp_data.test.class)=="no",0,1))
perf <- performance(pred,"tpr","fpr")
plot(perf, col="red")
abline(0,1, lty=2)

####################

#try with decision tree
library(C50)

#reorder bank data: y as last column
# remove duration from the data set to reduce bias
bank_tidy <- bank[,c(1:11,13:16,18,17)]

#split training data set
bank.train.c50 <- bank_tidy[train.split,]

#split test data set
bank.test.c50 <- bank_tidy[-train.split,]

#decision tree with 100 boosting iteration
bank.model <- C5.0(x = bank.train.c50[,1:16],y = bank.train.c50[,17], trials=1000)
bank.pred.c50 <- predict(bank.model, bank.test.c50,type = "prob")
pred.tree <- prediction(bank.pred.c50[,2],factor(bank.test.c50$y,labels=c(0,1)))
perf.tree <- performance(pred.tree,"tpr","fpr")
plot(perf.tree,add=T, col="blue")

######################

#try random forest
library(randomForest)

bank.model.rf <- randomForest(y~.,data=bank.train.c50,importance = T)
bank.rf.pred <- predict(bank.model.rf, bank.test.c50,type="prob")
pred.rf <- prediction(bank.rf.pred[,2],factor(bank.test.c50$y,labels=c(0,1)))
perf.rf <- performance(pred.rf, "tpr", "fpr")
plot(perf.rf, add=T, col="green")

#add title to the plot
title("Prediction on term deposit subscription")
#add legend
legend("bottomright", c("knn", "boosted decision tree", "random forest"), lwd = 1,
       col=c("red", "blue", "green"))

#try balance the data set
bank.train.rf.bal <- ubSMOTE(bank.train.c50[,-17],factor(bank.train.c50$y,labels=c(0,1)),perc.over = 800, perc.under=100)
bank.train.rf.bal <- cbind(bank.train.rf.bal$X, bank.train.rf.bal$Y)
names(bank.train.rf.bal)[17] <- "y"
bank.rf.bal.model <- randomForest(y~.,data=bank.train.rf.bal,importance=T)
bank.rf.bal.pred <- predict(bank.rf.bal.model,bank.test.c50, type="prob")
pred.bal.rf <- prediction(bank.rf.bal.pred[,2],factor(bank.test.c50$y, labels=c(0,1)))
perf.bal.rf <- performance(pred.bal.rf, "tpr", "fpr")
plot(perf.bal.rf, add=T, col="purple")















