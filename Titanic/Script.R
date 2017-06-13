library(randomForest)
library(rpart)

setwd("~/GitHub/Kaggle/Titanic")
Data <- read.csv(file="train.csv",head=TRUE,sep=",")
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=Data[!is.na(Data$Age),], 
                method="anova")
Data$Age[is.na(Data$Age)] <- predict(Agefit, Data[is.na(Data$Age),])
Data$Embarked[which((Data$Embarked == ""))] = "S"
Data$Fare[is.na(Data$Fare)==TRUE] = mean(Data$Fare[!is.na(Data$Fare)])
Data$numFamilyMembers = Data$SibSp + Data$Parch + 1
Data$Name <- as.character(Data$Name)
Data$Title <- sapply(Data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
Data$Title <- sub(' ', '', Data$Title)
Data$Title[Data$Title %in% c('Capt', 'Col', 'Major', 'Dr', 'Rev')] <- 'Officer'
Data$Title[Data$Title %in% c('Jonkheer', 'Don', 'Sir', 'the Countess', 'Dona', 'Lady')] <- 'Royalty'
Data$Title[Data$Title %in% c('Mme', 'Ms', 'Mrs')] <- 'Mrs'
Data$Title[Data$Title %in% c('Mlle', 'Miss')] <- 'Miss'
Data$Title <- factor(Data$Title)
Data$Cabin = as.character(Data$Cabin)
Data$Cabin[Data$Cabin == ""] <- 'U'
Data$Cabin[Data$Cabin!='U'] = substr(Data$Cabin[Data$Cabin!='U'], 1, 1)
Data$Cabin = factor(Data$Cabin)

#plot(Data$Survived,Data$SibSp)
#smp_size <- floor(0.75 * nrow(Data))
#set.seed(50)
#train_ind <- sample(seq_len(nrow(Data)), size = smp_size)
train = Data


Data <- read.csv(file="test.csv",head=TRUE,sep=",")
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=Data[!is.na(Data$Age),], 
                method="anova")
Data$Age[is.na(Data$Age)] <- predict(Agefit, Data[is.na(Data$Age),])
Data$Embarked[which((Data$Embarked == ""))] = "S"
Data$Fare[is.na(Data$Fare)==TRUE] = mean(Data$Fare[!is.na(Data$Fare)])
Data$numFamilyMembers = Data$SibSp + Data$Parch + 1
Data$Name <- as.character(Data$Name)
Data$Title <- sapply(Data$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
Data$Title <- sub(' ', '', Data$Title)
Data$Title[Data$Title %in% c('Capt', 'Col', 'Major', 'Dr', 'Rev')] <- 'Officer'
Data$Title[Data$Title %in% c('Jonkheer', 'Don', 'Sir', 'the Countess', 'Dona', 'Lady')] <- 'Royalty'
Data$Title[Data$Title %in% c('Mme', 'Ms', 'Mrs')] <- 'Mrs'
Data$Title[Data$Title %in% c('Mlle', 'Miss')] <- 'Miss'
Data$Title <- factor(Data$Title)
Data$Cabin = as.character(Data$Cabin)
Data$Cabin[Data$Cabin == ""] <- 'U'
Data$Cabin[Data$Cabin!='U'] = substr(Data$Cabin[Data$Cabin!='U'], 1, 1)
Data$Cabin = factor(Data$Cabin)

Data$Survived = NaN
combi <- rbind(train, Data)
test = combi[(nrow(train)+1):nrow(combi),]

#train <- Data[train_ind, ]
#test <- Data[-train_ind, ]

set.seed(100)
rf <- randomForest(as.factor(Survived) ~ Pclass
                                         + Sex 
                                         + Age 
                                         + SibSp 
                                        # + Parch 
                                         + Fare
                                         + numFamilyMembers 
                                         + Title 
                                         + Cabin
                                         , data=train, importance=TRUE, 
                                         ntree=3000,
                                         maxnodes = round(sqrt(nrow(train))))
plot(rf)
rf.legend <- if (is.null(rf$test$err.rate)) {colnames(rf$err.rate)} else {colnames(rf$test$err.rate)}
legend("top", cex =0.5, legend=rf.legend, lty=c(1,2,3), col=c(1,2,3), horiz=T)


pSurvived = predict(rf, test, OOB=TRUE, type = "response")
Out = data.frame(PassengerId = test$PassengerId, Survived = pSurvived)
write.csv(Out, file="output.csv", row.names = FALSE) 
pSurvived = predict(rf, test)

varImpPlot(rf)
# probability = predict(rf, test, type="prob")
# accuracy <- sum(test$Survived == pSurvived)/nrow(test)
# print(accuracy)
