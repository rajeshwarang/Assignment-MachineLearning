
library(caret)
library(plyr)
library(dplyr)
library(randomForest)
library(ranger)
library(corrplot)
setwd("C:/Personal/Raj")

TrainingSet <- read.csv("pml-training.csv" , colClasses = c("character"))

TestingSet <- read.csv("pml-testing.csv" , colClasses = c("character"))


# Remove the unnecessary Variables 

TrainingSet <- TrainingSet[ ,-c(1:7)]


TestingSet <- TestingSet[ ,-c(1:7)]


str(TrainingSet)


TrainingSet$classe


summary(TrainingSet)







TrainingSet <- sapply(TrainingSet , as.numeric)


TestingSet <- sapply(TestingSet , as.numeric)


summary(TrainingSet)

sapply(TrainingSet , function(x) sum(is.na(x)))


Training_NA <- apply(TrainingSet , 2 , function(x) sum(is.na(x))) 

View(Training_NA)

Training_NA <- as.data.frame(Training_NA)

Training_NA$ColumnNames  <- row.names(Training_NA)

row.names(Training_NA[which(Training_NA$Training_NA > 0 ) , ])

row.names(Training_NA)



Training_NA <-  Training_NA[which(Training_NA$Training_NA > 0 ) , ]



NA_ColIndex <- which(colnames(TrainingSet) %in% Training_NA$ColumnNames)


TrainingSet %>% select((NA_ColIndex)) %>% View()


TrainingSet <- TrainingSet[ , -NA_ColIndex]

TestingSet <- TestingSet[ , -NA_ColIndex]




# Get the number of zero entries 


TrainingSet <- as.data.frame(TrainingSet)

TestingSet <- as.data.frame(TestingSet)




apply(TrainingSet, 2, function(c)sum(c==0))



CorrData <- cor(TrainingSet)


corrplot(CorrData, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)


Training_PR_Comp <- prcomp(TrainingSet , scale. = T)

names(Training_PR_Comp)


dim(Training_PR_Comp$x)


std_dev <- Training_PR_Comp$sdev


pr_var <- std_dev^2


pr_var[1:10]


prop_varex <- pr_var/sum(pr_var)


prop_varex


plot(prop_varex , xlab = "Principal Component" , ylab = "proportion explained" , type = "b")



#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")



Training.pca <- data.frame(Training_PR_Comp$x)


View(Training_PR_Comp)



Training.pca <- Training.pca[ , 1:35]


Testing_PR_Comp <- predict(Training_PR_Comp , TestingSet)

Testing.pca <- as.data.frame(Testing_PR_Comp)


Testing.pca <- Testing.pca[ , 1:35]



Training_withOutput <- cbind(Training.pca , TrainingOutput)


str(Training_withOutput)

# Building a Model 

library(caret)

# Create model with default paramters

control <- trainControl(method="repeatedcv", number=5, repeats=3)

seed <- 7

metric <- "Accuracy"

set.seed(seed)

mtry <- sqrt(ncol(Training_withOutput))

tunegrid <- expand.grid(.mtry=mtry)

rf_default <- train(TrainingOutput~., data=Training_withOutput, method="rf",
                    metric=metric, tuneGrid=tunegrid, trControl=control)

print(rf_default)

print(rf_default$finalModel)

rf_default$results

Predict_Test <- predict(rf_default , Testing.pca)

Predict_Test

































