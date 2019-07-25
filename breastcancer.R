library(class)
library(caret)
library(e1071)

setwd("C:\\Users\\Admin\\Documents\\Test_Data")
cancer <- read.csv("Breastcancerdata.csv", sep = ",")

cancer_NoID <- cancer[, -1]
sapply(cancer_NoID,class)#

cancer_NoResults <- cancer_NoID[,-1]
FeaturesScaling <- function(x){((x-min(x))/(max(x)-min(x)))}
cancer_Normalised <- as.data.frame((lapply(cancer_NoResults, FeaturesScaling)))
cancer_Normalised <- cbind(cancer_Normalised,diagnosis=cancer_NoID[,1])

validation <- createDataPartition(cancer_NoID$diagnosis,p=0.80,list=FALSE)#
cancer_Training <- cancer_Normalised[validation,]
cancer_Test <- cancer_Normalised[-validation,]
cancer_Training_check <-cancer_Normalised[validation ,2]#
cancer_Test_check <-cancer_Normalised[-validation ,2]#

K_value <- floor(sqrt(length(cancer_Training[,1])))
head(cancer_Training[1:29])
head(cancer_Test[1:29])
cancer_prediction <- knn(cancer_Training[1:29],cancer_Test[1:29],cancer_Training$diagnosis,k=K_value)#

cancer_Reference <- cancer_NoID[451:569,1]
head(cancer_prediction)
head(cancer_Test$diagnosis)

confusionMatrix(cancer_prediction,cancer_Test$diagnosis)
head(knn_factor)#
sapply(cancer_prediction, class)
sapply(cancer_Test$diagnosis, class)

