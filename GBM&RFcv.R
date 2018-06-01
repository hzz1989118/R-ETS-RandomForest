#####################
### Votat_group #####
#####################
cleandat_cate <-readRDS("C:/Users/Zhuangzhuang/Downloads/cleandat_cate.rds")
deletedVar <- c("AA", "SA", "SDD", "SR", "SRA", "RR", "ARR", "RRA",
                "DDD", "RA", "AR", "DA", "AD", "AE", "DE",
                "AAE", "ARE", "DDA", "ADE", "DDE", "RDD", "RD", "ADR",
                "DrawRightatFirst", "num_VOTAT")
cleandat_cate_reduced <- cleandat_cate[,-c(apply(as.matrix(deletedVar), 1, 
                                                 function(x){which(colnames(cleandat_cate) == x)}),81,82)]


#####################  GBM   ###########################

set.seed(226)

inTrain.GBT <- createDataPartition(cleandat_cate_reduced[,6], p = .3, list = F)
trainVar.GBT <- cleandat_cate_reduced[,c(9:61)][inTrain.GBT,]
testVar.GBT <- cleandat_cate_reduced[,c(9:61)][-inTrain.GBT,]
trainClass.GBT <- as.factor(cleandat_cate_reduced[,6])[inTrain.GBT]
testClass.GBT <- as.factor(cleandat_cate_reduced[,6])[-inTrain.GBT]


GBTgrid <- expand.grid(n.trees = c(50,100,300),
                       interaction.depth = c(1, c(1:6)*2),
                       shrinkage = c(0.001, 0.01, 0.1, 1))
GBTControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           summaryFunction = twoClassSummary,
                           classProbs = T)
cl <- makeCluster(8)
registerDoParallel(cl)
system.time(GBTfit2 <- train(x = trainVar.GBT, y = trainClass.GBT,
                             method = "gbm", trControl = GBTControl,
                             tuneGrid = GBTgrid , verbose = F,
                             metric = c("ROC")))
stopCluster(cl)
plot(GBTfit2)
plot(varImp(GBTfit2), top = 20)
saveRDS(GBTfit2, file = "C:/work/ETS/2015Sintern/R/GBTfit2.rds")
pred2 <- predict(GBTfit2, testVar.GBT)
confusionMatrix(pred2, testClass.GBT)
getTrainPerf(GBTfit2)

#######################################################
#####################  RF   ###########################

set.seed(1109)

inTrain.RF <- createDataPartition(cleandat_cate_reduced[,6], p = .3, list = F)
trainVar.RF <- cleandat_cate_reduced[,c(9:61)][inTrain.RF,]
testVar.RF <- cleandat_cate_reduced[,c(9:61)][-inTrain.RF,]
trainClass.RF <- as.factor(cleandat_cate_reduced[,6])[inTrain.RF]
testClass.RF <- as.factor(cleandat_cate_reduced[,6])[-inTrain.RF]


RFgrid <- expand.grid("mtry" = c(3, 7, 14))
RFControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           summaryFunction = twoClassSummary,
                           classProbs = T)
cl <- makeCluster(8)
registerDoParallel(cl)
system.time(RFfit1 <- train(x = trainVar.RF, y = trainClass.RF,
                             method = "rf", trControl = RFControl,
                             tuneGrid = RFgrid , verbose = F,
                             metric = c("ROC")))
stopCluster(cl)
plot(RFfit1)
plot(varImp(RFfit1), top = 20)
saveRDS(RFfit1, file = "C:/work/ETS/2015Sintern/R/RFfit1.rds")
