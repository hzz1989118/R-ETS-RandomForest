###########################
### final model fitting ###
###########################

cleandat_cate_final <- cleandat_cate_reduced[,c(6, apply(as.matrix(finalVars), 1, 
                                                   function(x){which(colnames(cleandat_cate_reduced) == x)}))]

saveRDS(cleandat_cate_final, file = "C:/work/ETS/2015Sintern/R/cleandat_cate_final.rds")
#########################################################
set.seed(227)

inTrain.GBT <- createDataPartition(cleandat_cate_final[,1], p = .3, list = F)
trainVar.GBT <- cleandat_cate_final[,c(-1)][inTrain.GBT,]
testVar.GBT <- cleandat_cate_final[,c(-1)][-inTrain.GBT,]
trainClass.GBT <- as.factor(cleandat_cate_final[,1])[inTrain.GBT]
testClass.GBT <- as.factor(cleandat_cate_final[,1])[-inTrain.GBT]


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
system.time(GBTfit3 <- train(x = trainVar.GBT, y = trainClass.GBT,
                             method = "gbm", trControl = GBTControl,
                             tuneGrid = GBTgrid , verbose = F,
                             metric = c("ROC")))
stopCluster(cl)
plot(GBTfit3)
plot(varImp(GBTfit3), top = 20)
saveRDS(GBTfit3, file = "C:/work/ETS/2015Sintern/R/GBTfit3.rds")
pred3 <- predict(GBTfit3, testVar.GBT)
confusionMatrix(pred3, testClass.GBT)
getTrainPerf(GBTfit3)

gbmfit1 <- gbm(formula = as.formula(paste("response ~ ", 
                                          paste(colnames(cleandat_cate_final)[-1], collapse = " + "))),
               data = cleandat_cate_final[inTrain.GBT,],
               n.tree = 100,
               interaction.depth = 2,
               shrinkage = .1)

varcombnum <- combn(2:17, 2)
varname <- t(apply(varcombnum, 2, function(x){colnames(cleandat_cate_final)[x]}))
interactionGBT <- t(apply(varcombnum, 2, function(x){interact.gbm(gbmfit1, cleandat_cate_final, i.var = as.vector(x - 1))}))
GBTInteract <- data.frame(varname, t(interactionGBT))

saveRDS(gbmfit1, file = "C:/work/ETS/2015Sintern/R/gbmfit1.rds")
#####################  RF  ###############################
set.seed(1110)

inTrain.RF <- createDataPartition(cleandat_cate_final[,1], p = .3, list = F)
trainVar.RF <- cleandat_cate_final[,c(-1)][inTrain.RF,]
testVar.RF <- cleandat_cate_final[,c(-1)][-inTrain.RF,]
trainClass.RF <- as.factor(cleandat_cate_final[,1])[inTrain.RF]
testClass.RF <- as.factor(cleandat_cate_final[,1])[-inTrain.RF]


RFgrid <- expand.grid("mtry" = c(2, 4, 8))
RFControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3,
                          summaryFunction = twoClassSummary,
                          classProbs = T)
cl <- makeCluster(8)
registerDoParallel(cl)
system.time(RFfit2 <- train(x = trainVar.RF, y = trainClass.RF,
                            method = "rf", trControl = RFControl,
                            tuneGrid = RFgrid , verbose = F,
                            metric = c("ROC")))
stopCluster(cl)
plot(RFfit2)
plot(varImp(RFfit2), top = 20)
saveRDS(RFfit2, file = "C:/work/ETS/2015Sintern/R/RFfit2.rds")

randomFfit1 <- randomForest(x = trainVar.RF,
                            y = trainClass.RF,
                            ntree = 300,
                            importance = T)
saveRDS(randomFfit1, file = "C:/work/ETS/2015Sintern/R/randomFfit1.rds")
importance(randomFfit1)[order(importance(randomFfit1)[,3], decreasing = T),]
