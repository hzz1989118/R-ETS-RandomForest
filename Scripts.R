cleandat_cate <-readRDS("C:/Users/Zhuangzhuang/Downloads/cleandat_cate.rds")
deletedVar <- c("AA", "SA", "SDD", "SR", "SRA", "RR", "ARR", "RRA",
                "DDD", "RA", "AR", "DA", "AD", "AE", "DE",
                "AAE", "ARE", "DDA", "ADE", "DDE", "RDD", "RD", "ADR",
                "DrawRightatFirst", "VOTAT_group")



cleandat_cate_reduced <- cleandat_cate[,-c(apply(as.matrix(deletedVar), 1, 
                                                 function(x){which(colnames(cleandat_cate) == x)}),81,82)]
library(randomForest)

system.time(fit <- randomForest(x = cleandat_cate_reduced[,c(9:59)],
                                y = as.factor(cleandat_cate[,6]),
                                importance = T))

lassofit <- glmnet(x = as.data.frame(cleandat_cate[,c(9:80, 83:88)]),
                    y = as.factor(cleandat_cate[,6]),
                    alpha = 1, family = "binomial")
###########################################################################################
rfGrid <- expand.grid(c(300,500,1000,2000), 2^(1:5))
RFcv4tuning <- apply(rfGrid, 1, function(grid){tempfit <- randomForest(x = cleandat_cate[,c(9:80, 83:88)],
                                                                       y = as.factor(cleandat_cate[,6]), 
                                                                       ntree = grid[1],
                                                                       mtry = grid[2])
                                               out <- mean(tempfit$err.rate[,1])
                                               return(out)})

RFcv4tune <- data.frame("Grids" = apply(rfGrid, 1, paste, collapse = "X"),
                        "MOOB.err" = RFcv4tuning)
saveRDS(RFcv4tune, file = "C:/work/ETS/2015Sintern/R/RFcv4tune.rds")
plot(x = RFcv4tune$MOOB.err, type = "b")
RFcv4tune[which(grepl("5", RFcv4tune$Grids)),]
########################################################################################
RFvalSelfit <- randomForest(x = cleandat_cate[,c(9:80, 83:88)],
                            y = as.factor(cleandat_cate[,6]),
                            importance = T,
                            ntree = 500,
                            mtry = 16)

saveRDS(RFvalSelfit, file = "C:/work/ETS/2015Sintern/R/RFvalSelfitFull.rds")
write.csv(importance(RFvalSelfit), file = "C:/work/ETS/2015Sintern/R/impFull.csv")
varImpPlot(RFvalSelfit)

#########################################################################################
#################################################################################
#:: After removing the highly correlated item,
#:: we need to run cv again to find the best m.
#################################################################################
saveRDS(cleandat_cate_reduced,"C:/Users/Zhuangzhuang/Downloads/cleandat_cate_reduced.rds")
cleandat_cate_reduced <-readRDS("C:/Users/Zhuangzhuang/Downloads/cleandat_cate_reduced.rds")
rfGrid2 <- seq(4,16, by = 2)
RFcv4tuning2 <- apply(as.matrix(rfGrid2), 1, function(grid){tempfit <- randomForest(x = cleandat_cate_reduced[,9:70],
                                                                       y = as.factor(cleandat_cate_reduced[,6]),
                                                                       ntree = 300,
                                                                       mtry = grid)
                                                            out <- mean(tempfit$err.rate[,1])
                                                            return(out)})
##################################################################################
RFselect <- rfcv(cleandat_cate_reduced[, 9:70],
                 as.factor(cleandat_cate_reduced[,6]),
                 cv.fold = 5)
saveRDS(RFselect, file = "C:/work/ETS/2015Sintern/R/RFselect.rds")
with(RFselect, plot(n.var, error.cv, log="x", type="o", lwd=2))
#::Based on this, we know that 8-varible set size can have 80% of predictive accuracy
#################################
##:: Backward Elimination
##################################
leaveout_var <- NULL
MOOB.err <- NULL
tempx <- cleandat_cate_reduced[, 9:59]

for(i in 1:49) {
  tempfit <- randomForest(x = tempx,
                          y = as.factor(cleandat_cate_reduced[,6]),
                          importance = T,
                          ntree = 300)
  tempMOOB.err <- mean(tempfit$err.rate[,1])
  templeaveout_var <- rownames(importance(tempfit))[order(importance(tempfit)[,3], decreasing = F)][1]
  
  #::update the tempx (i.e. exclude the last-important var)
  tempx <- tempx[,-which(colnames(tempx) == templeaveout_var)]
  
  ##:: record the eliminated var and the Moob error rate in this round
  ifelse(i > 1, MOOB.err <- c(MOOB.err, tempMOOB.err), MOOB.err <- tempMOOB.err)
  ifelse(i == 49, leaveout_var <- leaveout_var, leaveout_var <- c(leaveout_var, templeaveout_var))
}

BackelimRFdescp <- data.frame("Elimated.Var" = leaveout_var,
                              "oob.rate" = MOOB.err[-1])

plot(BackelimRFdescp$oob.rate, type = "b", 
     ylab = "OOB.err.rate", xlab = "Leave-out Size",
     main = "Random Forest Backward Elimination")
points(x = 40, y = MOOB.err[41], col = "red", pch = 19)
abline(v = 40, lty = 4)
saveRDS(BackelimRFdescp, file = "C:/work/ETS/2015Sintern/R/BackelimRFdescp.rds")
finalVars <- c(rownames(importance(tempfit)), as.character(BackelimRFdescp$Elimated.Var[48:40]))

#################################
###############################
### Grediant Boosting Tree  ###
###############################
library(caret)
library(doParallel)
##########################################################
##:: Full Var
set.seed(1108)

inTrain.GBT <- createDataPartition(cleandat_cate[,6], p = .2, list = F)
trainVar.GBT <- cleandat_cate[,c(9:80, 83:88)][inTrain.GBT,]
testVar.GBT <- cleandat_cate[,c(9:80, 83:88)][-inTrain.GBT,]
trainClass.GBT <- as.factor(cleandat_cate[,6])[inTrain.GBT]
testClass.GBT <- as.factor(cleandat_cate[,6])[-inTrain.GBT]


GBTgrid <- expand.grid(n.trees = c(100,300,500,1000),
                       interaction.depth = (1:4) * 3,
                       shrinkage = c(.001, .01, .1))
GBTControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           summaryFunction = twoClassSummary,
                           classProbs = T)
cl <- makeCluster(detectCores()/2)
registerDoParallel(cl)
system.time(GBTFull.fit <- train(x = trainVar.GBT, y = trainClass.GBT,
                                 method = "gbm", trControl = GBTControl,
                                 tuneGrid = GBTgrid , verbose = F,
                                 metric = "ROC"))
stopCluster(cl)
plot(GBTFull.fit)
plot(varImp(GBTFull.fit), top = 20)
saveRDS(GBTFull.fit, file = "C:/work/ETS/2015Sintern/R/GBTFullfit.rds")
#####################################################
##########################################################
##:: Reduced Var
set.seed(225)

inTrain.GBT <- createDataPartition(cleandat_cate_reduced[,6], p = .2, list = F)
trainVar.GBT <- cleandat_cate_reduced[,c(9:59)][inTrain.GBT,]
testVar.GBT <- cleandat_cate_reduced[,c(9:59)][-inTrain.GBT,]
trainClass.GBT <- as.factor(cleandat_cate_reduced[,6])[inTrain.GBT]
testClass.GBT <- as.factor(cleandat_cate_reduced[,6])[-inTrain.GBT]


GBTgrid <- expand.grid(n.trees = c(100,300,500),
                       interaction.depth = c(1, c(1:6)*2),
                       shrinkage = c(0.001, 0.01, 0.1, 1))
GBTControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 3,
                           summaryFunction = twoClassSummary,
                           classProbs = T)
cl <- makeCluster(8)
registerDoParallel(cl)
system.time(GBTfit1 <- train(x = trainVar.GBT, y = trainClass.GBT,
                                 method = "gbm", trControl = GBTControl,
                                 tuneGrid = GBTgrid , verbose = F,
                                 metric = "ROC"))
stopCluster(cl)
plot(GBTfit1)
plot(varImp(GBTfit1), top = 20)
saveRDS(GBTfit1, file = "C:/work/ETS/2015Sintern/R/GBTfit1.rds")

pred1 <- predict(GBTfit1$, testVar.GBT)
confusionMatrix(pred1, testClass.GBT)
getTrainPerf(GBTfit1)
##############################
##:: best tune shrinkage = .1, ntree = 100, interaction.depth = 1
bestfit1 <- gbm.fit(x = trainVar.GBT, y = trainClass.GBT, n.trees = 100,
                    interaction.depth = 1, shrinkage = .1, distribution ="adaboost")


############################################################################
#### :: keep VOTAT_group
deletedVar <- c("AA", "R", "SA", "SDD", "SR", "SRA", "RR", "ARR", "RRA",
                "DDD", "ARA", "AAR", "RAR", "DA", "AD", "AE", "DE",
                "AAE", "ARE", "DDA", "ADE", "DDE", "RDD", "RD", "ADR",
                "DrawRightatFirst", "num_VOTAT")



cleandat_cate_reduced <- cleandat_cate[,-c(apply(as.matrix(deletedVar), 1, 
                                                 function(x){which(colnames(cleandat_cate) == x)}),81,82)]


