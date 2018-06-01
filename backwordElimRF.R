###################################
###  Backward Elimination by RF ###
###################################

leaveout_var <- NULL
MOOB.err <- NULL
tempx <- cleandat_cate_reduced[, 9:61]

for(i in 1:51) {
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
  ifelse(i == 51, leaveout_var <- leaveout_var, leaveout_var <- c(leaveout_var, templeaveout_var))
}

BackelimRFdescp <- data.frame("Elimated.Var" = leaveout_var,
                              "oob.rate" = MOOB.err[-1])

plot(BackelimRFdescp$oob.rate, type = "b", 
     ylab = "OOB.err.rate", xlab = "Leave-out Size",
     main = "Random Forest Backward Elimination")


points(x = 38, y = MOOB.err[39], col = "red", pch = 19)
abline(v = 38, lty = 4)
saveRDS(BackelimRFdescp, file = "C:/work/ETS/2015Sintern/R/BackelimRFdescp.rds")
finalVars <- c(rownames(importance(tempfit))[order(importance(tempfit)[,3], decreasing = T)],
               as.character(BackelimRFdescp$Elimated.Var[50:38]))
