###################################
#####    much higher level    #####
###################################
#######################################################
MuchHigherLevel <- function(x){
  #:: x is the vector of ActionSequence and Highlevel
  a <- unlist(strsplit(as.character(x[1]), ", "))
  b <- unlist(strsplit(as.character(x[2]), ""))
  
  #:: remove the R since it will not be used later
  if (length(which(b == "R")) == 0) {
    b <- b
  } else {
      a <- a[-which(b == "R")]
      b <- b[-which(b == "R")]
  }
  
  #:: remove the first D whose pattern is "DFFF"
  #:: this is trivial case in terms of our purpose
  if (length(which(b == "D")) == 0) {
    b <- b
  } else {
      if(a[which(b == "D")[1]] == "DFFF") {
        b <- b[-(which(b == "D")[1])]
      }
  }
  out <- paste(b, collapse = "")
  return(out)
}

####################################################################
MuchHigherLevelSequence <- apply(cleandat[, 7:8], 1, MuchHigherLevel)
####################################################################


################################################################
# This Function helps to get more condensed MuchHigherSequence
################################################################
ApplyVsDiagramBehavior <- function (x) {
  #:: x is the MuchHigherLevelSequence
  x <- unlist(strsplit(as.character(x), ""))
  #:: get rid of S and E
  if (length(x) > 2) {
    #:: do not consider the case "SE"
    x <- x[-c(1,length(x))]
    if (length(unique(x)) == 1) {
      out <- "N"
    } else {
      #:: get rid of the rep A in the x
      reporder <- which(x[1:(length(x) - 1)] == x[2:length(x)]) #& x[1:(length(x) - 1)] == "A")
      if (length(reporder) > 0) {
        temp <- x[-reporder]
        out <- paste(temp, collapse = "") 
      } else {
        out <- paste(x, collapse = "")
      }
    }
  } else { #:: this case is "SE" in MuchHigherLevelSequence
    out <- "N"
  }
}

A_vs_D_Behavior <- apply(as.matrix(MuchHigherLevelSequence), 1, ApplyVsDiagramBehavior)

##################################################################################


#########################
##### AD predictors #####
#########################
AD_predictors <- as.data.frame(lapply(names(table(A_vs_D_Behavior)[which(table(A_vs_D_Behavior) >= 30)]), function(x){list = c(A_vs_D_Behavior == x)*1}))
colnames(AD_predictors) <- names(table(A_vs_D_Behavior)[which(table(A_vs_D_Behavior) >= 30)])
LowFreNames <- names(table(A_vs_D_Behavior)[which(table(A_vs_D_Behavior) < 30)])
LowfrequncyADpattern <- apply(as.matrix(A_vs_D_Behavior), 1, function(x){any(x == LowFreNames)*1})
AD_predictors <- cbind(AD_predictors, LowfrequncyADpattern)

AD_predictors_noFreqLimit <- as.data.frame(lapply(names(table(A_vs_D_Behavior)), function(x){list = c(A_vs_D_Behavior == x)*1}))
colnames(AD_predictors_noFreqLimit) <- names(table(A_vs_D_Behavior)) 


COR_AD <- apply(AD_predictors, 2, cor, y = cleandat$response)


saveRDS(AD_predictors, "C:/Users/zhan/Desktop/Project/R/Data/AD_predictors.rds")
write.csv(AD_predictors, "C:/Users/zhan/Desktop/Project/R/Data/AD_predictors.csv")
write.csv(COR_AD, "C:/Users/zhan/Desktop/Project/R/Data/COR_AD.csv")