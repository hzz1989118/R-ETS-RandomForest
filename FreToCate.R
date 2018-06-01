#########################################
#### Frequency into Categorical data ####
#########################################

#################################################################################
##:: This function recodes the counts into categorical data
##:: only counts with frequency large than 30 will be categorized;
##:: low frequency count will be recoded into one category
getFineCate <- function(x) {
                 tempname <- names(table(x))[which(table(x) >= 100)]
                 out <- apply(as.matrix(x), 1, function(x) {tempout <- ifelse(any(as.character(x) == tempname) == T, 
                                                                              tempname[which(as.character(x) == tempname)],
                                                                              "LowFreq")
                                                            return(tempout)})
                 return(as.factor(out))
}

#################################################################################

#################################################################################
getFineCate4time


########################
##:: Gram
########################

Fre2Cate_Grams <- apply(Gram_predictors[,-c(1, 2, 6)], 2, 
                        function(x){
                          cut(x, breaks = c(-Inf,unique(summary(x)[-c(1,4,6)]), Inf))
                          })

FineFre2Cate_Grams <- apply(Gram_predictors[, -c(1,2,6)], 2, getFineCate)
FineFre2Cate_Grams <- as.data.frame(FineFre2Cate_Grams)

FineFre2Cate_Grams <- as.data.frame(apply(Gram_predictors[, -c(1,2,6)], 2,
                                          function(x){
                                            tempbreak <- unique(quantile(x,probs=seq(0,1,0.1)))
                                            if (length(tempbreak) > 2) {
                                              cut(x, breaks = tempbreak,
                                              include.lowest=TRUE,right=FALSE)}
                                            else {
                                              cut(x, breaks = c(0,1,Inf), 
                                                  include.lowest = T,
                                                  right = F)
                                              }
                                            }))




write.csv(round(apply(Fre2Cate_Grams[,-c(1,5)], 2, function(x){cor(as.numeric(x),cleandat$response)}),3), "C:/Users/zhan/Desktop/Project/R/Data/COR_Gram_rescaled.csv")
#################################################################################
########################
##:: Time
########################
Time2Cate <- apply(cleandat[,88:93], 2, 
                  function(x){
                    cut(x, breaks = c(-Inf,unique(summary(x)[-c(1,4,6)]), Inf))
                  })
Time2Cate <- as.data.frame(Time2Cate)

FineTime2Cate <- as.data.frame(apply(cleandat[,88:93], 2,
                                     function(x){cut(x, breaks = unique(quantile(x,probs=seq(0,1,0.1))),
                                                     include.lowest=TRUE,right=FALSE)}))

############################################################################
getDes4eachCate <- function(x) {
  Freq <- NULL
  num_right <- NULL
  percentage <- NULL
  cate <- names(table(x))
  for (i in 1 : length (cate)) {
    Freq[i] <- sum(x == cate[i])
    num_right[i] <- sum(cleandat$response[which(x == cate[i])] == 1)
    percentage[i] <- (num_right[i]/Freq[i])*100
  }
  out <- as.data.frame(cbind(Freq, num_right, percentage))
  rownames(out) <- cate
  return(out)
}




########################
##:: num_exact_rep
########################
Fre2Cate_exactRep <- cut(num_exact_rep, breaks = c(-Inf,unique(summary(num_exact_rep)[-c(1,4,6)]), Inf))
FineFre2Cate_exactRep <- getFineCate(num_exact_rep)
########################
##:: VOTAT_predictors
########################
Fre2Cate_VOTAT <- apply(VOTAT_predictors[,c(2,9)], 2, 
                        function(x){
                          cut(as.numeric(x), breaks = c(-Inf,unique(summary(as.numeric(x))[-c(1,4,6)]), Inf))
                        })
FineFre2Cate_VOTAT <- apply(VOTAT_predictors[,c(2,9)], 2, getFineCate)
########################
##:: AD_predictors
########################
Fre2Cate_AD <- as.factor(as.vector(apply(AD_predictors, 1, function(x){out <- colnames(AD_predictors)[which(x == 1)]})))
Fre2Cate_AD_noFreqLimit <- as.factor(as.vector(apply(AD_predictors_noFreqLimit, 1, function(x){out <- colnames(AD_predictors_noFreqLimit)[which(x == 1)]})))
summarized_AD <- apply(as.matrix(as.character(Fre2Cate_AD_noFreqLimit)), 1, 
function(x){
  if (substr(x, 1, 1) == "D"){out <- "StartFromD"} 
  else if (substr(x, 1, 1) == "N") {out <- "Incomplete"} 
  else if (x == "AD") {out <- "AD"} 
  else if (nchar(x) < 6 & nchar(x) > 2) {out <- "1<=AD<3"} 
  else {out <- "AD >= 3"} 
  return(out)
  })
########################
##:: DrawfirstRight
########################
Fre2Cate_DrawFirst <- as.factor(as.vector(apply(as.matrix(DrawRightatFirst), 1, function(x){ifelse(x == 1, "Correct", "Incorrect")})))

#######################
##:: n_actions
#######################
Fre2Cate_n_actions <- cut(cleandat$n_actions, breaks = unique(quantile(cleandat$n_actions,probs=seq(0,1,0.1))),
                          include.lowest=TRUE,right=FALSE)
FineFre2Cate_n_actions <- getFineCate(cleandat$n_actions)











##############################
##:: Call all descriptives
##############################

##:: Gram
apply(Fre2Cate_Grams, 2, getDes4eachCate)
apply(FineFre2Cate_Grams, 2, getDes4eachCate)
lapply(apply(FineFre2Cate_Grams, 2, getDes4eachCate), function(x){x[order(x[,3]), ]})
##:: Time
apply(Time2Cate, 2, getDes4eachCate)
apply(FineTime2Cate, 2, getDes4eachCate)
lapply(apply(FineTime2Cate, 2, getDes4eachCate), function(x){x[order(x[,3]), ]})
##:: num_exact_rep
getDes4eachCate(Fre2Cate_exactRep)
getDes4eachCate(FineFre2Cate_exactRep)[order(getDes4eachCate(FineFre2Cate_exactRep)[,3]), ]
##:: VOTAT
apply(Fre2Cate_VOTAT, 2, getDes4eachCate)
apply(FineFre2Cate_VOTAT, 2, getDes4eachCate)
##:: AD
getDes4eachCate(Fre2Cate_AD)
write.csv(getDes4eachCate(Fre2Cate_AD), "C:/Users/zhan/Desktop/Project/R/Data/AD_Cate_Descp.csv")
getDes4eachCate(Fre2Cate_AD_noFreqLimit)
write.csv(getDes4eachCate(Fre2Cate_AD_noFreqLimit), "C:/Users/zhan/Desktop/Project/R/Data/AD_Cate_Descp.csv")
getDes4eachCate(summarized_AD)

###################################################
##:: incomplete people but right
cleandat$cnt[which(cleandat$response == 1 & summarized_AD == "Incomplete")]



##:: FirstDraw
getDes4eachCate(Fre2Cate_DrawFirst)
##:: n_action
getDes4eachCate(Fre2Cate_n_actions)
getDes4eachCate(FineFre2Cate_n_actions)
getDes4eachCate(FineFre2Cate_n_actions)[order(getDes4eachCate(FineFre2Cate_n_actions)[,3]), ]
##:: n_act_type
getDes4eachCate(cleandat$n_act_type)

################################################
##:: cleandat_cate
################################################
cleandat_cate <- cbind(cleandat[,1:8], 
                       FineFre2Cate_Grams,
                       FineFre2Cate_VOTAT,
                       "AD_preidctor" = summarized_AD,
                       "DrawRightatFirst" = cleandat$DrawRightatFirst,
                       "n_actions" = Fre2Cate_n_actions,
                       "n_actions_Fine" = FineFre2Cate_n_actions,
                       "n_actions" = cleandat$n_actions,
                       FineTime2Cate)
saveRDS(cleandat_cate, "C:/Users/zhan/Desktop/Project/R/Data/cleandat_cate.rds")
#################################################################################