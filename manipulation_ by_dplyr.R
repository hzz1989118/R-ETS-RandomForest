##################################
#### data etracting functions ####
##################################
tempFUN <- function(x) {
  # this is a scalar fucntion
  # need to be vectorized later
  as.character(x) 
  if(x == "-2") {x <- "A"} 
  if(x == "-1") {x <- "B"}
  if(x == "0") {x <- "C"}
  if(x == "1") {x <- "D"}
  if(x == "2") {x <- "E"}
  if(x == "NULL") {x <- "N"}
  return(x)
  # the function returens a character
  # may be needed to be factorized later
} 
#:: recode function for slider status
recodeSliderAction <- Vectorize(tempFUN)
########################################
tempFUN <- function(x) {
  # this is a scalar fucntion
  # need to be vectorized later
  as.character(x) 
  if(x == "apply") {x <- "A"} 
  if(x == "Diagram") {x <- "D"}
  if(x == "reset") {x <- "R"}
  if(x == "NULL") {x <- "N"}
  return(x)
  # the function returens a character
  # may be needed to be factorized later
}
#:: recode function for action type
recodeEventType <- Vectorize(tempFUN)
########################################
tempFUN <- function(x) {
  # this is a scalar fucntion
  # need to be vectorized later
  temp <- strsplit(as.character(x), "(?<=.{2})", perl = TRUE)[[1]]
  temp[which(c(temp == "00") == T)] <- "F" 
  temp[which(c(temp == "01") == T)] <- "G"
  temp[which(c(temp == "10") == T)] <- "H"
  temp[which(c(temp == "11") == T)] <- "I"
  return(paste(c("D", temp), collapse = ""))
  # the function returens a character
  # may be needed to be factorized later
}
recodeDiagState <- Vectorize(tempFUN)
#######################################
myMelt <- function(x){
  if (as.character(x[1]) == "DNNN") {
    temp <- as.character(x[2])
  }
  else {
    temp <- as.character(x[1])
  }
  return(temp)
}



###########################
#### recoding the data ####
###########################
# this data is long-verson data, that is, action-wised
pisa12a_Recoded <- cbind(pisa12a[, -(7:10)],
                         event_type = recodeEventType(pisa12a[,7]),
                         top_setting = recodeSliderAction(pisa12a[,8]),
                         central_setting =  recodeSliderAction(pisa12a[,9]),
                         bottom_setting = recodeSliderAction(pisa12a[,10]))

# event_tpye  "N" will be changed to "s" standing for "start"
# will be change to "E" standing for "end"
pisa12a_Recoded[,10] <- as.character(pisa12a_Recoded[,10])
pisa12a_Recoded$event_type[which(pisa12a_Recoded$event == "START_ITEM")] <- "S"
pisa12a_Recoded$event_type[which(pisa12a_Recoded$event == "END_ITEM")] <- "E"

# collapse slidings in each action into a small sequence pattern
pisa12a_Recoded <- cbind(pisa12a_Recoded, 
                         collapsed_experiment_action = apply(pisa12a_Recoded[,10:13], 1, paste, collapse = ""))

# get out of "'" in "'000000" strings
pisa12a_Recoded$diag_state <- gsub("'", "", as.character(pisa12a_Recoded$diag_state))

# collapse linkings in each action into a small sequence pattern
pisa12a_Recoded <- cbind(pisa12a_Recoded,
                         collapsed_diag_action = recodeDiagState(pisa12a_Recoded$diag_state))
# combine the slidings and linkings into one column 
collapsed_action <- apply(pisa12a_Recoded[, 14:15], 1, myMelt)
 
pisa12a_Recoded <- cbind(pisa12a_Recoded, collapsed_action = collapsed_action)

# recode the ID and creat the uniID 
# and remove the redundant columns
pisa12a_Recoded_full <- pisa12a_Recoded
pisa12a_Recoded <- pisa12a_Recoded[,-(11:13)]
IDs <- cbind(as.character(pisa12a_Recoded$cnt), 
             sprintf("%07.0f", pisa12a_Recoded$schoolid),
             sprintf("%05.0f", pisa12a_Recoded$StIDStd))
uniID <- apply(IDs, 1, paste, collapse = "")
pisa12a_Recoded <- cbind(uniID = uniID, pisa12a_Recoded[,-(1:3)])
pisa12a_Recoded_full <- cbind(uniID = uniID, pisa12a_Recoded_full)
pisa12a_Recoded_full <- cbind(pisa12a_Recoded_full[,1:2],
                              schoolid = sprintf("%07.0f", pisa12a_Recoded_full$schoolid),
                              StIDStd = sprintf("%05.0f", pisa12a_Recoded_full$StIDStd),
                              pisa12a_Recoded_full[, 5:15])

pisa12a_Recoded_norepD <- pisa12a_Recoded[-which(D_rep_indicator == T),]
pisa12a_Recoded_full_norepD <- pisa12a_Recoded_full[-which(D_rep_indicator == T),]                              


# take a look at the data
head(cbind(pisa12a[, 7:10], pisa12a_Recoded[,10:14]))
tail(cbind(pisa12a[, 7:10], pisa12a_Recoded[,10:14]))



#########################################################
## take into the response now 

pisa12_res <- pisa12response[, c(1, 6, 7, 10)]
IDs <- cbind(as.character(pisa12_res$CNT),
             sprintf("%07.0f", pisa12_res$SCHOOLID),
             sprintf("%05.0f", pisa12_res$StIDStd))
uniID <- apply(IDs, 1, paste, collapse = "")
pisa12_res <- cbind(uniID = uniID,
                    cnt = as.character(pisa12_res$CNT),
                    schoolid = sprintf("%07.0f", pisa12_res$SCHOOLID),
                    StIDStd = sprintf("%05.0f", pisa12_res$StIDStd),
                    CP025Q01 = pisa12_res[, 4])
pisa12_res <- as.data.frame(pisa12_res)

pisa12_res_no7 <- filter(pisa12_res, CP025Q01 != 7)

pisa12Q25a <- full_join(pisa12_res_no7,pisasum_clean, by = "uniID")



#########################################
pisa12Q25a_clean <- filter(tbl_df(na.omit(pisa12Q25a)), CP025Q01 != 8)
#########################################
### clean data set ###
######################
#########################################
pisa12Q25a_clean_full <- na.omit(full_join(pisa12Q25a_clean, pisa12a_Recoded, by = 'uniID'))
pisa12Q25a_clean_full <- pisa12Q25a_clean_full %>%
                           group_by(uniID) %>%
                             mutate(each_action_time = time - lag(time))

####################
#### Dta Saving ####
####################
write.csv(data_descp, file = "C:/Users/zhan/Desktop/Project/R/Data/data_descp.csv")
write.csv(data_descp_clean, file = "C:/Users/zhan/Desktop/Project/R/Data/data_descp_clean.csv")
write.csv(data_descp_cleanID, file = "C:/Users/zhan/Desktop/Project/R/Data/data_descp_cleanID.csv")
write.csv(pisa12a_Recoded, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12a_Recoded.csv")
write.csv(pisa12a_Recoded_full, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12a_Recoded_full.csv")
write.csv(pisasum, file = "C:/Users/zhan/Desktop/Project/R/Data/pisasum.csv")
write.csv(pisasum_action, file = "C:/Users/zhan/Desktop/Project/R/Data/pisasum_action.csv")
write.csv(pisasum_clean, file = "C:/Users/zhan/Desktop/Project/R/Data/pisasum_clean.csv")
write.csv(pisa12Q25a_clean, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12Q25a_clean.csv")
saveRDS(pisa12Q25a_clean, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12Q25a_clean.rds")
saveRDS(pisa12a_Recoded, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12a_Recoded.rds")
saveRDS(pisa12a_Recoded_full, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12a_Recoded_full.rds")
saveRDS(pisa12a_Recoded_norepD, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12a_Recoded_norepD.rds")
saveRDS(pisa12a_Recoded_full_norepD, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12a_Recoded_full_norepD.rds")
saveRDS(pisa12Q25a_clean_full, file = "C:/Users/zhan/Desktop/Project/R/Data/pisa12Q25a_clean_full.rds")
