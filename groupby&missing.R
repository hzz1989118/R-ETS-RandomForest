####################################
#### recoding the data by group ####
####################################

pisa12a_by_ID <- pisa12a_Recoded_norepD %>%
  group_by(uniID)

pisasum <- summarise(pisa12a_by_ID,
                     n_actions = n(),
                     n_act_type = n_distinct(event_type),
                     time_bf_act = time[2] - time[1],
                     totaltime = diff(range(time)))

pisasum_action <- summarise(pisa12a_by_ID, ActionSequence = paste(collapsed_action, collapse = ", "))


pisasum_clean <- cbind(pisasum, pisasum_action[, 2])
pisasum_clean <- pisasum_clean[which(grepl(" ", pisasum_clean$uniID) == F), ]

#################################################
#### calculate the each action time by group ####
#################################################

pisa12Q25a_clean_full <- pisa12Q25a_clean_full %>%
                           group_by(uniID) %>%
                             mutate(each_action_time = time - lag(time))






#####################
### missing data ####
#####################
##:: the long-versoned data missing

apply(is.na(pisa12a_Recoded), 2, sum)

sum(is.na(pisa12a_Recoded)[,2])/951481



sum(is.na(pisasum[,2]))
sum(is.na(pisasum_action[,2]))
apply(is.na(pisasum[,1:3]), 2, sum)
apply(is.na(pisasum[,4:7]), 2, sum)

as.data.frame(pisasum[is.na(pisasum[,2]),])

##:: there are 22 people have no schoolid and student id
as.data.frame(pisasum[is.na(pisasum[,6]),])



##########################
#### data description ####
##########################
##:: total sample size
dim(pisasum)[1]
##:: # of studnets in each cnt
table(pisasum$cnt)

pisasum[order(pisasum$n_actions, decreasing = T),]
pisasum[order(pisasum$time_bf_act, decreasing = T),]
pisasum[order(pisasum$totaltime, decreasing = T),]

data_descp <- data.frame(Country = names(table(pisasum$cnt)),
                         NumberOfStudents = as.vector(table(pisasum$cnt)))

data_descp_clean <- data.frame(Country = names(table(pisasum_clean$cnt)),
                               NumberOfStudents = as.vector(table(pisasum_clean$cnt)))

data_descp_cleanID <- data.frame(Country = names(table(na.omit(pisasum)$cnt)),
                                 NumberOfStudents = as.vector(table(na.omit(pisasum)$cnt)))

include_IDmissing_descrp <- filter(tbl_df(pisa12a_Recoded_full), event_number == 1)
include_IDmissing_descrp <- data.frame(Country = names(table(include_IDmissing_descrp$cnt)),
                                       NumberOfStudents = as.vector(table(include_IDmissing_descrp$cnt)))
write.csv(include_IDmissing_descrp, file = "C:/Users/zhan/Desktop/Project/R/Data/include_IDmissing_descrp.csv")



