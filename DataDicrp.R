#############################
###   Data Discription    ###
#############################

##### Raw Table summarized From Process Data set ####
include_IDmissing_descrp <- filter(tbl_df(pisa12a_Recoded_full), event_number == 1)
include_IDmissing_descrp <- data.frame(Country = names(table(include_IDmissing_descrp$cnt)),
                                       NumberOfStudents = as.vector(table(include_IDmissing_descrp$cnt)))

write.csv(include_IDmissing_descrp, file = "C:/Users/zhan/Desktop/Project/R/Data/include_IDmissing_descrp.csv")

##### Raw Table Summarized From Response Data #####
raw_response_descrp <- data.frame(Country = names(table(pisa12_res_no7$cnt)),
                                  NumberOfStudents = as.vector(table(pisa12_res_no7$cnt)))

write.csv(raw_response_descrp, file = "C:/Users/zhan/Desktop/Project/R/Data/raw_response_descrp.csv")

##### Table Summarized From Clean Combined Data #####
clean_data_descrp <- data.frame(Country = names(table(pisa12Q25a_clean$cnt)),
                                NumberOfStudents = as.vector(table(pisa12Q25a_clean$cnt)))

write.csv(clean_data_descrp, file = "C:/Users/zhan/Desktop/Project/R/Data/clean_data_descrp.csv")