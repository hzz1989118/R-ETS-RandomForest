
### clean data
table(na.omit(pisa12Q25a)$cnt)
length(na.omit(pisa12Q25a)$cnt)
### response clean data
table(pisa12_res_no7$cnt)
length(pisa12_res_no7$cnt)
### raw process data 
table(include_IDmissing_descrp$cnt)
length(include_IDmissing_descrp$cnt)


### no response but have actions ###
table(inner_join(filter(tbl_df(pisa12Q25a), is.na(CP025Q01))[,1], 
                 filter(tbl_df(pisa12a_Recoded_full), event_number == 1), 
                 by = "uniID")$cnt)
length((inner_join(filter(tbl_df(pisa12Q25a), is.na(CP025Q01))[,1], 
                   filter(tbl_df(pisa12a_Recoded_full), event_number == 1), 
                   by = "uniID")$cnt))

### no actions but have response ###
table(filter(tbl_df(pisa12Q25a), is.na(ActionSequence))$cnt)
length(filter(tbl_df(pisa12Q25a), is.na(ActionSequence))$cnt)


### country intersection ###
intersect(names(table(na.omit(pisa12Q25a)$cnt)),
          names(table(include_IDmissing_descrp$cnt)))

### country diff ###
setdiff(union(names(table(include_IDmissing_descrp$cnt)), 
              names(table(pisa12_res_no7$cnt))),
        intersect(names(table(na.omit(pisa12Q25a)$cnt)),
                        names(table(include_IDmissing_descrp$cnt))))
        
