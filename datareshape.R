##### data reading ####
pisa12a <- read.csv(file = "C:/Users/zhan/Desktop/Project/data/CPRO_logdata_released/CBA_cp025q01_logs12_SPSS.csv")
pisa12response <- read.csv(file = "C:/Users/zhan/Desktop/Project/data/PISA12CBA2response.csv")
pisa12response <- pisa12response[, c(1:9, 274)]

"CP025Q01"
# 7 means N/A
# 8 means "not reached"


###### N-grams ######
##:: this section creates tri-grams

library(NLP)

s <- "The quick brown fox jumps over the lazy dog"
## Split into words:
w <- strsplit(s, " ", fixed = TRUE)[[1L]]
## Word tri-grams:
ngrams(w, 3L)
## Word tri-grams pasted together:
vapply(ngrams(w, 3L), paste, "", collapse = " ")


##### grams permutation #####
##:: this section creates the permutations of the tri-grams
library(gtools)
perm3 <- permutations(length(w), 3, w)
perm3 <- apply(perm3, 1, paste, sep = " ", collapse = " ")



#################################
#################################
## get rid of "D" replication  ##
#################################
#################################

temp_indicator <- as.character(pisa12a_Recoded_full$diag_state)

D_rep_indicator<- NULL

for (i in 1:951480) {
  if (temp_indicator[i] == "NULL") {
    D_rep_indicator[i] <- F
  } else {
    if (temp_indicator[i] == temp_indicator[i+1]){
    D_rep_indicator[i] <- T
    } else {D_rep_indicator[i] <- F}
  }
}

D_rep_indicator[951481] <- F

