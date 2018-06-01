######################################
#####  D and R further removeing #####
######################################

#############################################
##:: subset out the D terms in ActionSequence 

FurtherDeleteD <- function(x) {
  x <- unlist(strsplit(x, ", "))
  oldnum <- which(grepl("DF", x) |
                    grepl("DG", x) |
                    grepl("DH", x) |
                    grepl("DI", x))
  allD <- x[oldnum]
  rep_D_indicator <- NULL
  if (length(allD) > 1) {
    for (i in 1 : (length(allD)-1)) {
      if (allD[i] == allD[i+1]) {
        rep_D_indicator[i+1] <- T
      } else {rep_D_indicator[i+1] <- F}
    }
    rep_D_indicator[length(1)] <- F
    if (sum(rep_D_indicator) == 0) {
      out <- paste(x, collapse = ", ")
    } else {out <- paste(x[-oldnum[which(rep_D_indicator)]], collapse = ", ")}
  } else {out <- paste(x, collapse = ", ")}
  return(out)
}
###############################################
###############################################
ActionSequence <- apply(as.matrix(cleandat$ActionSequence), 1, FurtherDeleteD)
cleandat <- cbind(cleandat[, -10], ActionSequence)
################################################
################################################
DeleteRepR <- function(x) {
  x <- unlist(strsplit(x, ", "))
  rep_D_indicator <- NULL
  for(i in 1 : (length(x) - 1)) {
    if (x[i] == "RCCC" & x[i + 1] == x[i]) {
      rep_D_indicator[i] <- T
    } else {
      rep_D_indicator[i] <- F
    }
  }
  rep_D_indicator[length(x)] <- F
  return(rep_D_indicator)
}



################################################
##############################################
##:: grep the perticular D

whichD <- function (x, order = 1) {
  #: x is the highlevel sequence
  x <- unlist(strsplit(x, ", "))
  allD <- x[which(grepl("DF", x) |
                       grepl("DG", x) |
                       grepl("DH", x) |
                       grepl("DI", x))]
  if (length(allD) < order) {
    out <- "NULL"
  } else {out <- allD[order]}
  return(out)
}

firstD <- apply(as.matrix(cleandat$ActionSequence), 1, whichD, order = 1)
secondD <- apply(as.matrix(cleandat$ActionSequence), 1, whichD, order = 2)

###############################################





################################################
##:: has good diag state or not
gooddiag <- function(x){
  if(any(x == "DHFF") | any(x == "DFGF") | any(x == "DFFG")) {
    out <- 1
  } else {out <- 0}
  return(out)
}
DrawRightatFirst <- apply(cbind(firstD, secondD), 1, gooddiag)
DrawRightatFirst <- DrawRightatFirst[-wired_time_indicator]
################################################
cleandat <- cbind(cleandat, DrawRightatFirst)