###########################################
###########################################
###########                    ############
########### Catching Functions ############
###########                    ############
###########################################
###########################################

#############################
###::       NGrams      ::###
#############################

#############################################
getgram <- function(x, n = 2) {
  #:: x should be a string "XXXX"
  #:: n represents the gramme size
  x <- unlist(strsplit(x, ""))
  base <- x
  temp <- x
  for(i in 1 : (n - 1) ){
    temp <- dplyr::lead(temp)
    base <- cbind(base, temp)
  }
  base <- na.omit(base)
  out <- apply(base, 1, paste, collapse = "")
  #out <- data.frame(Grams = names(table(out)),
                    #Counts = as.vector(table(out)))
  return(out)
}
############################################
gram2table <- function(x) {
  #:: x is obtained by the getgram fucntion
  l <- length(x)
  base <- x[[1]]
  for (i in 2 : l) {
    base <- dplyr::full_join(base, x[[i]], by = "Grams")
  }
  Names <- base[, 1]
  out <- t(base)[-1, ]
  colnames(out) <- Names
  rownames(out) <- NULL
  return(as.data.frame(out))
}

###############################################
getpermn <- function(x, n) {
  temp <- x
  for(i in 1 : (n - 1)) {
    temp <- apply(expand.grid(temp, x), 1, paste, collapse = "")
  }
  return(temp)
}


##############################################
getpermnbyN <- function(x, n) {
  #: x must be the unique case
  l <- length(x)
  if (n <= l) {
    combos <- combn(x, n)
    out <- apply(combos, 2, getpermn, n = n)
  } else {print("Error: n need to be <= length of x")}
  out <- unique(as.vector(out))
  return(out)
}

#############################################
getGramTable <- function(x, perm) {
  #: x should be obtained from getgram
  #: perm is a vector obtained by getpermbyN
  table <- t(as.matrix(rep(0, length(perm))))
  colnames(table) <- perm
  for (i in 1 : length(x)) {
    table[which(x[i] == perm)] <- table[which(x[i] == perm)] + 1
  }
  return(table)
}





###########################################
###:: Higher-level Action Sequences   ::###
###########################################

highlevel <- function(x) {
  #:: split the sequence into actions
  temp <- unlist(strsplit(as.character(x), ", "))
  #:: subset out the first letter of each action
  temp <- substr(temp, 1, 1)
  #:: get the highlevel sequence without "START" and "END"
  sequence <- paste(temp, collapse = "")
  #sequence <- paste(temp[-c(1, length(temp))], collapse = "")
  
  #:: counting the number of each higher level action
  # Rcounts <- sum(temp == "R")
  # Acounts <- sum(temp == "A")
  # Dcounts <- sum(temp == "D")
  return(sequence)
  # return(list("R" = Rcounts, "A" = Acounts, "D" = Dcounts))
}

highlevel.check <- function(x) {
  #:: split the sequence into actions
  temp <- unlist(strsplit(as.character(x), ", "))
  #:: subset out the first letter of each action
  temp <- substr(temp, 1, 1)
  #:: get the highlevel sequence without "START" and "END"
  sequence <- paste(temp[c(1,length(temp))], collapse = "")
  
  #:: counting the number of each higher level action
  # Rcounts <- sum(temp == "R")
  # Acounts <- sum(temp == "A")
  # Dcounts <- sum(temp == "D")
  return(sequence)
  # return(list("R" = Rcounts, "A" = Acounts, "D" = Dcounts))
}


###########################################
###:: obtain VOTAT in Sequence Action ::###
###########################################
vary_one <- function(x){
  temp <- unlist(strsplit(x[1], "")) == unlist(strsplit(x[2], ""))
  counts <- sum(temp)
  if(counts == 2) {
    if (temp[1] == F) {Vposition <- "T"}
    if (temp[2] == F) {Vposition <- "C"}
    if (temp[3] == F) {Vposition <- "B"}
  } else {Vposition <- "N"}
  return(Vposition)
}

getVOTAT <- function(x) {
  temp <- unlist(strsplit(x, ", "))
  #:: get the action with type "APPLY"
  temp <- temp[which(substr(temp, 1, 1) == "A" | substr(temp, 1, 1) == "R")]
  #:: obtain the last 3 letters of this action
  if (length(temp) == 0) {
    temp <- "N"
  } else if (length(temp) == 1) {
    temp <- "N"
  } else {
    temp <- substr(temp, 2, 4)
    tempMatrix <- cbind(temp[1 : (length(temp) - 1)], temp[2 : length(temp)])
    temp <- paste(apply(tempMatrix, 1, vary_one), collapse = "")
  }
  return(temp)
}


#############################
###:: obtain Hesitation ::###
#############################
hesitation 