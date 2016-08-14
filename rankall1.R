rankall <- function(outcome,num = "best"){
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  # change data type from character to numeric
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  
  outcomeIndex <- function(o){switch(o,"heart attack" = 11,"heart failure" = 17,"pneumonia"  = 23)}
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% data$State) {
    stop("not a valid state")
  } else if(!outcome %in% validOutcomes) {
    stop("not a valid outcome")
  }
  ##get states by alpha
  state_arr <- sort(unique(data$State))
  state_arr_len <- length(state_arr)
  for(i in 1:state_arr_len){
      state_specific_data <- data[data[,7] == state_arr[i],]
      if(outcome == "heart attack")
        {colInt <- 11}
      else if(outcome == "heart failure")
      {colInt <- 17}
      else {
        colInt <- 23
      }
  }
}