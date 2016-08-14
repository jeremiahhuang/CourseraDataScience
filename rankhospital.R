rankhospital <- function(state,outcome,num = "best"){
  # read the data file
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  # change data type from character to numeric
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  
  outcomeIndex <- function(o){switch(o,"heart attack" = 11,"heart failure" = 17,"pneumonia"  = 23)}
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  numStates <- function(n){switch(n,""}
  ## Check that state and outcome are valid
  if (!state %in% data$State) {
    stop("not a valid state")
  } else if(!outcome %in% validOutcomes) {
    stop("not a valid outcome")
  }
  ## calculate
  dataOfState <- data[data[,7] == state,]
  dataOfOutcome <- dataOfState[,  outcomeIndex(outcome)]
  return(dim(dataOfState[!is.na(dataOfOutcome),])[1])
  
}