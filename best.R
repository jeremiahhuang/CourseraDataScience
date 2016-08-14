
best <- function(state, outcome) {
  # read the data file
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
  ##should exit if the state and outcomes are invalid
  ##state is col 7 in the data set
  outcomeCol <- outcomeIndex(outcome)
  stateData <- data[data[,7] == state,]
  outcomeForState <- stateData[,outcomeCol]
  lowestMorality <- min(outcomeForState,na.rm=TRUE)
  lm_index <- which(outcomeForState  == lowestMorality)
  hopsitalName <-  stateData[lm_index,2]
  ## Return hospital name in that state with lowest 30-day death
  return(hopsitalName)
  
}
