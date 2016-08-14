rankhospital <- function(state,outcome,rank = "best"){
  ## read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  fd <- as.data.frame(cbind(data[,2],
                            data[,7],
                            data[,11],
                            data[,17],
                            data[,23]),stringAsFactors=FALSE)
  colnames(fd) <- c("hospital","state","heart attack","heart failure" , "pneumonia")
  
  ##check that teh state and outcome are valid
  validStates <- fd[,"state"]
  validOutcomes <- c("heart attack","heart failure","pneumonia")
  if(!state %in% validStates){
    stop('state is not valid')
  }
  if(!outcome %in% validOutcomes){
    stop('outcome is not valid')
  }
  ## calculate
}