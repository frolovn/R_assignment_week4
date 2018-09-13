rankall  <- function(outcome, num = "best") {

  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  FullDataSet <- file[,c(2,7,11,17,23)]
  FullDataSet [FullDataSet == "Not Available"] <- NA
  colnames(FullDataSet) <- c("Hospital", "State", "heart attack", "heart failure", "pneumonia")
  ## FullDataSet contains full data set
  
  allStates <- unique(FullDataSet[,2])
  allStates <-allStates[order(allStates)]
  i <- NULL
  HospitalStateAll <- NULL
  
 
    for (i in 1:length(allStates)) { 
      
    StateDataSet <- FullDataSet[FullDataSet$State == allStates[i], ]## 1 changed to i
   
    StateOutcomeSet <- StateDataSet[, c("Hospital", "State", outcome)] ##make new State set with 3 columns required
    StateOutcomeSet[,3] <- as.numeric(StateOutcomeSet[, 3])
    OrderedStateOutcomeSet <- StateOutcomeSet[order(StateOutcomeSet[,3], StateOutcomeSet[,1]), ]##sort the list
    OrderedStateOutcomeSetSum <- NULL
  
    if(num == "best") {numN <- 1}
    else if(num == "worst") {numN <- sum(complete.cases(StateOutcomeSet[,3]))}
    else {numN <- num}
    HospitalState <- OrderedStateOutcomeSet[numN,c(1,2)]
    HospitalStateAll <- rbind(HospitalStateAll, HospitalState)
   
    
    }
  HospitalStateAll[,2] <- allStates
  return(HospitalStateAll)
  }
    
    

  
  