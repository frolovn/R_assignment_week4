rankhospital <- function(state, outcome, num = "best") {

  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  df <- file[,c(2,7,11,17,23)]
  df [df == "Not Available"] <- NA

  colnames(df) <- c("Hospital", "St", "heart attack", "heart failure", "pneumonia")
  Statedf <- df[df$St == state, ] ##select state

  findf <- Statedf[, c("Hospital", outcome)] ##make new df with 2 columns required

  findf[,2] <- as.numeric(findf[, 2])
  
  ord <- findf[order(findf[,2], findf[,1]), ]
  
  
  ord$rank <- 1:nrow(ord)
  
 
  if(num == "best") {
    num = 1
  }
  else if(num == "worst") {
    num = sum(complete.cases(ord))
  }
  
  ind <- ord[num,1] 

  return(ind)

  }