best <- function(state, outcome) {

  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  df <- file[,c(2,7,11,17,23)]
  df [df == "Not Available"] <- NA

  colnames(df) <- c("Hospital", "St", "heart attack", "heart failure", "pneumonia")
  Statedf <- df[df$St == state, ] ##select state

  findf <- Statedf[, c("Hospital", outcome)] ##make new df with 2 columns required

  findf[,2] <- as.numeric(findf[, 2])
  
  ind <- findf[which.min(findf[,2]),1] ##select the name based on the min value

  return(ind)

  }