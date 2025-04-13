rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Map outcomes to column names
  outcome_colnames <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  ## Validate outcome
  if (!(outcome %in% names(outcome_colnames))) {
    stop("invalid outcome")
  }

  ## Get the relevant column
  colname <- outcome_colnames[[outcome]]
  
  ## Convert data to numeric for the outcome column
  data[, colname] <- suppressWarnings(as.numeric(data[, colname]))
  
  ## Initialize a data frame for results
  result <- data.frame(hospital = character(0), state = character(0))
  
  ## Loop through each state and get the hospital with the specified rank
  for (state in unique(data$State)) {
    # Filter for the state and relevant columns
    state_data <- data[data$State == state, c("Hospital.Name", "State", colname)]
    state_data <- state_data[!is.na(state_data[, colname]), ]
    
    # Sort by outcome and hospital name
    ordered <- state_data[order(state_data[, colname], state_data$Hospital.Name), ]
    
    # Determine the rank
    if (num == "best") {
      rank <- 1
    } else if (num == "worst") {
      rank <- nrow(ordered)
    } else if (is.numeric(num)) {
      rank <- num
    } else {
      stop("invalid num")
    }
    
    # If the rank is within the number of hospitals, append the result
    if (rank <= nrow(ordered)) {
      result <- rbind(result, data.frame(hospital = ordered$Hospital.Name[rank], state = state))
    } else {
      result <- rbind(result, data.frame(hospital = NA, state = state))
    }
  }
  
  # Return the final data frame
  return(result)
}
