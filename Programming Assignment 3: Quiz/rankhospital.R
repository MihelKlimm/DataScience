rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  # Map outcomes to column names
  outcome_colnames <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )

  # Validate outcome
  if (!(outcome %in% names(outcome_colnames))) {
    stop("invalid outcome")
  }

  # Validate state
  if (!(state %in% data$State)) {
    stop("invalid state")
  }

  # Get the relevant column
  colname <- outcome_colnames[[outcome]]

  # Filter for state and required columns
  state_data <- data[data$State == state, c("Hospital.Name", "State", colname)]
  state_data[, colname] <- suppressWarnings(as.numeric(state_data[, colname]))
  state_data <- state_data[!is.na(state_data[, colname]), ]

  # Sort by outcome and hospital name
  ordered <- state_data[order(state_data[, colname], state_data$Hospital.Name), ]

  # Determine rank
  if (num == "best") {
    rank <- 1
  } else if (num == "worst") {
    rank <- nrow(ordered)
  } else if (is.numeric(num)) {
    rank <- num
  } else {
    stop("invalid num")
  }

  # Return hospital name or NA
  if (rank > nrow(ordered)) {
    return(NA)
  } else {
    return(ordered$Hospital.Name[rank])
  }
}
