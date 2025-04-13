best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  outcome_colnames <- list(
    "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )

  if (!(outcome %in% names(outcome_colnames))) {
    stop("invalid outcome")
  }

  if (!(state %in% data$State)) {
    stop("invalid state")
  }

  colname <- outcome_colnames[[outcome]]
  state_data <- data[data$State == state, ]
  state_data[, colname] <- suppressWarnings(as.numeric(state_data[, colname]))
  valid_data <- state_data[!is.na(state_data[, colname]), ]
  min_val <- min(valid_data[, colname])
  best_hospitals <- valid_data[valid_data[, colname] == min_val, "Hospital.Name"]
  
  return(sort(best_hospitals)[1])
}
