library(data.table)
rankall <- function(outcome, num = "best") {
  
  # Read outcome data from the CSV file into a data.table object
  out_dt <- data.table::fread('outcome-of-care-measures.csv')
  
  # Convert the outcome parameter to lowercase for case-insensitive matching
  outcome <- tolower(outcome)
  
  # Check if the provided outcome is one of the accepted health conditions
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')  # Stop execution if the outcome is invalid
  }
  
  # Rename columns to be less verbose and lowercase:
  # Remove the prefix "Hospital 30-Day Death (Mortality) Rates from "
  setnames(out_dt, tolower(gsub("^Hospital 30-Day Death \\(Mortality\\) Rates from ", "", colnames(out_dt))))
  
  # Identify the indices of columns that are relevant to "hospital name", "state", and the specified outcome
  col_indices <- grep(paste0("hospital name|state|^", outcome), colnames(out_dt))
  
  # Filter the dataset to include only the relevant columns based on the identified indices
  out_dt <- out_dt[, .SD, .SDcols = col_indices]
  
  # Convert the selected outcome column to a numeric datatype
  # This step is necessary for accurate sorting and comparison
  out_dt[, (outcome) := as.numeric(get(outcome))]
  
  # Handle the `num` argument to return results based on the rank specified by the user:
  # 1. If `num` is "best", return the hospital with the best (lowest) outcome value for each state.
  if (num == "best") {
    return(out_dt[
      order(state, get(outcome), `hospital name`),  # Sort by state, outcome value, and hospital name
      .(hospital = head(`hospital name`, 1)),        # Select the first (best) hospital name for each state
      by = state                                    # Group by state
    ])
  }
  
  # 2. If `num` is "worst", return the hospital with the worst (highest) outcome value for each state.
  if (num == "worst") {
    return(out_dt[
      order(state, get(outcome), `hospital name`),  # Sort by state, outcome value, and hospital name
      .(hospital = tail(`hospital name`, 1)),        # Select the last (worst) hospital name for each state
      by = state                                    # Group by state
    ])
  }
  
  # 3. If `num` is a specific rank number, return the hospital with that rank for each state.
  return(out_dt[
    order(state, get(outcome), `hospital name`),     # Sort by state, outcome value, and hospital name
    head(.SD, num),                                  # Select the top `num` rows for each state
    by = state,                                      # Group by state
    .SDcols = c("hospital name")                     # Include only the hospital name in the result
  ])
}