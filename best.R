library(data.table)
best <- function(state, outcome) {
  
  # Read outcome data from CSV file into a data.table object
  out_dt <- data.table::fread('outcome-of-care-measures.csv')
  
  # Convert the provided outcome parameter to lowercase for case-insensitive matching
  outcome <- tolower(outcome)
  
  # Assign the state parameter to a new variable `chosen_state` for clarity
  chosen_state <- state
  
  # Check if the provided state is valid by comparing it with the unique states in the dataset
  if (!chosen_state %in% unique(out_dt[["State"]])) {
    stop('invalid state')  # Stop execution if the state is invalid
  }
  
  # Check if the provided outcome is one of the accepted outcomes
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')  # Stop execution if the outcome is invalid
  }
  
  # Rename columns: remove the verbose prefix and convert all column names to lowercase
  setnames(out_dt, tolower(gsub("^Hospital 30-Day Death \\(Mortality\\) Rates from ", "", colnames(out_dt))))
  
  # Filter the dataset to include only rows where the state matches the chosen state
  out_dt <- out_dt[state == chosen_state]
  
  # Get column indices that match the hospital name, state, and the specified outcome
  col_indices <- grep(paste0("hospital name|state|^", outcome), colnames(out_dt))
  
  # Filter the dataset to include only the relevant columns based on the identified indices
  out_dt <- out_dt[, .SD, .SDcols = col_indices]
  
  # Convert the outcome column to a numeric datatype (this is necessary for comparison operations)
  out_dt[, (outcome) := as.numeric(get(outcome))]
  
  # Remove rows with missing values in the outcome column to ensure accurate sorting and selection
  out_dt <- out_dt[complete.cases(out_dt),]
  
  # Sort the dataset by outcome values (ascending order) and by hospital name (alphabetically)
  out_dt <- out_dt[order(get(outcome), `hospital name`)]
  
  # Return the hospital name with the best (lowest) outcome value
  return(out_dt[, "hospital name"][1])
}