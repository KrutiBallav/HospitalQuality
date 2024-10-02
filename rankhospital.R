library(data.table)
rankhospital <- function(state, outcome, num = "best") {
  
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
  
  # Rename columns: remove the verbose prefix and convert all column names to lowercase for easier referencing
  setnames(out_dt, tolower(gsub("^Hospital 30-Day Death \\(Mortality\\) Rates from ", "", colnames(out_dt))))
  
  # Filter the dataset to include only rows where the state matches the chosen state
  out_dt <- out_dt[state == chosen_state]
  
  # Identify column indices that match "hospital name", "state", and the specified outcome
  col_indices <- grep(paste0("hospital name|state|^", outcome), colnames(out_dt))
  
  # Filter the dataset to include only the relevant columns based on the identified indices
  out_dt <- out_dt[, .SD, .SDcols = col_indices]
  
  # Convert the selected outcome column to a numeric datatype for sorting and comparison operations
  out_dt[, (outcome) := as.numeric(get(outcome))]
  
  # Remove rows with missing values in the outcome column to ensure accurate sorting and selection
  out_dt <- out_dt[complete.cases(out_dt),]
  
  # Sort the dataset by outcome values in ascending order. If there are ties, sort alphabetically by hospital name
  out_dt <- out_dt[order(get(outcome), `hospital name`)]
  
  # Create a new table with specific columns: hospital name, state, rate (outcome), and rank (index number)
  out_dt <- out_dt[, .(`hospital name` = `hospital name`, state = state, rate = get(outcome), Rank = .I)]
  
  # If the user requested the best hospital, return the first hospital in the sorted dataset
  if (num == "best") {
    return(out_dt[1, `hospital name`])
  }
  
  # If the user requested the worst hospital, return the last hospital in the sorted dataset
  if (num == "worst") {
    return(out_dt[.N, `hospital name`])  # .N gives the number of rows in the table
  }
  
  # If a numeric rank is specified, return the hospital at that rank (if it exists)
  return(out_dt[num, `hospital name`])
}