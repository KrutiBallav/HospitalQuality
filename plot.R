outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character") #outcome data into R
head(outcome)   # read first few rows
nrow(outcome)   # no of rows
ncol(outcome)   # no of columns
# making histogram of the data on 30 day death rates from heart attack (column 11 in the outcome dataset)
outcome[ ,11] <- as.numeric(outcome[ ,11])
hist (outcome[ ,11],xlab="Heart Attack Death rates",main="30-day mortality rates for Heart Attack")