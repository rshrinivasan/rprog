best <- function(state, outcome) {
        ## Read outcome data"
        outfile <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")        
        ## Check that state and outcome are valid
        # checking if valid outcome
        out <- c("heart attack","heart failure","pneumonia")
        if (!any(out == outcome)){
                stop("invalid outcome")
        }
        # checking if valid state
        if (!any(outfile$State == state)){
                stop("invalid state")
        }
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        # subset data with state and outcome desired as filters
        if (outcome == "heart attack"){
                subcols <- c("Hospital.Name", 
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" )
        } else if (outcome == "heart failure"){
                subcols <- c("Hospital.Name", 
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" )
        } else {
                subcols <- c("Hospital.Name", 
                             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
        }
                
        stateout <- subset(outfile, State == state, select = subcols)
        # convert the outcome column from string to numeric
        stateout[ , 2] <- as.numeric(stateout[ , 2])
        # get the vector of hospital names with minimum mortality rates
          res <- rank(stateout[, 2], na.last = NA, ties.method = "first")
        # sort by alpha in case of multiple hospitals returned 
        # and return first row
        # sort(res)[1]
}

