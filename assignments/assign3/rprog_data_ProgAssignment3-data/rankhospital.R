rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
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
        # coerce outcome columns to numeric
        outfile[ , 11] <- as.numeric(outfile[, 11])
        outfile[ , 17] <- as.numeric(outfile[, 17])
        outfile[ , 23] <- as.numeric(outfile[, 23])
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
        # remove NAs 
        stateout <- stateout[complete.cases(stateout), ]
        # sort dataframe by hospital name asc
        res <- stateout[order(stateout[1]), ]
        res$ranks <- rank(res[, 2], na.last = NA, ties.method = "first")
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if (num == "best"){
                res$Hospital.Name[res$ranks == 1]
        } else if (num == "worst") {
                res$Hospital.Name[res$ranks == nrow(res)]
        } else if (num > nrow(res)){
                NA
        }
          else {res$Hospital.Name[res$ranks == num]
        }
}