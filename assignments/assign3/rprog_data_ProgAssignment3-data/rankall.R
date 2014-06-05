rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outfile <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
        ## Check that outcome is valid
        out <- c("heart attack","heart failure","pneumonia")
        if (!any(out == outcome)){
                stop("invalid outcome")
        }
        outfile[ , 11] <- as.numeric(outfile[, 11])
        outfile[ , 17] <- as.numeric(outfile[, 17])
        outfile[ , 23] <- as.numeric(outfile[, 23])
        ## For each state, find the hospital of the given rank
        if (outcome == "heart attack"){
                subcols <- c("Hospital.Name","State", 
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" )
        } else if (outcome == "heart failure"){
                subcols <- c("Hospital.Name", "State",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" )
        } else {
                subcols <- c("Hospital.Name", "State",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" )
        }
        # subset to select only the outcome needed
        stateout <- subset(outfile, select = subcols)

        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        # sort the data frame first before assigning rank
        stateout <- stateout[order(stateout[ , 2], stateout[ , 1]), ]
        # rank with partitioning by state, this will rank each hospital in every
        #state
        res <- transform(stateout, rank = 
                                 ave(stateout[ , 3], stateout[, 2], 
                                     FUN = function(x) rank(x, ties.method = "first")))
        # determine the cutoff for the rank
        if (num == "best"){
                n = 1
        } else if (num == "worst") {
                n = 1
        } else if (num > nrow(res)){
                NA
        }
        else {
                n = num
        }
        
        # function return should be a dataframe with the first column named
        # hospital and the second column named state
        # first create a dataframe of unique states
        st <- cbind.data.frame(unique(res$State))
        # rename column to state
        names(st) <- "state"
        
        res <- res[res$rank == n, ]
        ret <- merge(st, res, by.x = "state", by.y = "State", 
                   all.x = TRUE, all.y = FALSE)[, c("Hospital.Name","state")]
        names(ret) <- c("hospital", "state")
        ret
}