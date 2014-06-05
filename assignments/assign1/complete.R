complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        
        # define an empty vector to hold file names
        filenames <- character(length(id))
        # populate the vector
        # filenames are fixed width 3 characters e.g. 001.csv, 050.csv, 100.csv
        # sprintf will pad to three characters
        for(f in seq(length(id))){
        # load file in dataframe
                pollutants <- read.csv(paste("/Users/ramesh/Documents/Coursera/datasci/rprog/",directory,"/",sprintf("%03d",id[f]), ".csv", sep = ""),  header = TRUE)
        # create empty dataframe to store counts with columns id and nobs
        # if it does not exist
                if(!exists ("counts")){
                        counts <- data.frame(id = numeric(0), nobs = numeric(0))
                }
        # for each file insert id and total rows without NAs
        # subset pollutants data frame to keep rows with 
        # nonNA in both sulfate and nitrate columns
        # to use rbind the second data frame should have the same structure
                counts <- rbind(counts, data.frame(id = id[f], 
                                                   nobs = nrow(subset(pollutants,
                                                               !is.na(pollutants$sulfate)
                                                               & !is.na(pollutants$nitrate)))))
        }
        counts
}