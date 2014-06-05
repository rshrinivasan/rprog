pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        
        # define an empty vector to hold file names
        filenames <- character(length(id))
        # populate the vector
        # filenames are fixed width 3 characters e.g. 001.csv, 050.csv, 100.csv
        # sprintf will pad to three characters
        for(f in seq(length(id))){
                filenames[f] <- paste("/Users/ramesh/Documents/Coursera/datasci/rprog/",directory,"/",sprintf("%03d",id[f]), ".csv", sep = "") 
        }
        # uncomment for debugging
        #filenames
        
        # load each file in filenames into dataframe
        for(i in filenames){
                # create dataframe if it does not exist
                if(!exists ("pollutants")){
                        pollutants <- read.csv(i, header = TRUE)
                } else {
                        # create a temp dataframe and merge
                        temppollutants <- read.csv(i, header = TRUE)
                        # merge into pollutants
                        pollutants <- rbind(pollutants, temppollutants)
                        rm(temppollutants)
                }
        }
        # Uncomment for debugging
        #tail(pollutants)
        
        # calculate mean for pollutant
        mean_pollutant <- mean(pollutants[ ,pollutant], na.rm = TRUE)
        mean_pollutant
}