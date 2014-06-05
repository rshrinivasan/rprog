corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        # Get list of filenames
        #filenames <- list.files("/Users/ramesh/Documents/Coursera/datasci/rprog/specdata")
        d <- paste("/Users/ramesh/Documents/Coursera/datasci/rprog/",directory, sep="")
        filenames <- list.files(d)
        #filenames
        
        # create two vectors to store values
        #sulf <- vector(mode = "numeric", length = 0)
        #nit <- vector(mod = "numeric", length = 0)
        # create vector to store correlation
        corr <- vector(mod = "numeric", length = 0)
        # loop through each file to see if complete cases greater than threshold
        for (f in filenames){
                pollutants <- read.csv(paste(d,"/", f, sep = ""), header = TRUE) 
                # check if number of completed cases is > threshold
                if (nrow(subset(pollutants,
                                !is.na(pollutants$sulfate)
                                & !is.na(pollutants$nitrate))) > threshold){
                        # calculate corr
                        corr <- append(corr, cor(pollutants$nitrate, pollutants$sulfate, 
                                                 use = "complete.obs"))                        
                }
                # remove pollutants dataframe
                rm(pollutants)
        }
        corr

}