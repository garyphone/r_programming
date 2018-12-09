pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the 
        ## mean; either "sulfate" or "nitrate"
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        # load the all file names
        full_files <- list.files(directory, full.names = TRUE)
        
        # set an initial data frame
        data_ini <- data.frame()
        
        # set the specific files we want
        for (i in id) {
                data_ini <- rbind(data_ini, read.csv(full_files[i]))
        }
        
        # calculate the mean value
        mean(data_ini[[pollutant]], na.rm = TRUE)
}