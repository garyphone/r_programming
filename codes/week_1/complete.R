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
        
        # load the all file names
        full_files <- list.files(directory, full.names = TRUE)
        
        # set an initial data frame
        data_ini <- data.frame()
        
        # calculate the complete cases
        for (i in id) {
                monitor_num <- read.csv(full_files[i])
                nobs_num <- sum(complete.cases(monitor_num))
                data_temp <- data.frame(i, nobs_num)
                data_ini <- rbind(data_ini, data_temp)
        }
        
        # set the header for the data
        colnames(data_ini) <- c("id", "nobs")
        data_ini
}