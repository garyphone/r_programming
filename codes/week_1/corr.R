corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observation (on all
        ## variables) required to compute the correlation between 
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the results!
        
        # load the all file names
        full_files <- list.files(directory, full.names = TRUE)
        
        # set an initial vector
        data_ini <- c()
        
        # calculate the correlation of sulfate and nitrate
        for (i in 1:length(full_files)) {
                monitor_num <- read.csv(full_files[i])
                # calculate sulfate and nitrate for monitor locations where the number
                # of completely observed cases, regardless of NA
                # $ is used to select the key, either sulfate or nitrate
                summa <- sum((!is.na(monitor_num$sulfate)) & (!is.na(monitor_num$nitrate)))
                if (summa > threshold) {
                        monitor_temp <- monitor_num[which(!is.na(monitor_num$sulfate)), ]
                        # the temporary monitor should be fixed with same dimension
                        main_part <- monitor_temp[which(!is.na(monitor_temp$nitrate)), ]
                        data_ini <- c(data_ini, cor(main_part$sulfate, main_part$nitrate))
                }
                
        }
        data_ini
}