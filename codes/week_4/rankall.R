rankall <- function(outcome, num = "best") {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that outcome are valid
        if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
                stop(print("Invalid Outcome Entry!"))
        }

        if (outcome == "heart attack") {
                index <- 11
        }
        else if (outcome == "heart failure") {
                index <- 17
        }
        else {
                index <- 23
        }
        dat[, index] <- as.numeric(dat[, index])
        select_dat <- dat[!is.na(dat[, index]), ]
        
        ## For each state, find the hospital of the given rank
        
        split_dat <- split(select_dat, select_dat$State)
        result <- lapply(split_dat, function(x, num) {
                x <- x[order(x[, index], x$Hospital.Name), ]
                if (class(num) == "character") {
                        if (num == "best") {
                                return(x$Hospital.Name[1])
                        }
                        else if (num == "worst") {
                                return(x$Hospital.Name[nrow(x)])
                        }
                }
                else {
                        return(x$Hospital.Name[num])
                }
        }, num
        )
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        return(data.frame(hosptial = unlist(result), state = names(result)))
}