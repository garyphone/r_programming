rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if ((state %in% dat$State) == 0) {
                stop(print("Invalid State Entry!"))
        }
        else if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
                stop(print("Invalid Outcome Entry!"))
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        select_dat <- subset(dat, State == state)
        if (outcome == "heart attack") {
                index <- 11
        }
        else if (outcome == "heart failure") {
                index <- 17
        }
        else {
                index <- 23
        }
        select_dat[, index] <- as.numeric(select_dat[, index])
        order_dat <- select_dat[order(select_dat[, index], select_dat[, 2]), ]
        order_dat <- order_dat[(!is.na(order_dat[, index])), ]
        if (num == "best") {
                num <- 1
        }
        else if (num == "worst") {
                num <- nrow(order_dat)
        }
        return(order_dat[num, 2])
}