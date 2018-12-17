best <- function(state, outcome) {
        ## Read outcome data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        if ((state %in% dat$State) == 0) {
                stop(print("Invalid State Entry!"))
        }
        else if ((outcome %in% c("heart attack", "heart failure", "pneumonia")) == 0) {
                stop(print("Invalid Outcome Entry!"))
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
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
        min_result <- which(as.numeric(select_dat[, index]) == min(as.numeric(select_dat[, index]), na.rm = TRUE))
        names <- sort(select_dat[min_result, 2])
        return(names[1])
}