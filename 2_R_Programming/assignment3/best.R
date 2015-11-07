best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that state and outcome are valid
    states <- unique(outcome_data$State)
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    
    if (sum(states == state) == 0) {
        stop('invalid state')
    }
    
    if (sum(outcomes == outcome) == 0){
        stop('invalid outcome')
    }
    
    ## Return hospital name in that state with the lowest 30-day death
    ## rate
    
    col_num <- 0
    if (outcome == 'heart attack'){
        col_num <- 11
    }
    else if (outcome == 'heart failure'){
        col_num <- 17
    }
    else if (outcome == 'pneumonia'){
        col_num <- 23
    }
    outcome_data[, col_num] <- as.numeric(outcome_data[, col_num])
    subsetted_outcome_data <- subset(outcome_data, outcome_data$State == state)
    subsetted_outcome_data <- subsetted_outcome_data[complete.cases(subsetted_outcome_data), c(2, col_num)]
    rownames(subsetted_outcome_data) <- NULL
    sorted_outcome_data <- subsetted_outcome_data[order(subsetted_outcome_data[,2], subsetted_outcome_data[,1]),]
    sorted_outcome_data[1,1]
}