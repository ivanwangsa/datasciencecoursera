rankall <- function(outcome, num = 'best') {
    ## Read outcome data
    outcome_data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    
    ## Check that state and outcome are valid
    states <- unique(outcome_data$State)
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    
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
    subsetted_outcome_data <- outcome_data[,c(2, 7, col_num)]
    subsetted_outcome_data <- subsetted_outcome_data[complete.cases(subsetted_outcome_data), ]
    rownames(subsetted_outcome_data) <- NULL
    sorted_outcome_data <- subsetted_outcome_data[order(subsetted_outcome_data[,3], subsetted_outcome_data[,1]),]
    splitted_outcome_data <- split(sorted_outcome_data[, 1], sorted_outcome_data[, 2])
    result <- sapply(splitted_outcome_data, function(df, rank){
        if(rank == 'best'){
            rank <- 1
        }
        if(rank == 'worst'){
            rank <- length(df)
        }
        df[rank]
    }, rank = num)
    data.frame(hospital = result, state = names(result))
}