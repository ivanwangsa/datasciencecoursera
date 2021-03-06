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
    
    nobs <- rep(NA, length(id))
    
    for(i in seq_along(id)){
        path_to_file <- sprintf("./%s/%03d.csv", directory, id[i])
        dat <- read.csv(path_to_file)
        nobs[i] <- sum(complete.cases(dat))
    }
    
    data.frame(id = id, nobs = nobs)
}
