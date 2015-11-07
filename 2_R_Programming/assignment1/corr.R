corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    corr <- c()
    
    for(name in 1:332){
        path_to_file <- sprintf("./%s/%03d.csv", directory, name)
        dat <- read.csv(path_to_file)
        if (sum(complete.cases(dat)) >= threshold){
            dat <- dat[complete.cases(dat),]
            corr <- c(corr, cor(dat$sulfate, dat$nitrate))
        }
    }
    corr
}