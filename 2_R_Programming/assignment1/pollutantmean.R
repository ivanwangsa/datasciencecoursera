pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    total <- 0.0
    count <- 0
    
    for(name in id){
        path_to_file <- sprintf("./%s/%03d.csv", directory, name)
        dat <- read.csv(path_to_file)
        dat <- dat[[pollutant]]
        dat <- dat[complete.cases(dat)]
        total = total + sum(dat)
        count = count + length(dat)
    }
    total/count
}
