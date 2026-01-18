## helper function to check if all columns are rational arithmetic in the sense of rcdd
rat_cols <- function(data)
{
    ret <- TRUE
    if(is.matrix(data)) data <- as.data.frame(data)
    vals <- sapply(data,is.character) #lets first check if it is a character
    if(sum(vals) < ncol(data)) return(FALSE) 
    tmp <- lapply(data[,vals,drop=FALSE],ratregex) #now check if it is a ratio i.e. - integer/integer
    if(any(sapply(tmp,sum)!=nrow(data))) ret <- FALSE 
    return(ret)
}

## regexp for rational arithmetic style; checks if a vector has only entries that are either +-integer/integer or +-integer   
ratregex <- function(x) {
    return(grepl("-?[[:digit:]]+/[[:digit:]]+",x) | grepl("^-?[0-9]+$",x))
    }
